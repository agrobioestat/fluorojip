#' Read a FluorPen Excel export
#'
#' Reads an OJIP export from the FluorPen software (Photon Systems
#' Instruments) saved as an Excel workbook: one measurement per column, the
#' first column holding the time grid (us), header rows `index`/`time`/`id`,
#' the fluorescence trace, and footer rows starting at `Fm` with the
#' parameters calculated by the FluorPen software.
#'
#' @param file Path to the `.xlsx` (or `.xls`) workbook.
#' @param sheet Sheet name or index. Defaults to the first sheet.
#' @param repair_decimals Logical. Some workbooks are produced by opening the
#'   FluorPen text export in a spreadsheet configured with a locale in which
#'   `"."` is the thousands separator; values such as `1.017` are then stored
#'   as `1017`. When `TRUE` (default), footer values of ratio-type parameters
#'   that contain no decimal mark and are >= 1000 are divided by 1000 (the
#'   FluorPen export uses three decimals). The number of repaired cells is
#'   reported in a message and recorded in `summary_repaired`.
#' @return A list with `sample_id`, `measurement_time`, `protocol_id`,
#'   `times_us`, `times_ms`, `mat` (one row per measurement, raw signal
#'   including the background point), `summary_raw` (footer as text),
#'   `summary_numeric` (footer converted to numbers, after optional repair),
#'   and `summary_repaired` (logical matrix of repaired cells).
#' @details The function stops if the `index` header row, the `Fm` footer row,
#'   or a numeric time grid cannot be found. Typical workflow:
#'   `read_fluorpen_xlsx() -> fluorpen_to_ojip() -> calc_fluorojip()`.
#' @references Photon Systems Instruments (2023). FluorPen and Monitoring Pen
#'   Manual, version 2023/07. PSI, Drasov, Czech Republic.
#' @importFrom readxl read_excel
#' @examples
#' xlsx <- system.file("extdata", "FluorPen_test.xlsx", package = "fluorojip")
#' if (nzchar(xlsx)) {
#'   raw <- read_fluorpen_xlsx(xlsx)
#'   dim(raw$mat)
#'   head(raw$times_us)
#' }
#' @export
read_fluorpen_xlsx <- function(file, sheet = 1, repair_decimals = TRUE) {
  x <- suppressMessages(readxl::read_excel(file, sheet = sheet, col_names = FALSE, .name_repair = "minimal"))
  x <- as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)

  first_col <- trimws(as.character(x[[1]]))
  header_row <- which(grepl("^index", first_col, ignore.case = TRUE))[1]
  if (is.na(header_row)) {
    stop("Could not find the FluorPen header row starting with 'index'.", call. = FALSE)
  }
  time_row <- header_row + 1L
  id_row <- header_row + 2L
  footer_row <- which(first_col == "Fm")[1]
  if (is.na(footer_row)) {
    stop("Could not find the FluorPen footer summary row 'Fm'.", call. = FALSE)
  }
  desc_row <- which(first_col == "description")[1]
  if (is.na(desc_row)) desc_row <- nrow(x) + 1L

  header_vals <- as.character(unlist(x[header_row, -1, drop = TRUE]))
  measure_cols <- which(!is.na(header_vals) & nzchar(trimws(header_vals))) + 1L
  if (length(measure_cols) == 0) {
    stop("Could not find any FluorPen measurement columns.", call. = FALSE)
  }

  sample_id <- make.unique(trimws(as.character(unlist(x[header_row, measure_cols, drop = TRUE]))))
  measurement_time <- trimws(as.character(unlist(x[time_row, measure_cols, drop = TRUE])))
  protocol_id <- trimws(as.character(unlist(x[id_row, measure_cols, drop = TRUE])))

  trace_rows <- seq.int(id_row + 1L, footer_row - 1L)
  times_us <- suppressWarnings(as.numeric(x[trace_rows, 1, drop = TRUE]))
  keep_rows <- is.finite(times_us)
  if (!any(keep_rows)) {
    stop("Could not extract a numeric time grid from the FluorPen file.", call. = FALSE)
  }
  times_us <- times_us[keep_rows]
  raw_mat <- as.matrix(x[trace_rows[keep_rows], measure_cols, drop = FALSE])
  raw_mat <- suppressWarnings(apply(raw_mat, 2, as.numeric))
  if (is.null(dim(raw_mat))) raw_mat <- matrix(raw_mat, ncol = length(measure_cols))
  mat <- t(raw_mat)
  rownames(mat) <- sample_id
  colnames(mat) <- times_us

  footer_rows <- seq.int(footer_row, desc_row - 1L)
  footer_labels <- trimws(as.character(x[footer_rows, 1, drop = TRUE]))
  footer_vals <- as.data.frame(x[footer_rows, measure_cols, drop = FALSE], stringsAsFactors = FALSE, check.names = FALSE)
  summary_raw <- as.data.frame(t(footer_vals), stringsAsFactors = FALSE, check.names = FALSE)
  names(summary_raw) <- footer_labels
  rownames(summary_raw) <- NULL
  summary_raw$sample_id <- sample_id
  summary_raw$measurement_time <- measurement_time
  summary_raw$protocol_id <- protocol_id

  ratio_params <- c("Vj", "Vi", "Fm/Fo", "Fv/Fo", "Fv/Fm", "Mo", "Sm", "Ss", "N",
                    "Phi_Po", "Psi_o", "Phi_Eo", "Phi_Do", "Phi_Pav", "Pi_Abs",
                    "ABS/RC", "TRo/RC", "ETo/RC", "DIo/RC")
  value_cols <- setdiff(names(summary_raw), c("sample_id", "measurement_time", "protocol_id"))
  summary_numeric <- summary_raw
  repaired <- matrix(FALSE, nrow = nrow(summary_raw), ncol = length(value_cols),
                     dimnames = list(sample_id, value_cols))
  for (nm in value_cols) {
    txt <- trimws(as.character(summary_raw[[nm]]))
    val <- suppressWarnings(as.numeric(txt))
    if (repair_decimals && nm %in% ratio_params) {
      fix <- is.finite(val) & !grepl("[.,]", txt) & abs(val) >= 1000
      val[fix] <- val[fix] / 1000
      repaired[, nm] <- fix
    }
    summary_numeric[[nm]] <- val
  }
  if (repair_decimals && any(repaired)) {
    message(sprintf(
      "Repaired %d FluorPen summary value(s) that had lost their decimal mark (divided by 1000).",
      sum(repaired)
    ))
  }

  list(
    sample_id = sample_id,
    measurement_time = measurement_time,
    protocol_id = protocol_id,
    times_us = times_us,
    times_ms = times_us / 1000,
    mat = mat,
    summary_raw = summary_raw,
    summary_numeric = summary_numeric,
    summary_repaired = repaired
  )
}

#' Convert FluorPen traces to an OJIP summary table
#'
#' Applies [trace_to_ojip()] with the `"fluorpen"` preset: the first exported
#' point is treated as the background reading and subtracted, Fo is the
#' nominal 50 us point (exported sample labelled 41 us), the early O-J point
#' is 300 us, J is 2 ms, I is 30 ms, and Fm is the trace maximum. Measurement
#' time, protocol, and light settings stored in the workbook are appended.
#'
#' @param x A list returned by [read_fluorpen_xlsx()].
#' @param ... Further arguments passed to [trace_to_ojip()] (for example
#'   `preset`, `background`, or `tolerance_ms`).
#' @return The data frame returned by [trace_to_ojip()], plus
#'   `measurement_time`, `protocol_id`, and any light-setting columns found in
#'   the footer.
#' @examples
#' xlsx <- system.file("extdata", "FluorPen_test.xlsx", package = "fluorojip")
#' if (nzchar(xlsx)) {
#'   ojip <- fluorpen_to_ojip(read_fluorpen_xlsx(xlsx))
#'   head(ojip[, c("sample_id", "fo", "f300us", "j", "i", "fm", "qc_flags")])
#' }
#' @export
fluorpen_to_ojip <- function(x, ...) {
  args <- list(...)
  if (is.null(args$preset)) args$preset <- "fluorpen"
  if (is.null(args$time_unit)) args$time_unit <- "us"
  res <- do.call(trace_to_ojip, c(list(times = x$times_us, mat = x$mat, sample_id = x$sample_id), args))
  ext <- attr(res, "extraction")
  res$measurement_time <- x$measurement_time
  res$protocol_id <- x$protocol_id
  light_cols <- grep("Intensity|Percent|Wavelength", names(x$summary_raw), value = TRUE)
  for (nm in light_cols) res[[nm]] <- x$summary_raw[[nm]]
  attr(res, "extraction") <- ext
  res
}

#' Calculate JIP-test parameters from a FluorPen workbook
#'
#' Convenience wrapper:
#' `read_fluorpen_xlsx() -> fluorpen_to_ojip() -> calc_fluorojip()`.
#'
#' @param file Path to the FluorPen workbook.
#' @param sheet Sheet name or index. Defaults to the first sheet.
#' @param ... Further arguments passed to [trace_to_ojip()].
#' @return The data frame returned by [calc_fluorojip()].
#' @examples
#' xlsx <- system.file("extdata", "FluorPen_test.xlsx", package = "fluorojip")
#' if (nzchar(xlsx)) {
#'   res <- calc_fluorojip_fluorpen(xlsx)
#'   head(res[, c("sample_id", "Fv_Fm", "PI_abs", "qc_flags")])
#' }
#' @export
calc_fluorojip_fluorpen <- function(file, sheet = 1, ...) {
  raw <- read_fluorpen_xlsx(file, sheet = sheet)
  calc_fluorojip(fluorpen_to_ojip(raw, ...))
}
