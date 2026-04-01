#' Read a FluorPen Excel export
#'
#' Reads a FluorPen `.xlsx` workbook exported in a wide vendor layout, where
#' columns represent measurements, the first column stores the OJIP time grid,
#' and footer rows contain vendor-calculated JIP-test parameters.
#'
#' @param file Path to the FluorPen `.xlsx` file.
#' @param sheet Sheet name or numeric index. Defaults to the first sheet.
#' @return A list with metadata, the raw trace matrix, and the vendor summary:
#'   \itemize{
#'     \item `sample_id` measurement identifiers taken from the header row
#'     \item `measurement_time` acquisition timestamps
#'     \item `protocol_id` protocol labels such as `OJIP`
#'     \item `times_us` raw time grid in microseconds
#'     \item `times_ms` raw time grid converted to milliseconds
#'     \item `mat` numeric matrix with one row per sample and one column per time
#'     \item `summary_raw` footer summary as imported from the workbook
#'     \item `summary_numeric` footer summary converted to numeric values where possible
#'   }
#' @details
#' This function reads a supported FluorPen workbook and prepares its trace and
#' footer summary data for downstream OJIP conversion, validation, and
#' comparison workflows.
#'
#' A typical workflow is:
#' `read_fluorpen_xlsx(file) -> fluorpen_to_ojip(raw) -> calc_fluorojip(ojip)`.
#' @importFrom readxl read_excel
#' @export
read_fluorpen_xlsx <- function(file, sheet = 1) {
  x <- readxl::read_excel(file, sheet = sheet, col_names = FALSE, .name_repair = "minimal")
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
  if (is.na(desc_row)) {
    desc_row <- nrow(x) + 1L
  }

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
  mode(raw_mat) <- "numeric"
  mat <- t(raw_mat)
  rownames(mat) <- sample_id
  colnames(mat) <- times_us

  footer_rows <- seq.int(footer_row, desc_row - 1L)
  footer_labels <- trimws(as.character(x[footer_rows, 1, drop = TRUE]))
  footer_vals <- as.data.frame(x[footer_rows, measure_cols, drop = FALSE], stringsAsFactors = FALSE, check.names = FALSE)
  summary_raw <- as.data.frame(t(footer_vals), stringsAsFactors = FALSE, check.names = FALSE)
  names(summary_raw) <- footer_labels
  summary_raw$sample_id <- sample_id
  summary_raw$measurement_time <- measurement_time
  summary_raw$protocol_id <- protocol_id

  summary_numeric <- summary_raw
  value_cols <- setdiff(names(summary_numeric), c("sample_id", "measurement_time", "protocol_id"))
  for (nm in value_cols) {
    summary_numeric[[nm]] <- suppressWarnings(as.numeric(as.character(summary_numeric[[nm]])))
  }

  rescale_to_upper <- function(x, upper) {
    y <- as.numeric(x)
    for (i in seq_along(y)) {
      while (is.finite(y[i]) && abs(y[i]) > upper) {
        y[i] <- y[i] / 10
      }
    }
    y
  }

  bounds <- c(
    "Vj" = 1.5,
    "Vi" = 1.5,
    "Fm/Fo" = 10,
    "Fv/Fo" = 10,
    "Fv/Fm" = 1.5,
    "Mo" = 2,
    "Sm" = 5000,
    "Ss" = 2,
    "N" = 10000,
    "Phi_Po" = 1.5,
    "Psi_o" = 1.5,
    "Phi_Eo" = 1.5,
    "Phi_Do" = 1.5,
    "Phi_Pav" = 2,
    "Pi_Abs" = 10,
    "ABS/RC" = 10,
    "TRo/RC" = 5,
    "ETo/RC" = 10,
    "DIo/RC" = 2
  )

  for (nm in intersect(names(bounds), names(summary_numeric))) {
    summary_numeric[[nm]] <- rescale_to_upper(summary_numeric[[nm]], bounds[[nm]])
  }

  list(
    sample_id = sample_id,
    measurement_time = measurement_time,
    protocol_id = protocol_id,
    times_us = times_us,
    times_ms = times_us / 1000,
    mat = mat,
    summary_raw = summary_raw,
    summary_numeric = summary_numeric
  )
}

#' Convert FluorPen traces to an OJIP summary table
#'
#' @param x A list returned by [read_fluorpen_xlsx()].
#' @return A data frame containing one row per sample with OJIP summary values
#'   ready for [calc_fluorojip()].
#' @details
#' This helper converts the FluorPen trace matrix into the fluorescence summary
#' structure expected by the core calculation workflow, including the O, K, J,
#' I, P, and area-related quantities extracted from the trace.
#'
#' A typical workflow is:
#' `read_fluorpen_xlsx(file) -> fluorpen_to_ojip(raw) -> calc_fluorojip(ojip)`.
#' @export
fluorpen_to_ojip <- function(x) {
  handypea_to_ojip(list(times_s = x$times_ms, mat = x$mat))
}

#' Calculate FluorOJIP parameters from a FluorPen workbook
#'
#' @param file Path to the FluorPen `.xlsx` file.
#' @param sheet Sheet name or numeric index. Defaults to the first sheet.
#' @return A data frame with FluorOJIP / JIP-test parameters calculated from
#'   the imported workbook.
#' @details
#' This is a convenience wrapper for the typical FluorPen workflow:
#' `read_fluorpen_xlsx(file) -> fluorpen_to_ojip(raw) -> calc_fluorojip(ojip)`.
#'
#' Use this function when you want to import a supported FluorPen workbook and
#' compute JIP-test parameters in one step.
#' @export
calc_fluorojip_fluorpen <- function(file, sheet = 1) {
  raw <- read_fluorpen_xlsx(file, sheet = sheet)
  calc_fluorojip(fluorpen_to_ojip(raw))
}
