#' Read a Biolyzer-exported CSV trace table
#'
#' Reads a wide CSV trace table exported by Biolyzer (Fluoromatics) from
#' Hansatech Handy-PEA measurements and prepares it for [biolyzer_to_ojip()].
#'
#' The supported layout has one header row whose first cell contains
#' `Record No` followed by one time value per column, and one row per trace
#' whose first cell is the trace identifier. Rows labelled `End Of File` and
#' empty columns are ignored. Time values may be in seconds, milliseconds, or
#' microseconds.
#'
#' `read_handypea_csv()` is the name used in fluorojip 0.1.x and is kept as
#' an alias: the name refers to the Handy-PEA instrument that produced the
#' traces, whereas the file layout is the one written by Biolyzer.
#'
#' @param file Path to the CSV file.
#' @return A list with `times_s` (time values as written in the header, kept
#'   under this name for backward compatibility even if they are not in
#'   seconds), `ids` (trace identifiers), and `mat` (numeric matrix, one row
#'   per trace).
#' @details The function stops with an informative error if the `Record No`
#'   header or numeric time values cannot be found.
#' @importFrom utils read.csv
#' @examples
#' txt <- c(
#'   '"Record No",0.00002,0.00027,0.002,0.03,0.10',
#'   '"S1",280,340,700,950,1200',
#'   '"S2",300,360,730,980,1250'
#' )
#' f <- tempfile(fileext = ".csv")
#' writeLines(txt, f)
#'
#' raw <- read_biolyzer_csv(f)
#' raw$times_s
#' dim(raw$mat)
#' @export
read_biolyzer_csv <- function(file) {
  lines <- readLines(file, warn = FALSE)

  idx <- grep("Record No", lines)[1]
  if (is.na(idx)) stop("Header 'Record No' not found; this is not a supported Biolyzer CSV trace table.", call. = FALSE)

  header_clean <- gsub('"', "", lines[idx])
  parts <- strsplit(header_clean, ",")[[1]]
  times_vals <- suppressWarnings(as.numeric(parts))
  times_s <- times_vals[!is.na(times_vals)]
  if (length(times_s) == 0) stop("No numeric time values found in the 'Record No' header.", call. = FALSE)

  df <- utils::read.csv(file, skip = idx, header = FALSE, stringsAsFactors = FALSE)
  df <- df[!grepl("End Of File", df[[1]]), , drop = FALSE]
  df <- df[, !apply(df, 2, function(x) all(is.na(x) | x == "")), drop = FALSE]

  ids <- as.character(df[[1]])
  mat_data <- as.matrix(df[, -1, drop = FALSE])
  if (ncol(mat_data) > length(times_s)) {
    mat_data <- mat_data[, seq_along(times_s), drop = FALSE]
  }
  if (ncol(mat_data) < length(times_s)) {
    stop("Fewer fluorescence columns than time values in the header.", call. = FALSE)
  }

  mat_data <- suppressWarnings(apply(mat_data, 2, as.numeric))
  if (is.null(dim(mat_data))) mat_data <- matrix(mat_data, nrow = length(ids))
  colnames(mat_data) <- times_s
  rownames(mat_data) <- ids

  list(times_s = times_s, ids = ids, mat = mat_data)
}

#' @rdname read_biolyzer_csv
#' @export
read_handypea_csv <- read_biolyzer_csv

#' Read a Biolyzer Excel export (trace data and JIP parameters)
#'
#' Reads an Excel workbook exported by Biolyzer containing a `Trace data`
#' sheet (time in ms in the second column, one column per trace) and,
#' optionally, a `JIP Parameters` sheet with the values calculated by
#' Biolyzer. The bundled example returned by
#' [fluorojip_example_biolyzer_file()] has this layout.
#'
#' @param file Path to the `.xls` or `.xlsx` workbook.
#' @param trace_sheet,jip_sheet Sheet names.
#' @return A list with `times_ms`, `ids`, `mat` (one row per trace), and
#'   `jip` (data frame of the Biolyzer parameter sheet with its original
#'   column names, or `NULL` if the sheet is absent).
#' @details Rows after the trace block (labelled `Fm`, `Point # Tfm`, and
#'   `Total Points`) and zero-padded time entries are discarded.
#' @examples
#' raw <- read_biolyzer_xls(fluorojip_example_biolyzer_file())
#' dim(raw$mat)
#' range(raw$times_ms)
#' @export
read_biolyzer_xls <- function(file, trace_sheet = "Trace data", jip_sheet = "JIP Parameters") {
  sheets <- readxl::excel_sheets(file)
  if (!trace_sheet %in% sheets) {
    stop(sprintf("Sheet '%s' not found in the workbook.", trace_sheet), call. = FALSE)
  }
  tr <- as.data.frame(suppressMessages(readxl::read_excel(file, sheet = trace_sheet, col_names = FALSE)))
  first <- trimws(as.character(tr[[1]]))
  stop_row <- which(first == "Fm")[1]
  if (is.na(stop_row)) stop_row <- nrow(tr) + 1L
  rows <- seq.int(3L, stop_row - 1L)
  times_ms <- suppressWarnings(as.numeric(tr[rows, 2]))
  vals <- tr[rows, -(1:2), drop = FALSE]
  mat <- t(vapply(vals, function(x) suppressWarnings(as.numeric(x)), numeric(length(rows))))
  ids <- trimws(as.character(unlist(tr[1, -(1:2)])))
  rownames(mat) <- ids
  keep <- is.finite(times_ms) & times_ms > 0
  mat <- mat[, keep, drop = FALSE]
  times_ms <- times_ms[keep]
  colnames(mat) <- times_ms

  jip <- NULL
  if (!is.null(jip_sheet) && jip_sheet %in% sheets) {
    jp <- suppressMessages(readxl::read_excel(file, sheet = jip_sheet, col_names = FALSE))
    hdr <- as.character(unlist(jp[1, ]))
    jip <- as.data.frame(jp[-1, , drop = FALSE], stringsAsFactors = FALSE)
    names(jip) <- hdr
  }
  list(times_ms = times_ms, ids = ids, mat = mat, jip = jip)
}

#' Convert Biolyzer traces to an OJIP summary table
#'
#' Applies [trace_to_ojip()] with the `"biolyzer"` preset (Fo at 20 us, early
#' O-J point at 270 us, J at 2 ms, I at 30 ms, Fm at the trace maximum) to
#' traces read by [read_biolyzer_csv()] or [read_biolyzer_xls()].
#'
#' `handypea_to_ojip()` is the 0.1.x name and is kept as an alias.
#'
#' @param x A list returned by [read_biolyzer_csv()] (elements `times_s` and
#'   `mat`) or [read_biolyzer_xls()] (elements `times_ms` and `mat`).
#' @param ... Further arguments passed to [trace_to_ojip()], for example
#'   `tolerance_ms`, `targets_ms`, or `step_method`.
#' @return The data frame returned by [trace_to_ojip()].
#' @examples
#' raw <- list(
#'   times_s = c(0.00002, 0.00027, 0.002, 0.03, 0.10),
#'   mat = matrix(
#'     c(280, 340, 700, 950, 1200,
#'       300, 360, 730, 980, 1250),
#'     nrow = 2, byrow = TRUE,
#'     dimnames = list(c("S1", "S2"), NULL)
#'   )
#' )
#' biolyzer_to_ojip(raw)
#' @export
biolyzer_to_ojip <- function(x, ...) {
  args <- list(...)
  if (!is.null(x$times_ms)) {
    times <- x$times_ms
    if (is.null(args$time_unit)) args$time_unit <- "ms"
  } else {
    times <- x$times_s
  }
  if (is.null(args$preset)) args$preset <- "biolyzer"
  do.call(trace_to_ojip, c(list(times = times, mat = x$mat), args))
}

#' @rdname biolyzer_to_ojip
#' @export
handypea_to_ojip <- biolyzer_to_ojip

#' Calculate JIP-test parameters from a Biolyzer export
#'
#' Reads a Biolyzer CSV trace table or Excel workbook, extracts the OJIP
#' summary with [biolyzer_to_ojip()], and calls [calc_fluorojip()].
#' `calc_fluorojip_handypea()` is the 0.1.x name and is kept as an alias.
#'
#' @param file Path to a Biolyzer CSV trace table (`.csv`, `.txt`) or Excel
#'   export (`.xls`, `.xlsx`).
#' @param ... Further arguments passed to [trace_to_ojip()].
#' @return The data frame returned by [calc_fluorojip()].
#' @examples
#' res <- calc_fluorojip_biolyzer(fluorojip_example_biolyzer_file())
#' res[, c("sample_id", "Fv_Fm", "PI_abs", "qc_flags")]
#' @export
calc_fluorojip_biolyzer <- function(file, ...) {
  ext <- tolower(tools::file_ext(file))
  raw <- if (ext %in% c("xls", "xlsx")) read_biolyzer_xls(file) else read_biolyzer_csv(file)
  calc_fluorojip(biolyzer_to_ojip(raw, ...))
}

#' @rdname calc_fluorojip_biolyzer
#' @export
calc_fluorojip_handypea <- calc_fluorojip_biolyzer
