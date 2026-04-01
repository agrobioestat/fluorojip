#' Read a supported Biolyzer-exported CSV trace table
#'
#' Reads a CSV trace table exported by Biolyzer and prepares it for
#' downstream OJIP summary extraction.
#'
#' Although the function name retains the historical `handypea` prefix for
#' backward compatibility, this workflow is based on supported exported trace
#' tables rather than direct parsing of proprietary raw instrument files.
#'
#' @param file Path to the supported Biolyzer-exported CSV trace table.
#' @return A list with three elements: `times_s`, the extracted time points;
#'   `ids`, the trace identifiers; and `mat`, a numeric matrix containing the
#'   fluorescence traces.
#' @details The function locates the trace header, extracts the time values,
#'   reads the numeric trace block, removes trailing non-data rows, and returns
#'   the result in a format suitable for `handypea_to_ojip()`.
#' @importFrom utils read.csv
#' @export
read_handypea_csv <- function(file) {
  lines <- readLines(file, warn = FALSE)

  # Find header
  idx <- grep("Record No", lines)[1]
  if (is.na(idx)) stop("Header 'Record No' not found.")

  # Extract times manually
  header_clean <- gsub('"', '', lines[idx])
  parts <- strsplit(header_clean, ",")[[1]]
  times_vals <- suppressWarnings(as.numeric(parts))
  valid_indices <- !is.na(times_vals)
  times_s <- times_vals[valid_indices]

  if (length(times_s) == 0) stop("Error reading times.")

  # Read data
  df <- utils::read.csv(file, skip = idx, header = FALSE, stringsAsFactors = FALSE)

  # Clean unwanted rows
  df <- df[!grepl("End Of File", df[[1]]), ]
  df <- df[, !apply(df, 2, function(x) all(is.na(x) | x == ""))]

  # Build matrix
  ids <- as.character(df[[1]])
  mat_data <- as.matrix(df[, -1, drop = FALSE])

  if (ncol(mat_data) > length(times_s)) {
    mat_data <- mat_data[, 1:length(times_s), drop = FALSE]
  }

  mode(mat_data) <- "numeric"
  colnames(mat_data) <- times_s
  rownames(mat_data) <- ids

  list(times_s = times_s, ids = ids, mat = mat_data)
}

# Helper function for integration
trapz <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]
  if (length(x) < 2) return(NA_real_)
  sum((x[-1] - x[-length(x)]) * (y[-1] + y[-length(y)]) / 2)
}

#' Convert a supported CSV trace export to an OJIP summary table
#'
#' Converts a supported exported trace table into an OJIP summary table ready
#' for downstream FluorOJIP parameter calculation.
#'
#' Although the function name retains the historical `handypea` prefix for
#' backward compatibility, the supported import workflow is based on
#' Biolyzer-exported trace tables.
#'
#' @param x A list object returned by `read_handypea_csv()`.
#' @return A data frame containing `sample_id`, `t_fm`, `fo`, `k`, `fm`, `j`,
#'   `i`, `p`, and `area`.
#' @details Time values are normalized internally to milliseconds so the
#'   function can target the standard OJIP steps even when exported trace times
#'   are recorded in seconds, milliseconds, or microseconds.
#' @export
handypea_to_ojip <- function(x) {
  times <- x$times_s
  mat <- x$mat
  ids <- rownames(mat)

  # Normalize time to milliseconds so we can support raw traces exported
  # in seconds (e.g. 0.0003), milliseconds (e.g. 0.27), or microseconds
  # (e.g. 270) while still targeting the standard OJIP steps.
  infer_times_ms <- function(times) {
    targets_ms <- c(0.02, 0.27, 2, 30)
    score <- function(factor) {
      converted <- times * factor
      sum(vapply(targets_ms, function(target) min(abs(converted - target), na.rm = TRUE), numeric(1)))
    }

    candidates <- c(0.001, 1, 1000)
    best_factor <- candidates[which.min(vapply(candidates, score, numeric(1)))]
    times * best_factor
  }

  pick_idx <- function(times_ms, target_ms) {
    which.min(abs(times_ms - target_ms))
  }

  times_ms <- infer_times_ms(times)

  # Key indices for O (0.02 ms), K (0.27 ms), J (2 ms) and I (30 ms)
  idx_o <- pick_idx(times_ms, 0.02)
  idx_k <- pick_idx(times_ms, 0.27)
  idx_j <- pick_idx(times_ms, 2)
  idx_i <- pick_idx(times_ms, 30)

  # Fo is taken from the point nearest 0.02 ms (20 us). This also works
  # when the first available point is slightly later, such as 0.05 ms.
  Fo <- mat[, idx_o]

  # Fm, time to Fm, and Area
  Fm <- apply(mat, 1, max, na.rm = TRUE)
  t_fm <- numeric(nrow(mat))
  area <- numeric(nrow(mat))

  for (r in 1:nrow(mat)) {
    ix_fm <- which.max(mat[r, ])
    if (length(ix_fm) == 0) ix_fm <- length(times_ms)
    t_fm[r] <- times_ms[ix_fm]
    t_sub <- times_ms[1:ix_fm]
    f_sub <- mat[r, 1:ix_fm]
    area[r] <- trapz(t_sub, Fm[r] - f_sub)
  }

  data.frame(
    sample_id = ids,
    t_fm = t_fm,
    fo = Fo,
    k = mat[, idx_k],
    fm = Fm,
    j = mat[, idx_j],
    i = mat[, idx_i],
    p = Fm,
    area = area,
    stringsAsFactors = FALSE
  )
}

#' Calculate FluorOJIP parameters from a supported exported table
#'
#' Reads a supported exported trace table, derives an OJIP summary, and then
#' computes FluorOJIP parameters.
#'
#' Although the function name retains the historical `handypea` prefix for
#' backward compatibility, the supported import workflow is based on
#' Biolyzer-exported trace tables.
#'
#' @param file Path to the supported Biolyzer-exported CSV trace table.
#' @return A data frame with FluorOJIP parameters returned by
#'   `calc_fluorojip()`.
#' @details Typical workflow:\cr
#'   `read_handypea_csv()` -> `handypea_to_ojip()` -> `calc_fluorojip()`.
#' @export
calc_fluorojip_handypea <- function(file) {
  raw <- read_handypea_csv(file)
  df_summ <- handypea_to_ojip(raw)
  calc_fluorojip(df_summ)
}
