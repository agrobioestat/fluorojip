#' Read HandyPEA CSV export
#'
#' @param file Path to the HandyPEA CSV file.
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

#' Convert HandyPEA to OJIP summary
#'
#' @param x A list object returned by read_handypea_csv.
#' @export
handypea_to_ojip <- function(x) {
  times <- x$times_s
  mat <- x$mat
  ids <- rownames(mat)

  # Key indices for J (2ms) and I (30ms)
  idx_j <- which.min(abs(times - 0.002))
  idx_i <- which.min(abs(times - 0.03))

  # Fo (50us or first point)
  Fo <- mat[, 1]

  # Fm and Area
  Fm <- apply(mat, 1, max, na.rm=TRUE)
  area <- numeric(nrow(mat))

  for(r in 1:nrow(mat)) {
    ix_fm <- which.max(mat[r,])
    if(length(ix_fm)==0) ix_fm <- length(times)
    t_sub <- times[1:ix_fm]
    f_sub <- mat[r, 1:ix_fm]
    area[r] <- trapz(t_sub, Fm[r] - f_sub)
  }

  data.frame(
    sample_id = ids,
    fo = Fo,
    fm = Fm,
    j = mat[, idx_j],
    i = mat[, idx_i],
    p = Fm,
    area = area,
    stringsAsFactors = FALSE
  )
}

#' Calculate fluorojip from HandyPEA file
#'
#' @param file Path to the HandyPEA CSV file.
#' @export
calc_fluorojip_handypea <- function(file) {
  raw <- read_handypea_csv(file)
  df_summ <- handypea_to_ojip(raw)
  # Call the main function of your package (calc_fluorojip)
  # Assuming calc_fluorojip already exists in your package
  calc_fluorojip(df_summ)
}
