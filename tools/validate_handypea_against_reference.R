args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 2) {
  stop(
    paste(
      "Usage:",
      "Rscript tools/validate_handypea_against_reference.R",
      "<handypea_export.csv>",
      "<reference_parameters.csv>",
      "[output_comparison.csv]"
    ),
    call. = FALSE
  )
}

handypea_file <- normalizePath(args[[1]], winslash = "/", mustWork = TRUE)
reference_file <- normalizePath(args[[2]], winslash = "/", mustWork = TRUE)
output_file <- if (length(args) >= 3) {
  args[[3]]
} else {
  "validation_comparison.csv"
}

source("R/calc_fluorojip.R")
source("R/handypea_io.R")

read_reference <- function(path) {
  ref <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  names(ref) <- trimws(names(ref))

  if (!("sample_id" %in% names(ref))) {
    stop("Reference file must contain a 'sample_id' column.", call. = FALSE)
  }

  ref
}

compare_numeric_columns <- function(calc, ref) {
  common <- intersect(names(calc), names(ref))
  common <- setdiff(common, "sample_id")

  if (length(common) == 0) {
    stop("No common parameter columns found between calculated and reference tables.", call. = FALSE)
  }

  merged <- merge(calc, ref, by = "sample_id", suffixes = c("_calc", "_ref"))
  if (nrow(merged) == 0) {
    stop("No matching sample_id values between calculated and reference tables.", call. = FALSE)
  }

  out <- data.frame(stringsAsFactors = FALSE)

  for (param in common) {
    calc_col <- paste0(param, "_calc")
    ref_col <- paste0(param, "_ref")

    vals_calc <- suppressWarnings(as.numeric(merged[[calc_col]]))
    vals_ref <- suppressWarnings(as.numeric(merged[[ref_col]]))

    ok <- is.finite(vals_calc) & is.finite(vals_ref)
    if (!any(ok)) {
      next
    }

    tmp <- data.frame(
      sample_id = merged$sample_id[ok],
      parameter = param,
      calculated = vals_calc[ok],
      reference = vals_ref[ok],
      abs_diff = vals_calc[ok] - vals_ref[ok],
      rel_diff_pct = ifelse(vals_ref[ok] != 0, 100 * (vals_calc[ok] - vals_ref[ok]) / vals_ref[ok], NA_real_),
      stringsAsFactors = FALSE
    )

    out <- rbind(out, tmp)
  }

  if (nrow(out) == 0) {
    stop("No overlapping numeric parameter columns were comparable.", call. = FALSE)
  }

  out
}

raw <- read_handypea_csv(handypea_file)
ojip <- handypea_to_ojip(raw)
calc <- calc_fluorojip(ojip)
ref <- read_reference(reference_file)

comparison <- compare_numeric_columns(calc, ref)
utils::write.csv(comparison, output_file, row.names = FALSE)

summary_tab <- aggregate(
  cbind(abs_diff, rel_diff_pct) ~ parameter,
  data = comparison,
  FUN = function(x) c(mean = mean(abs(x), na.rm = TRUE), max = max(abs(x), na.rm = TRUE))
)

print(comparison)
cat("\nSummary (absolute mean and max differences):\n")
print(summary_tab)
cat("\nComparison written to:", normalizePath(output_file, winslash = "/", mustWork = FALSE), "\n")
