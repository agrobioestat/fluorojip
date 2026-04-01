library(readxl)

script_dir <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = TRUE)))
  }
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

find_project_root <- function(start = script_dir()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)

  repeat {
    if (file.exists(file.path(current, "DESCRIPTION"))) {
      return(current)
    }

    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Could not find the project root (folder containing DESCRIPTION).", call. = FALSE)
    }
    current <- parent
  }
}

jroot <- find_project_root()
source(file.path(jroot, "R", "calc_fluorojip.R"))
source(file.path(jroot, "R", "handypea_io.R"))
source(file.path(jroot, "R", "fluorpen_io.R"))

fluorpen_dir <- file.path(jroot, "fluorpen")
files <- list.files(fluorpen_dir, pattern = "\\.xlsx$", full.names = TRUE)
files <- files[!grepl("(^~\\$)|(^\\.~lock\\.)", basename(files))]

if (length(files) == 0) {
  stop("Could not find any FluorPen .xlsx files in the project-local fluorpen folder.", call. = FALSE)
}

all_comp <- list()

for (file in files) {
  raw <- read_fluorpen_xlsx(file)
  ojip <- fluorpen_to_ojip(raw)
  res <- calc_fluorojip(ojip)
  vendor <- raw$summary_numeric

  comp <- data.frame(
    source_file = basename(file),
    sample_id = ojip$sample_id,
    fo = ojip$fo,
    k = ojip$k,
    j = ojip$j,
    i = ojip$i,
    fm = ojip$fm,
    fluorojip_FvFm = res$Fv_Fm,
    vendor_FvFm = vendor[["Fv/Fm"]],
    fluorojip_Mo = res$Mo,
    vendor_Mo = vendor[["Mo"]],
    fluorojip_ABS_RC = res$ABS_RC,
    vendor_ABS_RC = vendor[["ABS/RC"]],
    fluorojip_TRo_RC = res$TRo_RC,
    vendor_TRo_RC = vendor[["TRo/RC"]],
    fluorojip_ETo_RC = res$ETo_RC,
    vendor_ETo_RC = vendor[["ETo/RC"]],
    fluorojip_DIo_RC = res$DIo_RC,
    vendor_DIo_RC = vendor[["DIo/RC"]],
    fluorojip_psi_Eo = res$psi_Eo,
    vendor_Psi_o = vendor[["Psi_o"]],
    fluorojip_phi_Eo = res$phi_Eo,
    vendor_Phi_Eo = vendor[["Phi_Eo"]],
    fluorojip_PI_abs = res$PI_abs,
    vendor_PI_abs = vendor[["Pi_Abs"]],
    stringsAsFactors = FALSE
  )

  comp$diff_FvFm <- comp$fluorojip_FvFm - comp$vendor_FvFm
  comp$diff_Mo <- comp$fluorojip_Mo - comp$vendor_Mo
  comp$diff_ABS_RC <- comp$fluorojip_ABS_RC - comp$vendor_ABS_RC
  comp$diff_TRo_RC <- comp$fluorojip_TRo_RC - comp$vendor_TRo_RC
  comp$diff_ETo_RC <- comp$fluorojip_ETo_RC - comp$vendor_ETo_RC
  comp$diff_DIo_RC <- comp$fluorojip_DIo_RC - comp$vendor_DIo_RC
  comp$diff_Psi_o <- comp$fluorojip_psi_Eo - comp$vendor_Psi_o
  comp$diff_Phi_Eo <- comp$fluorojip_phi_Eo - comp$vendor_Phi_Eo
  comp$diff_PI_abs <- comp$fluorojip_PI_abs - comp$vendor_PI_abs

  all_comp[[basename(file)]] <- comp
}

comp_all <- do.call(rbind, all_comp)
out <- file.path(jroot, "tools", "fluorpen_comparison.csv")
write.csv(comp_all, out, row.names = FALSE)

metrics <- c(
  "FvFm" = "diff_FvFm",
  "Mo" = "diff_Mo",
  "ABS_RC" = "diff_ABS_RC",
  "TRo_RC" = "diff_TRo_RC",
  "ETo_RC" = "diff_ETo_RC",
  "DIo_RC" = "diff_DIo_RC",
  "Psi_o" = "diff_Psi_o",
  "Phi_Eo" = "diff_Phi_Eo",
  "PI_abs" = "diff_PI_abs"
)

cat("Rows compared:", nrow(comp_all), "\n")
cat("Files:\n")
print(unique(comp_all$source_file))

cat("\nMean absolute differences by file:\n")
for (fname in unique(comp_all$source_file)) {
  sub <- comp_all[comp_all$source_file == fname, , drop = FALSE]
  vals <- sapply(metrics, function(col) mean(abs(sub[[col]]), na.rm = TRUE))
  cat("\n", fname, "\n", sep = "")
  print(vals)
}

cat("\nOverall mean absolute differences:\n")
print(sapply(metrics, function(col) mean(abs(comp_all[[col]]), na.rm = TRUE)))

cat("\nOverall max absolute differences:\n")
print(sapply(metrics, function(col) max(abs(comp_all[[col]]), na.rm = TRUE)))

cat("\nSaved to: ", normalizePath(out, winslash = "/", mustWork = FALSE), "\n", sep = "")
