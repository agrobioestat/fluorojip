library(readxl)
find_project_root <- function(start = getwd()) {
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

workbook_name <- "OJIPExporttoExcelTest001-01062024at20h31.xls"
source_copy <- file.path(jroot, "inst", "extdata", workbook_name)
installed_copy <- system.file("extdata", workbook_name, package = "fluorojip")

x <- if (file.exists(source_copy)) {
  normalizePath(source_copy, winslash = "/", mustWork = TRUE)
} else if (nzchar(installed_copy)) {
  normalizePath(installed_copy, winslash = "/", mustWork = TRUE)
} else {
  stop("Could not find the Biolyzer example workbook in inst/extdata.", call. = FALSE)
}
jp <- read_excel(x, sheet = 'JIP Parameters', col_names = FALSE)
hdr <- as.character(unlist(jp[1, ]))
dat <- as.data.frame(jp[-1, ], stringsAsFactors = FALSE)
names(dat) <- hdr
num <- function(x) suppressWarnings(as.numeric(as.character(x)))

inp <- data.frame(
  sample_id = as.character(dat[['0 Trace Name']]),
  t_fm = num(dat[['18 T(Fm)']]),
  fo = num(dat[['22 Fo']]),
  k = num(dat[['13 F(K)']]),
  j = num(dat[['15 F(J)']]),
  i = num(dat[['17 F(I)']]),
  fm = num(dat[['23 Fm']]),
  stringsAsFactors = FALSE
)

res <- calc_fluorojip(inp)

comp <- data.frame(
  sample_id = inp[['sample_id']],
  fo = inp[['fo']],
  k = inp[['k']],
  j = inp[['j']],
  i = inp[['i']],
  fm = inp[['fm']],
  fluorojip_FvFm = res[['Fv_Fm']],
  biolyzer_FvFm = num(dat[['39 Fv/Fm']]),
  fluorojip_Mo = res[['Mo']],
  biolyzer_Mo = num(dat[['51 Mo']]),
  fluorojip_ABS_RC = res[['ABS_RC']],
  biolyzer_ABS_RC = num(dat[['68 ABS/RC']]),
  fluorojip_phiPo = res[['phi_Po']],
  biolyzer_TRo_ABS = num(dat[['89 TRo/ABS']]),
  fluorojip_phiEo = res[['phi_Eo']],
  biolyzer_ETo_ABS = num(dat[['90 ETo/ABS']]),
  fluorojip_PI_abs = res[['PI_abs']],
  biolyzer_PI_abs1 = num(dat[['101 PI(abs)1']]),
  biolyzer_PI_abs2 = num(dat[['102 PI(abs)2']]),
  stringsAsFactors = FALSE
)

comp$diff_FvFm <- comp$fluorojip_FvFm - comp$biolyzer_FvFm
comp$diff_Mo <- comp$fluorojip_Mo - comp$biolyzer_Mo
comp$diff_ABS_RC <- comp$fluorojip_ABS_RC - comp$biolyzer_ABS_RC
comp$diff_TRo_ABS <- comp$fluorojip_phiPo - comp$biolyzer_TRo_ABS
comp$diff_ETo_ABS <- comp$fluorojip_phiEo - comp$biolyzer_ETo_ABS
comp$diff_PI_abs1 <- comp$fluorojip_PI_abs - comp$biolyzer_PI_abs1
comp$diff_PI_abs2 <- comp$fluorojip_PI_abs - comp$biolyzer_PI_abs2

out <- file.path(jroot, "tools", "biolyzer_comparison.csv")
write.csv(comp, out, row.names = FALSE)

print(comp)
cat('\nMean absolute differences:\n')
print(c(
  FvFm = mean(abs(comp$diff_FvFm), na.rm = TRUE),
  Mo = mean(abs(comp$diff_Mo), na.rm = TRUE),
  ABS_RC = mean(abs(comp$diff_ABS_RC), na.rm = TRUE),
  TRo_ABS = mean(abs(comp$diff_TRo_ABS), na.rm = TRUE),
  ETo_ABS = mean(abs(comp$diff_ETo_ABS), na.rm = TRUE),
  PI_abs1 = mean(abs(comp$diff_PI_abs1), na.rm = TRUE),
  PI_abs2 = mean(abs(comp$diff_PI_abs2), na.rm = TRUE)
))
cat('\nMax absolute differences:\n')
print(c(
  FvFm = max(abs(comp$diff_FvFm), na.rm = TRUE),
  Mo = max(abs(comp$diff_Mo), na.rm = TRUE),
  ABS_RC = max(abs(comp$diff_ABS_RC), na.rm = TRUE),
  TRo_ABS = max(abs(comp$diff_TRo_ABS), na.rm = TRUE),
  ETo_ABS = max(abs(comp$diff_ETo_ABS), na.rm = TRUE),
  PI_abs1 = max(abs(comp$diff_PI_abs1), na.rm = TRUE),
  PI_abs2 = max(abs(comp$diff_PI_abs2), na.rm = TRUE)
))
cat('\nSource workbook: ', x, '\n', sep='')
cat('Saved to: ', normalizePath(out, winslash = "/", mustWork = FALSE), '\n', sep='')
