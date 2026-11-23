# Reproducible validation of fluorojip against values exported by
# Biolyzer (Handy-PEA traces) and the FluorPen software.
#
# Usage (from the repository root or any directory):
#   Rscript validation/run_validation.R [output_dir]
#
# Requires fluorojip >= 0.2.0 and readxl. All input files are distributed with
# the package (inst/extdata). Outputs: CSV tables, figures (PDF and PNG), and
# session information.

args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args) >= 1) args[1] else "validation_output"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages(library(fluorojip))
stopifnot(utils::packageVersion("fluorojip") >= "0.2.0")

biolyzer_file <- fluorojip_example_biolyzer_file()
fluorpen_file <- system.file("extdata", "FluorPen_test.xlsx", package = "fluorojip")

key <- c("Fv_Fm", "Vj", "Vi", "Mo", "psi_Eo", "phi_Eo", "ABS_RC", "TRo_RC",
         "ETo_RC", "DIo_RC", "RC_ABS", "PI_abs", "Sm", "N",
         "ABS_CSo", "TRo_CSo", "ETo_CSo", "DIo_CSo", "RC_CSo",
         "ABS_CSm", "TRo_CSm", "ETo_CSm", "DIo_CSm")

# ---------------------------------------------------------------------------
# 1. Agreement in four configurations
# ---------------------------------------------------------------------------
runs <- list(
  biolyzer_engine   = validate_biolyzer(biolyzer_file, mode = "engine"),
  biolyzer_importer = validate_biolyzer(biolyzer_file, mode = "importer"),
  fluorpen_engine   = validate_fluorpen(fluorpen_file, mode = "engine"),
  fluorpen_importer = validate_fluorpen(fluorpen_file, mode = "importer")
)
metrics <- do.call(rbind, lapply(names(runs), function(nm) {
  cbind(configuration = nm, runs[[nm]]$metrics, stringsAsFactors = FALSE)
}))
write.csv(metrics, file.path(out_dir, "agreement_metrics_all.csv"), row.names = FALSE)
for (nm in names(runs)) {
  write.csv(runs[[nm]]$comparison, file.path(out_dir, paste0("comparison_", nm, ".csv")), row.names = FALSE)
  write.csv(runs[[nm]]$ojip, file.path(out_dir, paste0("ojip_input_", nm, ".csv")), row.names = FALSE)
}

# Compact table for the manuscript
tab_params <- c("fo", "f300us", "j", "i", "fm", "Fv_Fm", "Vj", "Mo", "phi_Eo",
                "ABS_RC", "ETo_RC", "DIo_RC", "PI_abs", "Sm", "ABS_CSo", "TRo_CSm")
compact <- metrics[metrics$parameter %in% tab_params,
                   c("configuration", "parameter", "n", "bias", "mae", "max_abs_diff", "lin_ccc")]
write.csv(compact, file.path(out_dir, "agreement_metrics_compact.csv"), row.names = FALSE)

# ---------------------------------------------------------------------------
# 2. Sensitivity to extraction conventions (FluorPen traces)
# ---------------------------------------------------------------------------
fp_raw <- suppressMessages(read_fluorpen_xlsx(fluorpen_file))
ref <- runs$fluorpen_importer$comparison
conv <- list(
  "FluorPen preset (0.2.0)" = list(preset = "fluorpen"),
  "No background subtraction" = list(preset = "fluorpen", background = "none"),
  "Fo at 21 us (0.1.x rule)" = list(preset = "fluorpen", targets_ms = c(fo = 0.021),
                                     tolerance_ms = c(fo = 0.005)),
  "Early point at 271 us" = list(preset = "fluorpen", targets_ms = c(f300us = 0.271)),
  "0.1.x convention (21 us, 271 us, no background)" =
    list(preset = "handypea", time_unit = "us", tolerance_ms = c(fo = 0.005, f300us = 0.005),
         targets_ms = c(fo = 0.021, f300us = 0.271))
)
sens_params <- c("Fv_Fm", "Vj", "Mo", "ABS_RC", "ETo_RC", "DIo_RC", "PI_abs")
sens <- do.call(rbind, lapply(names(conv), function(nm) {
  ojip <- suppressWarnings(do.call(fluorpen_to_ojip, c(list(x = fp_raw), conv[[nm]])))
  res <- suppressWarnings(calc_fluorojip(ojip, digits = NULL))
  do.call(rbind, lapply(sens_params, function(p) {
    r <- ref[ref$parameter == p, ]
    y <- r$reference[match(fp_raw$sample_id, r$sample_id)]
    a <- agreement_metrics(res[[p]], y)
    data.frame(convention = nm, parameter = p, n = a$n, bias = a$bias,
               mean_rel_diff_pct = 100 * mean((res[[p]] - y) / y, na.rm = TRUE),
               mae = a$mae, max_abs_diff = a$max_abs_diff, lin_ccc = a$lin_ccc)
  }))
}))
write.csv(sens, file.path(out_dir, "convention_sensitivity_fluorpen.csv"), row.names = FALSE)

# Biolyzer: effect of the Fo definition (F at 20 us vs Biolyzer 'Fo calc')
bz_raw <- read_biolyzer_xls(biolyzer_file)
jip <- bz_raw$jip
num <- function(nm) as.numeric(as.character(jip[[nm]]))
bz_ojip <- biolyzer_to_ojip(bz_raw)
bz_ojip <- bz_ojip[match(jip[["0 Trace Name"]], bz_ojip$sample_id), ]
bz_fo <- data.frame(
  sample_id = bz_ojip$sample_id,
  F20us = bz_ojip$fo,
  Biolyzer_F1 = num("9 F1 = Fo"),
  Biolyzer_Fo_calc = num("22 Fo"),
  difference = bz_ojip$fo - num("22 Fo")
)
write.csv(bz_fo, file.path(out_dir, "biolyzer_fo_definitions.csv"), row.names = FALSE)

# ---------------------------------------------------------------------------
# 3. Figures
# ---------------------------------------------------------------------------
pal <- c(new = "#0072B2", old = "#D55E00", eng = "#009E73")

fig_agreement <- function() {
  old_ojip <- suppressWarnings(do.call(fluorpen_to_ojip, c(list(x = fp_raw), conv[[5]])))
  old_res <- suppressWarnings(calc_fluorojip(old_ojip, digits = NULL))
  new_res <- calc_fluorojip(runs$fluorpen_importer$ojip, digits = NULL)
  bz_res <- runs$biolyzer_engine$comparison
  op <- par(mfrow = c(2, 2), mar = c(4.2, 4.4, 2.2, 0.8), mgp = c(2.4, 0.7, 0), las = 1)
  on.exit(par(op))
  labs <- c(Fv_Fm = "Fv/Fm", Mo = "Mo (1/ms)", ABS_RC = "ABS/RC", PI_abs = "PI_abs")
  for (k in seq_along(labs)) {
    p <- names(labs)[k]
    r <- ref[ref$parameter == p, ]
    y <- r$reference[match(fp_raw$sample_id, r$sample_id)]
    b <- bz_res[bz_res$parameter == p, ]
    lim <- range(c(y, old_res[[p]], new_res[[p]], b$reference, b$fluorojip), na.rm = TRUE)
    plot(y, old_res[[p]], xlim = lim, ylim = lim, pch = 1, col = pal["old"], cex = 0.8,
         xlab = paste("Reference software,", labs[[p]]), ylab = paste("fluorojip,", labs[[p]]))
    abline(0, 1, lty = 2, col = "grey40")
    points(y, new_res[[p]], pch = 16, col = pal["new"], cex = 0.7)
    points(b$reference, b$fluorojip, pch = 17, col = pal["eng"], cex = 0.9)
    mtext(paste0("(", letters[k], ")"), side = 3, adj = 0, line = 0.6, font = 2)
    if (k == 1) legend("topleft", bty = "n", cex = 0.8,
                       pch = c(16, 1, 17), col = pal[c("new", "old", "eng")],
                       legend = c("FluorPen, 0.2.0 preset (n = 106)",
                                  "FluorPen, 0.1.x convention (n = 106)",
                                  "Biolyzer, equations (n = 10)"))
  }
}

fig_sensitivity <- function() {
  s <- sens
  s$convention <- factor(s$convention, levels = names(conv))
  s$parameter <- factor(s$parameter, levels = rev(sens_params))
  cols <- c("#0072B2", "#E69F00", "#D55E00", "#CC79A7", "#000000")
  op <- par(mar = c(4.2, 6.5, 6.2, 1), las = 1)
  on.exit(par(op))
  xl <- range(s$mean_rel_diff_pct, na.rm = TRUE)
  xl <- c(min(-1, xl[1]), max(1, xl[2]))
  plot(NA, xlim = xl, ylim = c(0.5, length(sens_params) + 0.5), yaxt = "n",
       xlab = "Mean difference from FluorPen software (%)", ylab = "")
  pretty_lab <- c(Fv_Fm = "Fv/Fm", Vj = "Vj", Mo = "Mo", ABS_RC = "ABS/RC",
                  ETo_RC = "ETo/RC", DIo_RC = "DIo/RC", PI_abs = "PI_abs")
  axis(2, at = seq_along(levels(s$parameter)), labels = pretty_lab[levels(s$parameter)])
  abline(v = 0, lty = 2, col = "grey50")
  abline(h = seq_along(levels(s$parameter)) + 0.5, col = "grey90")
  off <- seq(0.3, -0.3, length.out = nlevels(s$convention))
  for (k in seq_along(levels(s$convention))) {
    d <- s[s$convention == levels(s$convention)[k], ]
    points(d$mean_rel_diff_pct, as.integer(d$parameter) + off[k], pch = c(16, 15, 17, 18, 4)[k], col = cols[k], cex = 1.1)
  }
  legend("bottom", inset = c(0, 1.02), xpd = NA, legend = levels(s$convention),
         pch = c(16, 15, 17, 18, 4), col = cols, bty = "n", cex = 0.8, ncol = 2)
}

save_fig <- function(fun, name, w, h) {
  grDevices::pdf(file.path(out_dir, paste0(name, ".pdf")), width = w, height = h)
  fun(); grDevices::dev.off()
  grDevices::png(file.path(out_dir, paste0(name, ".png")), width = w, height = h, units = "in", res = 600)
  fun(); grDevices::dev.off()
}
save_fig(fig_agreement, "fig_validation_agreement", 7, 6.4)
save_fig(fig_sensitivity, "fig_convention_sensitivity", 7, 4.6)

writeLines(capture.output(sessionInfo()), file.path(out_dir, "sessionInfo.txt"))
writeLines(c(paste("biolyzer_file:", basename(biolyzer_file), unname(tools::md5sum(biolyzer_file))),
             paste("fluorpen_file:", basename(fluorpen_file), unname(tools::md5sum(fluorpen_file)))),
           file.path(out_dir, "input_md5.txt"))
message("Validation outputs written to: ", normalizePath(out_dir))
