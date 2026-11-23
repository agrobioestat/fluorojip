# Builds all manuscript figures (PDF + 600-dpi PNG) for the RSOS revision.
# Usage: Rscript make_figures.R <validation_script> <out_dir>
args <- commandArgs(trailingOnly = TRUE)
val_script <- args[1]
out_dir <- if (length(args) >= 2) args[2] else "figures"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
suppressPackageStartupMessages(library(fluorojip))
stopifnot(utils::packageVersion("fluorojip") >= "0.2.0")

save_fig <- function(fun, name, w, h) {
  grDevices::pdf(file.path(out_dir, paste0(name, ".pdf")), width = w, height = h)
  fun(); grDevices::dev.off()
  grDevices::png(file.path(out_dir, paste0(name, ".png")), width = w, height = h, units = "in", res = 600)
  fun(); grDevices::dev.off()
}

# Figure 1: architecture ------------------------------------------------------
fig1 <- function() {
  op <- par(mar = c(0, 0, 0, 0)); on.exit(par(op))
  plot(NA, xlim = c(0, 100), ylim = c(0, 64), axes = FALSE, xlab = "", ylab = "", asp = NA)
  box_ <- function(x, y, w, h, title, body, fill) {
    rect(x, y, x + w, y + h, col = fill, border = "grey25", lwd = 1.2)
    text(x + w / 2, y + h - 2.6, title, font = 2, cex = 0.82)
    text(x + w / 2, y + h / 2 - 2.2, body, cex = 0.66)
  }
  arr <- function(x0, y0, x1, y1) arrows(x0, y0, x1, y1, length = 0.08, lwd = 1.3, col = "grey20")
  c_in <- "#DCEAF5"; c_ex <- "#FCE3CF"; c_calc <- "#D9F0E3"; c_out <- "#EFE3F1"
  box_(1, 46, 22, 16, "Exported traces",
       "Biolyzer CSV / Excel\n(Handy-PEA)\nFluorPen workbook\nother: times + matrix", c_in)
  box_(1, 24, 22, 16, "Summary table",
       "one row per measurement\nfo, f300us (k), j, i, fm,\narea; metadata kept", c_in)
  box_(29, 40, 24, 22, "Extraction",
       "trace_to_ojip()\npreset: target times,\ntolerance windows,\nbackground rule\nreports times used\n+ quality flags", c_ex)
  box_(59, 30, 20, 22, "Calculation",
       "calc_fluorojip()\nequations in Table 3\nNA when inputs are\nmissing or invalid\n+ qc_flags", c_calc)
  box_(83, 40, 16, 22, "Checks",
       "plot_ojip_trace()\nvalidate_biolyzer()\nvalidate_fluorpen()\nagreement_metrics()", c_out)
  box_(83, 10, 16, 24, "Outputs",
       "data frame\nnormalized tables\nheatmap, 3D plot\nCSV export", c_out)
  box_(29, 4, 50, 14, "Shiny interface: run_fluorojip_app()",
       "same functions through a graphical interface;\nexports results and an analysis log (R script with settings and checksum)", "#F2F2F2")
  arr(23, 54, 29, 52); arr(53, 48, 59, 45); arr(23, 32, 59, 36)
  arr(79, 45, 83, 50); arr(79, 36, 83, 28)
  arr(54, 20, 62, 30)
}
save_fig(fig1, "Figure_1_architecture", 7.2, 4.6)

# Figure 2: traces with extracted points --------------------------------------
fig2 <- function() {
  op <- par(mfrow = c(1, 2), mar = c(4.2, 5.4, 2.8, 0.8), mgp = c(2.4, 0.7, 0), las = 1, cex.axis = 0.85)
  on.exit(par(op))
  bz <- read_biolyzer_xls(fluorojip_example_biolyzer_file())
  plot_ojip_trace(bz$times_ms, bz$mat, time_unit = "ms", preset = "biolyzer",
                  samples = c(1, 2, 8), normalize = "none", main = "")
  mtext("(a) Biolyzer export (Handy-PEA)", side = 3, adj = 0, line = 1.3, font = 2, cex = 0.85)
  fp <- suppressMessages(read_fluorpen_xlsx(system.file("extdata", "FluorPen_test.xlsx", package = "fluorojip")))
  plot_ojip_trace(fp$times_us, fp$mat, preset = "fluorpen", samples = c(1, 2, 5),
                  normalize = "none", main = "")
  mtext("(b) FluorPen export", side = 3, adj = 0, line = 1.3, font = 2, cex = 0.85)
}
save_fig(fig2, "Figure_2_traces", 7.4, 3.8)

# Figures 3 and 4: validation (from the archived validation script) ----------
tmp <- file.path(tempdir(), "val")
system2(file.path(R.home("bin"), "Rscript"), c(shQuote(val_script), shQuote(tmp)))
file.copy(file.path(tmp, "fig_validation_agreement.pdf"), file.path(out_dir, "Figure_5_agreement.pdf"), overwrite = TRUE)
file.copy(file.path(tmp, "fig_validation_agreement.png"), file.path(out_dir, "Figure_5_agreement.png"), overwrite = TRUE)
file.copy(file.path(tmp, "fig_convention_sensitivity.pdf"), file.path(out_dir, "Figure_6_conventions.pdf"), overwrite = TRUE)
file.copy(file.path(tmp, "fig_convention_sensitivity.png"), file.path(out_dir, "Figure_6_conventions.png"), overwrite = TRUE)

# Figure 5: exploratory outputs -------------------------------------------------
res <- calc_fluorojip(example_fluorojip)
fig5a <- function() {
  plot_heatmap_fluorojip(res, params = c("Fv_Fm", "Vj", "Mo", "ABS_RC", "TRo_RC", "ETo_RC", "DIo_RC", "PI_abs"),
                         main = "")
}
fig5b <- function() {
  op <- par(mar = c(3, 3, 1, 1)); on.exit(par(op))
  plot_3d_fluorojip(res, params = c("Fv_Fm", "ABS_RC", "PI_abs"), angle = 55, legend_pos = "topleft")
}
save_fig(fig5a, "Figure_3a_heatmap", 6, 3.6)
save_fig(fig5b, "Figure_3b_scatter3d", 5, 4.4)
message("Figures written to ", normalizePath(out_dir))
