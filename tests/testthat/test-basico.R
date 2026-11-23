test_that("calc_fluorojip reproduces hand-calculated JIP-test values", {
  df <- data.frame(sample_id = "S1", fo = 100, f300us = 130, j = 220, i = 400, fm = 600, area = 15000)
  res <- calc_fluorojip(df)

  expect_equal(res$Fv, 500)
  expect_equal(res$Fv_Fm, 0.833333, tolerance = 1e-6)
  expect_equal(res$Vj, 0.24, tolerance = 1e-6)
  expect_equal(res$Vi, 0.6, tolerance = 1e-6)
  expect_equal(res$Mo, 0.24, tolerance = 1e-6)
  expect_equal(res$TRo_RC, 1, tolerance = 1e-6)
  expect_equal(res$ABS_RC, 1.2, tolerance = 1e-6)
  expect_equal(res$RC_ABS, 1 / 1.2, tolerance = 1e-6)
  expect_equal(res$ETo_RC, 0.76, tolerance = 1e-6)
  expect_equal(res$DIo_RC, 0.2, tolerance = 1e-6)
  expect_equal(res$Sm, 30, tolerance = 1e-6)
  expect_equal(res$N, 30, tolerance = 1e-6)
  expect_equal(res$PI_abs, 13.194444, tolerance = 1e-6)
  expect_equal(res$ABS_CSm, 600)
  expect_equal(res$DIo_CSm, 100, tolerance = 1e-6)
  expect_equal(res$ABS_CSo, 100)
  expect_equal(res$qc_flags, "ok")
})

test_that("k is accepted as a backward-compatible alias of f300us", {
  a <- calc_fluorojip(data.frame(fo = 100, f300us = 130, j = 220, i = 400, fm = 600))
  b <- calc_fluorojip(data.frame(fo = 100, k = 130, j = 220, i = 400, fm = 600))
  expect_equal(a$PI_abs, b$PI_abs)
})

test_that("mo_interval_ms changes the Mo factor explicitly", {
  df <- data.frame(fo = 100, f300us = 130, j = 220, i = 400, fm = 600)
  expect_equal(calc_fluorojip(df, mo_interval_ms = 0.5)$Mo, 0.12, tolerance = 1e-6)
  expect_error(calc_fluorojip(df, mo_interval_ms = -1))
})

test_that("slope-dependent outputs are NA when F300us is unavailable", {
  df <- data.frame(sample_id = "S1", fo = 100, j = 220, i = 400, fm = 600, area = 15000)
  expect_warning(res <- calc_fluorojip(df), "F300us")
  for (p in c("Mo", "TRo_RC", "ABS_RC", "ETo_RC", "DIo_RC", "RC_ABS", "RC_CSo", "N", "PI_abs")) {
    expect_true(is.na(res[[p]]), info = p)
  }
  expect_false(is.na(res$Fv_Fm))
  expect_match(res$qc_flags, "f300us_missing")
})

test_that("invalid inputs give NA and flags instead of numbers", {
  df <- data.frame(
    sample_id = c("bad_fm", "bad_j", "bad_k", "non_mono"),
    fo = c(100, 100, 100, 100), f300us = c(130, 130, 700, 300),
    j = c(220, 650, 220, 220), i = c(400, 400, 400, 400), fm = c(90, 600, 600, 600)
  )
  res <- suppressWarnings(calc_fluorojip(df))
  expect_true(is.na(res$Fv_Fm[1]))
  expect_match(res$qc_flags[1], "fo_fm_invalid")
  expect_true(is.na(res$Vj[2]) && is.na(res$PI_abs[2]))
  expect_match(res$qc_flags[2], "fj_out_of_range")
  expect_true(is.na(res$Mo[3]))
  expect_match(res$qc_flags[3], "f300us_out_of_range")
  expect_match(res$qc_flags[4], "non_monotonic")
  expect_error(calc_fluorojip(data.frame(fo = 1, j = 2, i = 3)), "Fm")
})

# --- trace extraction ---------------------------------------------------------

synthetic_trace <- function(t_ms) {
  # Smooth OJIP-like rise: three exponential phases on top of Fo = 500
  500 + 900 * (1 - exp(-t_ms / 0.6)) + 500 * (1 - exp(-t_ms / 8)) + 600 * (1 - exp(-t_ms / 60))
}

test_that("trace_to_ojip extracts documented points on a non-uniform grid", {
  t_ms <- c(0.01, 0.02, 0.03, 0.05, 0.1, 0.2, 0.27, 0.3, 0.5, 1, 2, 3, 5, 10, 20, 30, 50, 100, 200, 300, 500, 1000)
  y <- synthetic_trace(t_ms)
  y[t_ms > 300] <- max(y[t_ms <= 300])
  ojip <- trace_to_ojip(t_ms, y, time_unit = "ms", preset = "handypea")
  expect_equal(ojip$fo, y[t_ms == 0.02])
  expect_equal(ojip$f300us, y[t_ms == 0.27])
  expect_equal(ojip$j, y[t_ms == 2])
  expect_equal(ojip$i, y[t_ms == 30])
  expect_equal(ojip$t_f300us_ms, 0.27)
  expect_equal(attr(ojip, "extraction")$time_unit, "ms")
  sel <- t_ms >= 0.02 & t_ms <= ojip$t_fm_ms
  manual <- sum(diff(t_ms[sel]) * (head(ojip$fm - y[sel], -1) + tail(ojip$fm - y[sel], -1)) / 2)
  expect_equal(ojip$area, manual)
})

test_that("time units are inferred from s, ms, and us grids", {
  t_ms <- c(0.01, 0.02, 0.27, 2, 30, 300)
  y <- synthetic_trace(t_ms)
  for (u in list(c("s", 1e-3), c("ms", 1), c("us", 1e3))) {
    o <- trace_to_ojip(t_ms * as.numeric(u[2]), y, preset = "handypea", time_unit = "auto")
    expect_equal(attr(o, "extraction")$time_unit, u[1])
    expect_equal(o$j, y[4])
  }
})

test_that("points outside the tolerance window become NA and are flagged", {
  t_ms <- c(0.05, 0.3, 2, 30, 300)  # no 20 us and no 270 us point
  y <- synthetic_trace(t_ms)
  expect_warning(
    expect_warning(o <- trace_to_ojip(t_ms, y, time_unit = "ms", preset = "handypea"), "fo target"),
    "f300us target"
  )
  expect_true(is.na(o$fo))
  expect_true(is.na(o$f300us))
  expect_match(o$qc_flags, "fo_time_out_of_tolerance")
  res <- suppressWarnings(calc_fluorojip(o))
  expect_true(is.na(res$PI_abs))
  # A wider tolerance is an explicit, recorded choice
  o2 <- suppressWarnings(trace_to_ojip(t_ms, y, time_unit = "ms", preset = "handypea",
                                       tolerance_ms = c(fo = 0.05, f300us = 0.05)))
  expect_equal(o2$fo, y[1])
  expect_equal(attr(o2, "extraction")$tolerance_ms[["fo"]], 0.05)
})

test_that("truncated and multi-peak traces are flagged", {
  t_ms <- c(0.01, 0.02, 0.1, 0.27, 1, 2, 10, 30, 60, 100)
  trunc <- synthetic_trace(t_ms)                  # still rising at the last point
  spike <- synthetic_trace(t_ms); spike[6] <- 5000 # spike at 2 ms before I
  o <- trace_to_ojip(t_ms, rbind(trunc, spike), time_unit = "ms", preset = "handypea")
  expect_match(o$qc_flags[1], "fm_at_last_point")
  expect_match(o$qc_flags[2], "fm_before_i")
  expect_match(o$qc_flags[1], "few_points")
})

test_that("background subtraction and padded zero times are handled", {
  t_us <- c(11, 21, 31, 41, 51, 301, 2001, 30001, 300001, 0, 0)
  y <- c(200, synthetic_trace(t_us[2:9] / 1000) + 200, NA, NA)
  o <- trace_to_ojip(t_us, y, preset = "fluorpen")
  expect_equal(o$background, 200)
  expect_equal(o$fo, synthetic_trace(0.041))
  expect_equal(attr(o, "extraction")$n_discarded_time_points, 2)
})

test_that("noise does not change fixed-time extraction rules", {
  set.seed(1)
  t_ms <- c(0.01, 0.02, seq(0.03, 0.3, by = 0.01), seq(0.4, 3, by = 0.1), 4:30, seq(40, 300, by = 10))
  y <- synthetic_trace(t_ms) + stats::rnorm(length(t_ms), sd = 10)
  o <- trace_to_ojip(t_ms, y, time_unit = "ms", preset = "handypea")
  expect_equal(o$t_j_ms, 2)
  expect_equal(o$t_i_ms, 30)
  d <- trace_to_ojip(t_ms, y, time_unit = "ms", preset = "handypea", step_method = "derivative")
  expect_true(is.na(d$t_j_ms) || (d$t_j_ms >= 0.8 && d$t_j_ms <= 5))
})

test_that("legacy handypea names still work", {
  raw <- list(
    times_s = c(0.00002, 0.00027, 0.002, 0.03, 0.10),
    mat = matrix(c(100, 130, 220, 400, 600, 110, 140, 260, 430, 590), nrow = 2, byrow = TRUE,
                 dimnames = list(c("S1", "S2"), NULL))
  )
  ojip <- handypea_to_ojip(raw)
  expect_equal(ojip$k, c(130, 140))
  expect_equal(ojip$f300us, c(130, 140))
  expect_equal(ojip$t_fm, c(100, 100))
  expect_false(any(is.na(calc_fluorojip(ojip)$PI_abs)))
  expect_identical(read_handypea_csv, read_biolyzer_csv)
})

test_that("read_biolyzer_csv reads the documented layout", {
  f <- tempfile(fileext = ".csv")
  writeLines(c('"Record No",0.00002,0.00027,0.002,0.03,0.10',
               '"S1",280,340,700,950,1200', '"S2",300,360,730,980,1250', "End Of File"), f)
  raw <- read_biolyzer_csv(f)
  expect_equal(raw$ids, c("S1", "S2"))
  expect_equal(dim(raw$mat), c(2L, 5L))
  res <- calc_fluorojip_biolyzer(f)
  expect_equal(res$fo, c(280, 300))
  bad <- tempfile(fileext = ".csv")
  writeLines(c("a,b", "1,2"), bad)
  expect_error(read_biolyzer_csv(bad), "Record No")
})

# --- validation against software-exported values ------------------------------

test_that("equations agree with Biolyzer to the rounding of the export", {
  v <- validate_biolyzer(mode = "engine")
  m <- v$metrics
  ratio <- c("Fv_Fm", "Vj", "Vi", "Mo", "psi_Eo", "phi_Eo", "ABS_RC", "TRo_RC", "ETo_RC", "DIo_RC", "RC_ABS", "PI_abs", "Sm")
  expect_true(all(m$n[m$parameter %in% ratio] == 10))
  expect_true(all(m$max_abs_diff[m$parameter %in% ratio] < 1e-3))
  cs <- c("ABS_CSo", "TRo_CSo", "ETo_CSo", "DIo_CSo", "ABS_CSm", "TRo_CSm", "ETo_CSm", "DIo_CSm")
  expect_true(all(m$max_abs_diff[m$parameter %in% cs] <= 0.55))  # Biolyzer exports integers
})

test_that("Biolyzer importer reproduces Biolyzer K, J, I, and Fm", {
  v <- validate_biolyzer(mode = "importer")
  m <- v$metrics
  expect_equal(m$max_abs_diff[m$parameter %in% c("f300us", "j", "i")], c(0, 0, 0))
  expect_lte(m$max_abs_diff[m$parameter == "fm"], 1)
  expect_true(all(v$ojip$qc_flags == "ok"))
})

test_that("FluorPen preset reproduces FluorPen software values", {
  xlsx <- system.file("extdata", "FluorPen_test.xlsx", package = "fluorojip")
  skip_if_not(nzchar(xlsx))
  v <- validate_fluorpen(xlsx, mode = "importer")
  m <- v$metrics
  expect_equal(m$max_abs_diff[m$parameter %in% c("fo", "fm")], c(0, 0))
  ratio <- c("Fv_Fm", "Vj", "Vi", "Mo", "psi_Eo", "phi_Eo", "ABS_RC", "TRo_RC", "ETo_RC", "DIo_RC", "PI_abs")
  expect_true(all(m$n[m$parameter %in% ratio] == 106))
  expect_true(all(m$max_abs_diff[m$parameter %in% ratio] <= 5e-4 + 1e-9))
})

test_that("FluorPen decimal repair restores values that lost the decimal mark", {
  xlsx <- system.file("extdata", "FluorPen_test.xlsx", package = "fluorojip")
  skip_if_not(nzchar(xlsx))
  raw <- suppressMessages(read_fluorpen_xlsx(xlsx))
  s <- raw$summary_numeric
  expect_true(any(raw$summary_repaired))
  # internal consistency of repaired values with integer fields
  expect_lt(max(abs(s[["Sm"]] - s[["Area"]] / s[["Fv"]])), 1e-3)
  expect_lt(max(abs(s[["Fv/Fm"]] - s[["Fv"]] / s[["Fm"]])), 1e-3)
  expect_true(all(s[["Mo"]] < 3))
})

test_that("agreement_metrics computes the documented statistics", {
  a <- agreement_metrics(c(1, 2, 3, 4), c(1, 2, 3, 5), tolerance = 0.5)
  expect_equal(a$n, 4)
  expect_equal(a$bias, -0.25)
  expect_equal(a$mae, 0.25)
  expect_equal(a$max_abs_diff, 1)
  expect_equal(a$within_tol, 0.75)
})

test_that("bundled resources and app are available", {
  expect_true(file.exists(fluorojip_example_biolyzer_file()))
  app_dir <- system.file("shiny", "fluorojip-app", package = "fluorojip")
  expect_true(file.exists(file.path(app_dir, "app.R")))
})

test_that("Shiny app writes an analysis log that records settings", {
  skip_if_not_installed("shiny")
  skip_on_cran()
  app <- shiny::shinyAppDir(system.file("shiny", "fluorojip-app", package = "fluorojip"))
  shiny::testServer(app, {
    session$setInputs(preset = "fluorpen", step_method = "fixed", summary_sep = ",",
                      export_norm_method = "none", export_control_level = "control",
                      export_params = c("Fv_Fm", "PI_abs"))
    suppressMessages(session$setInputs(load_example_fluorpen = 1))
    log <- readLines(output$download_log)
    expect_true(any(grepl("^# input_md5: [0-9a-f]{32}$", log)))
    expect_true(any(grepl("^# preset: fluorpen", log)))
    expect_true(any(grepl("fluorpen_to_ojip\\(raw, preset = \"fluorpen\"", log)))
  })
})

test_that("plotting functions run on calculated results", {
  res <- calc_fluorojip(example_fluorojip)
  f <- tempfile(fileext = ".png")
  grDevices::png(f)
  on.exit(grDevices::dev.off(), add = TRUE)
  h <- plot_heatmap_fluorojip(res, params = c("Fv_Fm", "PI_abs", "ABS_RC", "DIo_RC"))
  expect_equal(h$zlim[1], -h$zlim[2])
  raw <- read_biolyzer_xls(fluorojip_example_biolyzer_file())
  o <- plot_ojip_trace(raw$times_ms, raw$mat, time_unit = "ms", preset = "biolyzer", samples = 1:2)
  expect_s3_class(o, "data.frame")
  skip_if_not_installed("scatterplot3d")
  expect_silent(plot_3d_fluorojip(res, params = c("Fv_Fm", "PI_abs", "ABS_RC")))
})
