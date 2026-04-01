test_that("calc_fluorojip uses standard K-step based JIP-test formulas", {
  df <- data.frame(
    sample_id = "S1",
    fo = 100,
    k = 130,
    j = 220,
    i = 400,
    fm = 600,
    area = 15000
  )

  res <- calc_fluorojip(df)

  expect_equal(res$Fv, 500)
  expect_equal(res$Fv_Fm, 0.833333, tolerance = 1e-6)
  expect_equal(res$Vj, 0.24, tolerance = 1e-6)
  expect_equal(res$Vi, 0.6, tolerance = 1e-6)
  expect_equal(res$Mo, 0.24, tolerance = 1e-6)
  expect_equal(res$TRo_RC, 1, tolerance = 1e-6)
  expect_equal(res$ABS_RC, 1.2, tolerance = 1e-6)
  expect_equal(res$ETo_RC, 0.76, tolerance = 1e-6)
  expect_equal(res$DIo_RC, 0.2, tolerance = 1e-6)
  expect_equal(res$PI_abs, 13.194444, tolerance = 1e-6)
})

test_that("PI_abs-dependent terms are NA when K-step data are unavailable", {
  df <- data.frame(
    sample_id = "S1",
    fo = 100,
    j = 220,
    i = 400,
    fm = 600,
    area = 15000
  )

  expect_warning(res <- calc_fluorojip(df), "Missing K-step / F300")
  expect_true(is.na(res$Mo))
  expect_true(is.na(res$TRo_RC))
  expect_true(is.na(res$ABS_RC))
  expect_true(is.na(res$PI_abs))
})

test_that("handypea_to_ojip returns K-step and time-to-Fm", {
  raw <- list(
    times_s = c(0.00005, 0.00030, 0.00200, 0.03000, 0.10000),
    mat = matrix(
      c(
        100, 130, 220, 400, 600,
        110, 140, 260, 430, 590
      ),
      nrow = 2,
      byrow = TRUE,
      dimnames = list(c("S1", "S2"), NULL)
    )
  )

  ojip <- handypea_to_ojip(raw)
  res <- calc_fluorojip(ojip)

  expect_true(all(c("t_fm", "k", "fo", "fm", "j", "i", "area") %in% names(ojip)))
  expect_equal(ojip$k, c(130, 140))
  expect_equal(ojip$t_fm, c(100, 100))
  expect_false(any(is.na(res$PI_abs)))
})

test_that("handypea_to_ojip handles millisecond-based raw traces", {
  raw <- list(
    times_s = c(0.01, 0.02, 0.27, 2.00, 30.00, 230.00),
    mat = matrix(
      c(
        1374, 1332, 2030, 2450, 3136, 3224,
        1079, 1048, 1935, 2745, 4028, 4073
      ),
      nrow = 2,
      byrow = TRUE,
      dimnames = list(c("S1", "S2"), NULL)
    )
  )

  ojip <- handypea_to_ojip(raw)

  expect_equal(ojip$k, c(2030, 1935))
  expect_equal(ojip$j, c(2450, 2745))
  expect_equal(ojip$i, c(3136, 4028))
  expect_equal(ojip$fo, c(1332, 1048))
  expect_equal(ojip$t_fm, c(230, 230))
})

test_that("fluorojip_example_biolyzer_file returns the bundled workbook path", {
  x <- fluorojip_example_biolyzer_file()

  expect_true(file.exists(x))
  expect_equal(basename(x), "OJIPExporttoExcelTest001-01062024at20h31.xls")
})

test_that("handypea_to_ojip handles microsecond-based raw traces", {
  raw <- list(
    times_s = c(11, 21, 271, 2001, 30001, 230001),
    mat = matrix(
      c(
        1374, 1332, 2030, 2450, 3136, 3224,
        1079, 1048, 1935, 2745, 4028, 4073
      ),
      nrow = 2,
      byrow = TRUE,
      dimnames = list(c("S1", "S2"), NULL)
    )
  )

  ojip <- handypea_to_ojip(raw)

  expect_equal(ojip$fo, c(1332, 1048))
  expect_equal(ojip$k, c(2030, 1935))
  expect_equal(ojip$j, c(2450, 2745))
  expect_equal(ojip$i, c(3136, 4028))
  expect_equal(ojip$t_fm, c(230.001, 230.001))
})

test_that("read_fluorpen_xlsx parses the project-local FluorPen format", {
  xlsx <- file.path("D:/FLUOROJIP/fluorpen", "FluorPen - test 2.xlsx")
  skip_if_not(file.exists(xlsx))

  raw <- read_fluorpen_xlsx(xlsx)

  expect_true(length(raw$times_us) > 500)
  expect_equal(raw$times_us[1], 11)
  expect_equal(raw$times_us[which.min(abs(raw$times_us - 270))], 271)
  expect_true(all(c("Fv/Fm", "Mo", "Pi_Abs", "ABS/RC", "ETo/RC", "DIo/RC") %in% names(raw$summary_numeric)))
  expect_equal(nrow(raw$mat), length(raw$sample_id))
})

test_that("run_fluorojip_app can find the bundled Shiny app", {
  app_dir <- system.file("shiny", "fluorojip-app", package = "fluorojip")

  expect_true(nzchar(app_dir))
  expect_true(dir.exists(app_dir))
  expect_true(file.exists(file.path(app_dir, "app.R")))
})
