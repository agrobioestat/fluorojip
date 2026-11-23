#' Agreement metrics between two numeric vectors
#'
#' Summarizes agreement between values calculated by fluorojip and reference
#' values (for example those exported by instrument software).
#'
#' @param x Numeric vector (for example fluorojip values).
#' @param y Numeric vector of reference values, same length as `x`.
#' @param tolerance Optional absolute tolerance; if supplied, the proportion of
#'   pairs with `|x - y| <= tolerance` is returned.
#' @return A one-row data frame with `n` (complete pairs), `bias` (mean of
#'   x - y), `mae`, `rmse`, `max_abs_diff`, `rel_mae_pct` (mean absolute
#'   difference relative to |y|, in percent), `pearson_r`, `lin_ccc` (Lin's
#'   concordance correlation coefficient), and `within_tol`.
#' @examples
#' agreement_metrics(c(1, 2, 3.01), c(1, 2, 3), tolerance = 0.005)
#' @export
agreement_metrics <- function(x, y, tolerance = NULL) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]
  y <- y[ok]
  n <- length(x)
  d <- x - y
  na <- NA_real_
  r <- if (n > 2 && stats::sd(x) > 0 && stats::sd(y) > 0) stats::cor(x, y) else na
  ccc <- if (n > 2) {
    sxy <- sum((x - mean(x)) * (y - mean(y))) / n
    2 * sxy / (stats::var(x) * (n - 1) / n + stats::var(y) * (n - 1) / n + (mean(x) - mean(y))^2)
  } else na
  data.frame(
    n = n,
    bias = if (n) mean(d) else na,
    mae = if (n) mean(abs(d)) else na,
    rmse = if (n) sqrt(mean(d^2)) else na,
    max_abs_diff = if (n) max(abs(d)) else na,
    rel_mae_pct = if (n) 100 * mean(abs(d)[y != 0] / abs(y[y != 0])) else na,
    pearson_r = r,
    lin_ccc = ccc,
    within_tol = if (!is.null(tolerance) && n) mean(abs(d) <= tolerance + 1e-12) else na
  )
}

# Build long comparison table and per-parameter metrics
compare_to_reference <- function(ours, ref, map, sample_id, tolerance = NULL) {
  rows <- lapply(names(map), function(p) {
    if (!p %in% names(ours) || !map[[p]] %in% names(ref)) return(NULL)
    data.frame(
      sample_id = sample_id,
      parameter = p,
      fluorojip = as.numeric(ours[[p]]),
      reference = suppressWarnings(as.numeric(as.character(ref[[map[[p]]]]))),
      stringsAsFactors = FALSE
    )
  })
  comp <- do.call(rbind, rows)
  comp$difference <- comp$fluorojip - comp$reference
  metrics <- do.call(rbind, lapply(split(comp, factor(comp$parameter, levels = unique(comp$parameter))), function(d) {
    tol <- if (is.null(tolerance)) NULL else tolerance[[d$parameter[1]]]
    cbind(parameter = d$parameter[1], agreement_metrics(d$fluorojip, d$reference, tol), stringsAsFactors = FALSE)
  }))
  rownames(metrics) <- NULL
  list(comparison = comp, metrics = metrics)
}

#' Validate fluorojip against parameters exported by Biolyzer
#'
#' Compares fluorojip with the `JIP Parameters` sheet of a Biolyzer Excel
#' export (see [read_biolyzer_xls()]).
#'
#' @param file Path to a Biolyzer workbook; defaults to the bundled example.
#' @param mode `"engine"` feeds the fluorescence values selected by Biolyzer
#'   (Fo, F(K), F(J), F(I), Fm, area) to [calc_fluorojip()] and therefore tests
#'   only the equations. `"importer"` extracts the values from the `Trace data`
#'   sheet with [biolyzer_to_ojip()] and tests extraction plus equations.
#' @param ... Further arguments passed to [trace_to_ojip()] in importer mode.
#' @return A list with `comparison` (long table: sample, parameter,
#'   fluorojip, reference, difference), `metrics` (one row per parameter, see
#'   [agreement_metrics()]), and `ojip` (the summary table used as input).
#' @examples
#' v <- validate_biolyzer(mode = "engine")
#' v$metrics[, c("parameter", "n", "mae", "max_abs_diff")]
#' @export
validate_biolyzer <- function(file = fluorojip_example_biolyzer_file(),
                              mode = c("engine", "importer"), ...) {
  mode <- match.arg(mode)
  raw <- read_biolyzer_xls(file)
  jip <- raw$jip
  if (is.null(jip)) stop("The workbook has no 'JIP Parameters' sheet.", call. = FALSE)
  num <- function(nm) suppressWarnings(as.numeric(as.character(jip[[nm]])))
  ids <- as.character(jip[["0 Trace Name"]])

  if (mode == "engine") {
    ojip <- data.frame(
      sample_id = ids, fo = num("22 Fo"), f300us = num("13 F(K)"),
      j = num("15 F(J)"), i = num("17 F(I)"), fm = num("23 Fm"),
      area = num("32 Area max"), stringsAsFactors = FALSE
    )
  } else {
    ojip <- biolyzer_to_ojip(raw, ...)
    ojip <- ojip[match(ids, ojip$sample_id), , drop = FALSE]
  }
  res <- calc_fluorojip(ojip, digits = NULL)

  map <- c(
    fo = "22 Fo", f300us = "13 F(K)", j = "15 F(J)", i = "17 F(I)", fm = "23 Fm",
    area = "32 Area max",
    Fv_Fm = "39 Fv/Fm", Vj = "42 V(J)", Vi = "43 V(I)", Mo = "51 Mo",
    psi_Eo = "45 Psi Eo", phi_Eo = "46 Phi Eo", phi_Do = "47 Phi Do",
    Sm = "55 Sm", N = "57 N", ABS_RC = "68 ABS/RC", TRo_RC = "69 Tro/RC",
    ETo_RC = "70 ETo/RC", DIo_RC = "71 DIo/RC", RC_ABS = "88 RC/ABS",
    ABS_CSo = "74 ABS/CSo", TRo_CSo = "75 TRo/CSo", ETo_CSo = "76 ETo/CSo",
    DIo_CSo = "77 DIo/CSo", RC_CSo = "73 RC/CSo", ABS_CSm = "80 ABS/CSm",
    TRo_CSm = "81 TRo/CSm", ETo_CSm = "82 ETo/CSm", DIo_CSm = "83 DIo/CSm",
    PI_abs = "101 PI(abs)1"
  )
  if (mode == "engine") map <- map[!names(map) %in% c("fo", "f300us", "j", "i", "fm", "area")]
  out <- compare_to_reference(res, jip, map, ids)
  out$ojip <- ojip
  out
}

#' Validate fluorojip against parameters exported by the FluorPen software
#'
#' Compares fluorojip with the footer parameters of a FluorPen workbook (see
#' [read_fluorpen_xlsx()]).
#'
#' @param file Path to a FluorPen workbook; defaults to the bundled example.
#' @param mode `"engine"` reconstructs the fluorescence values used by the
#'   FluorPen software from its exported footer (Fo = Fm - Fv,
#'   Fj = Fo + Vj Fv, Fi = Fo + Vi Fv, F300us = Fo + Mo Fv / 4) and therefore
#'   tests only the equations (agreement is limited by the three-decimal
#'   rounding of the exported ratios). `"importer"` extracts the values from
#'   the exported traces with [fluorpen_to_ojip()] and tests extraction plus
#'   equations.
#' @param ... Further arguments passed to [trace_to_ojip()] in importer mode,
#'   for example `preset = "handypea"` to reproduce the 0.1.x behaviour.
#' @return A list with `comparison`, `metrics`, and `ojip` (see
#'   [validate_biolyzer()]).
#' @examples
#' xlsx <- system.file("extdata", "FluorPen_test.xlsx", package = "fluorojip")
#' if (nzchar(xlsx)) {
#'   v <- validate_fluorpen(xlsx, mode = "importer")
#'   v$metrics[, c("parameter", "n", "mae", "max_abs_diff")]
#' }
#' @export
validate_fluorpen <- function(file = system.file("extdata", "FluorPen_test.xlsx", package = "fluorojip"),
                              mode = c("engine", "importer"), ...) {
  mode <- match.arg(mode)
  raw <- suppressMessages(read_fluorpen_xlsx(file))
  v <- raw$summary_numeric
  fm <- v[["Fm"]]
  fv <- v[["Fv"]]
  fo <- fm - fv
  ref <- v
  ref$fo_ref <- fo
  ref$f300us_ref <- fo + v[["Mo"]] * fv / 4
  ref$j_ref <- fo + v[["Vj"]] * fv
  ref$i_ref <- fo + v[["Vi"]] * fv

  if (mode == "engine") {
    ojip <- data.frame(
      sample_id = raw$sample_id, fo = fo, f300us = ref$f300us_ref,
      j = ref$j_ref, i = ref$i_ref, fm = fm, area = v[["Area"]],
      stringsAsFactors = FALSE
    )
  } else {
    ojip <- fluorpen_to_ojip(raw, ...)
  }
  res <- calc_fluorojip(ojip, digits = NULL)

  map <- c(
    fo = "fo_ref", f300us = "f300us_ref", j = "j_ref", i = "i_ref", fm = "Fm",
    Fv_Fm = "Fv/Fm", Vj = "Vj", Vi = "Vi", Mo = "Mo", psi_Eo = "Psi_o",
    phi_Eo = "Phi_Eo", phi_Do = "Phi_Do", Sm = "Sm", N = "N",
    ABS_RC = "ABS/RC", TRo_RC = "TRo/RC", ETo_RC = "ETo/RC",
    DIo_RC = "DIo/RC", PI_abs = "Pi_Abs"
  )
  if (mode == "engine") map <- map[!names(map) %in% c("fo", "f300us", "j", "i", "fm")]
  out <- compare_to_reference(res, ref, map, raw$sample_id)
  out$ojip <- ojip
  out
}
