#' Compute chlorophyll a fluorescence OJIP / JIP-test parameters
#'
#' Computes JIP-test parameters from a table of fluorescence summary values
#' (one row per measurement). The function implements the equations listed in
#' the *Details* section exactly as written; it does not inspect raw traces,
#' and it never estimates a missing input from other transient points.
#'
#' @param df A data frame with one row per measurement. Required columns:
#'   minimum fluorescence Fo (`fo` or `o`), maximum fluorescence Fm (`fm` or
#'   `p`), fluorescence at the J step (`j` or `fj`) and at the I step (`i` or
#'   `fi`). Optional columns: the early O-J point used for the initial slope
#'   (preferred name `f300us`; accepted aliases `f300`, `f_300us`, `f_300_us`,
#'   `fk`, and the backward-compatible `k`) and the area above the transient
#'   (`area`). All other columns (sample identifiers, treatments, metadata) are
#'   returned unchanged.
#' @param mo_interval_ms Time interval, in milliseconds, between the Fo point
#'   and the early O-J point used for the initial slope `Mo`. The JIP-test
#'   convention uses 0.25 ms (for example Fo at 20 us with F at 270 us, or Fo
#'   at 50 us with F at 300 us), which gives the familiar factor 4 in
#'   `Mo = 4 (F300us - Fo) / (Fm - Fo)`. Change it only if the two points used
#'   in your data are separated by a different interval.
#' @param digits Number of decimal places used to round calculated columns.
#'   Use `NULL` to return unrounded values.
#' @return A data frame containing all input columns followed by the
#'   calculated parameters listed in *Details* and a `qc_flags` column. Rows
#'   whose inputs fail the validity checks receive `NA` for the affected
#'   parameters and a warning is issued.
#' @details
#' **Notation.** Fo, Fj, Fi, Fm are fluorescence intensities (instrument
#' units) at O, J, I, and P; F300us is the early O-J point (often called the
#' K-step position, but used here only for the initial slope); `area` is the
#' area between the transient and the Fm level from Fo to the time of Fm.
#'
#' **Equations implemented** (output column = formula):
#' \itemize{
#'   \item `Fv` = Fm - Fo
#'   \item `phi_Po` (= `Fv_Fm`) = Fv / Fm = TRo/ABS
#'   \item `phi_Do` = 1 - phi_Po = Fo / Fm
#'   \item `Vj` = (Fj - Fo) / Fv;  `Vi` = (Fi - Fo) / Fv
#'   \item `Mo` = ((F300us - Fo) / Fv) / `mo_interval_ms` (= 4 (F300us - Fo) / Fv
#'     with the default 0.25 ms), units ms^-1
#'   \item `psi_Eo` = 1 - Vj;  `phi_Eo` = phi_Po psi_Eo
#'   \item `Sm` = area / Fv (time units of `area`);  `N` = Sm Mo / Vj
#'   \item `TRo_RC` = Mo / Vj;  `ABS_RC` = TRo_RC / phi_Po;
#'     `ETo_RC` = TRo_RC psi_Eo;  `DIo_RC` = ABS_RC - TRo_RC
#'   \item `RC_ABS` = 1 / ABS_RC = phi_Po Vj / Mo
#'   \item `PI_abs` = RC_ABS (phi_Po / (1 - phi_Po)) (psi_Eo / (1 - psi_Eo))
#'   \item Cross-section fluxes at t = 0 (`*_CSo`): `ABS_CSo` = Fo,
#'     `TRo_CSo` = phi_Po Fo, `ETo_CSo` = phi_Eo Fo,
#'     `DIo_CSo` = ABS_CSo - TRo_CSo, `RC_CSo` = phi_Po (Vj / Mo) Fo
#'   \item Cross-section fluxes at t = tFm (`*_CSm`): `ABS_CSm` = Fm,
#'     `TRo_CSm` = phi_Po Fm, `ETo_CSm` = phi_Eo Fm,
#'     `DIo_CSm` = ABS_CSm - TRo_CSm
#' }
#' The specific fluxes per reaction centre are dimensionless ratios in the
#' JIP-test framework and `PI_abs` is dimensionless. Cross-section fluxes use
#' Fo or Fm as a proxy for the absorption flux per excited cross section and
#' therefore carry the arbitrary units of the instrument signal: they can be
#' compared only among samples measured with the same instrument and settings.
#'
#' **Missing and invalid inputs.**
#' \itemize{
#'   \item Missing required columns (Fo, Fm, Fj, Fi) stop the function.
#'   \item If no F300us column is supplied, `Mo`, `N`, all RC-based fluxes,
#'     `RC_ABS`, `RC_CSo`, and `PI_abs` are returned as `NA` with a warning.
#'     They are never estimated from later points.
#'   \item Rows with non-finite values, Fo < 0, or Fm <= Fo receive `NA` for
#'     all derived parameters. Fj, Fi, or F300us outside the interval
#'     \[Fo, Fm\] make the parameters that depend on them `NA`.
#'   \item If `area` is absent, `Sm` and `N` are `NA`.
#' }
#'
#' **Quality flags.** The `qc_flags` column contains `"ok"` or a
#' semicolon-separated list of: `fo_fm_invalid`, `fj_out_of_range`,
#' `fi_out_of_range`, `f300us_out_of_range`, `f300us_missing`,
#' `non_monotonic` (Fo <= F300us <= Fj <= Fi <= Fm violated), and any flags
#' already present in an input `qc_flags` column (for example those produced by
#' [trace_to_ojip()]). Flags describe the numerical inputs only; they cannot
#' establish that the measurement itself (dark adaptation, saturating
#' excitation, absence of artefacts) was adequate.
#'
#' Equations follow Strasser et al. (2000, 2004) and the terminology of
#' Tsimilli-Michael (2020).
#'
#' @references
#' Strasser RJ, Srivastava A, Tsimilli-Michael M (2000). The fluorescence
#' transient as a tool to characterize and screen photosynthetic samples. In:
#' Yunus M, Pathre U, Mohanty P (eds) Probing Photosynthesis: Mechanisms,
#' Regulation and Adaptation. Taylor and Francis, London, pp 445-483.
#'
#' Strasser RJ, Tsimilli-Michael M, Srivastava A (2004). Analysis of the
#' chlorophyll a fluorescence transient. In: Papageorgiou GC, Govindjee (eds)
#' Chlorophyll a Fluorescence: A Signature of Photosynthesis. Springer,
#' Dordrecht, pp 321-362. \doi{10.1007/978-1-4020-3218-9_12}
#'
#' Tsimilli-Michael M (2020). Revisiting JIP-test: an educative review on
#' concepts, assumptions, approximations, definitions and terminology.
#' Photosynthetica 58:275-292. \doi{10.32615/ps.2019.150}
#' @examples
#' df <- data.frame(
#'   sample_id = c("S1", "S2"),
#'   fo = c(280, 300),
#'   f300us = c(340, 360),
#'   j = c(700, 730),
#'   i = c(950, 980),
#'   fm = c(1200, 1250),
#'   area = c(32000, 35000)
#' )
#'
#' res <- calc_fluorojip(df)
#' res[, c("sample_id", "Fv_Fm", "Mo", "ABS_RC", "PI_abs", "qc_flags")]
#'
#' # Without the early O-J point, slope-dependent outputs are NA (with a warning)
#' res2 <- suppressWarnings(calc_fluorojip(df[, c("sample_id", "fo", "j", "i", "fm")]))
#' res2[, c("sample_id", "Fv_Fm", "Mo", "PI_abs", "qc_flags")]
#' @export
calc_fluorojip <- function(df, mo_interval_ms = 0.25, digits = 6) {

  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.", call. = FALSE)
  }
  if (!is.numeric(mo_interval_ms) || length(mo_interval_ms) != 1L ||
      !is.finite(mo_interval_ms) || mo_interval_ms <= 0) {
    stop("`mo_interval_ms` must be a single positive number.", call. = FALSE)
  }

  # 1) Helpers --------------------------------------------------------------
  safe_div <- function(a, b) {
    a <- as.numeric(a)
    b <- as.numeric(b)
    ifelse(is.finite(a) & is.finite(b) & b != 0, a / b, NA_real_)
  }
  rnd <- function(x) if (is.null(digits) || !is.numeric(x)) x else round(x, digits)
  col_or_null <- function(choices) {
    hit <- choices[choices %in% names(df)][1]
    if (is.na(hit)) {
      return(NULL)
    }
    suppressWarnings(as.numeric(df[[hit]]))
  }
  warn_bad_rows <- function(label, idx, detail) {
    if (length(idx) > 0) {
      warning(
        sprintf("%s invalid for %d row(s); %s.", label, length(idx), detail),
        call. = FALSE
      )
    }
  }

  # 2) Input resolution -----------------------------------------------------
  Fo <- col_or_null(c("fo", "o"))
  Fm <- col_or_null(c("fm", "p"))
  Fj <- col_or_null(c("fj", "j"))
  Fi <- col_or_null(c("fi", "i"))
  F300 <- col_or_null(c("f300us", "f300", "f_300us", "f_300_us", "fk", "k"))
  area <- col_or_null("area")

  if (is.null(Fo)) stop("Missing Fo. Provide `fo` or `o`.", call. = FALSE)
  if (is.null(Fm)) stop("Missing Fm. Provide `fm` or `p`.", call. = FALSE)
  if (is.null(Fj)) stop("Missing Fj. Provide `j` or `fj`.", call. = FALSE)
  if (is.null(Fi)) stop("Missing Fi. Provide `i` or `fi`.", call. = FALSE)

  n <- nrow(df)
  flags <- vector("list", n)
  add_flag <- function(idx, flag) {
    for (r in idx) flags[[r]] <<- c(flags[[r]], flag)
  }

  # 3) Validity checks ------------------------------------------------------
  Fv <- Fm - Fo
  base_ok <- is.finite(Fo) & is.finite(Fm) & Fm > Fo & Fo >= 0
  warn_bad_rows("Fo/Fm pair", which(!base_ok), "all derived parameters were set to NA")
  add_flag(which(!base_ok), "fo_fm_invalid")

  j_ok <- base_ok & is.finite(Fj) & Fj >= Fo & Fj <= Fm
  i_ok <- base_ok & is.finite(Fi) & Fi >= Fo & Fi <= Fm
  warn_bad_rows("Fj values", which(base_ok & !j_ok), "Vj and parameters depending on Vj were set to NA")
  warn_bad_rows("Fi values", which(base_ok & !i_ok), "Vi was set to NA")
  add_flag(which(base_ok & !j_ok), "fj_out_of_range")
  add_flag(which(base_ok & !i_ok), "fi_out_of_range")

  if (is.null(F300)) {
    warning(
      "No F300us-equivalent column (`f300us` or alias `k`); Mo, N, RC-based fluxes, RC_ABS, RC_CSo, and PI_abs were set to NA.",
      call. = FALSE
    )
    k_ok <- rep(FALSE, n)
    F300 <- rep(NA_real_, n)
    add_flag(seq_len(n), "f300us_missing")
  } else {
    k_ok <- base_ok & is.finite(F300) & F300 >= Fo & F300 <= Fm
    warn_bad_rows("F300us values", which(base_ok & !k_ok), "Mo, RC-based fluxes, and PI_abs were set to NA")
    add_flag(which(base_ok & !k_ok & is.finite(F300)), "f300us_out_of_range")
    add_flag(which(base_ok & !is.finite(F300)), "f300us_missing")
  }

  ordered_k <- Fo <= F300 & F300 <= Fj & Fj <= Fi & Fi <= Fm
  ordered_nok <- Fo <= Fj & Fj <= Fi & Fi <= Fm
  mono_ok <- ifelse(is.finite(F300), ordered_k, ordered_nok)
  add_flag(which(base_ok & j_ok & i_ok & !is.na(mono_ok) & !mono_ok), "non_monotonic")

  # 4) Yields and relative variable fluorescence ---------------------------
  phi_Po <- ifelse(base_ok, safe_div(Fv, Fm), NA_real_)
  phi_Do <- 1 - phi_Po
  Vj <- ifelse(j_ok, safe_div(Fj - Fo, Fv), NA_real_)
  Vi <- ifelse(i_ok, safe_div(Fi - Fo, Fv), NA_real_)
  Mo <- ifelse(k_ok, safe_div(F300 - Fo, Fv) / mo_interval_ms, NA_real_)
  psi_Eo <- 1 - Vj
  phi_Eo <- phi_Po * psi_Eo

  # 5) Area-derived terms ---------------------------------------------------
  if (is.null(area)) {
    Sm <- rep(NA_real_, n)
  } else {
    area_ok <- base_ok & is.finite(area) & area >= 0
    warn_bad_rows("Area values", which(base_ok & !area_ok), "Sm and N were set to NA")
    Sm <- ifelse(area_ok, safe_div(area, Fv), NA_real_)
  }

  # 6) Specific fluxes per reaction centre ---------------------------------
  TRo_RC <- ifelse(is.finite(Vj) & Vj > 0, safe_div(Mo, Vj), NA_real_)
  ABS_RC <- safe_div(TRo_RC, phi_Po)
  ETo_RC <- TRo_RC * psi_Eo
  DIo_RC <- ABS_RC - TRo_RC
  RC_ABS <- safe_div(1, ABS_RC)
  N <- Sm * TRo_RC

  # 7) Phenomenological fluxes per excited cross section -------------------
  ABS_CSo <- ifelse(base_ok, Fo, NA_real_)
  TRo_CSo <- phi_Po * ABS_CSo
  ETo_CSo <- phi_Eo * ABS_CSo
  DIo_CSo <- ABS_CSo - TRo_CSo
  RC_CSo <- phi_Po * safe_div(Vj, Mo) * ABS_CSo

  ABS_CSm <- ifelse(base_ok, Fm, NA_real_)
  TRo_CSm <- phi_Po * ABS_CSm
  ETo_CSm <- phi_Eo * ABS_CSm
  DIo_CSm <- ABS_CSm - TRo_CSm

  # 8) Performance index ----------------------------------------------------
  PI_abs <- RC_ABS * safe_div(phi_Po, 1 - phi_Po) * safe_div(psi_Eo, 1 - psi_Eo)

  # 9) Assemble -------------------------------------------------------------
  res <- df
  out <- list(
    Fv = ifelse(base_ok, Fv, NA_real_), phi_Po = phi_Po, Fv_Fm = phi_Po,
    phi_Do = phi_Do, Vj = Vj, Vi = Vi, Mo = Mo, psi_Eo = psi_Eo,
    phi_Eo = phi_Eo, Sm = Sm, N = N, ABS_RC = ABS_RC, TRo_RC = TRo_RC,
    ETo_RC = ETo_RC, DIo_RC = DIo_RC, RC_ABS = RC_ABS, ABS_CSo = ABS_CSo,
    TRo_CSo = TRo_CSo, ETo_CSo = ETo_CSo, DIo_CSo = DIo_CSo, RC_CSo = RC_CSo,
    ABS_CSm = ABS_CSm, TRo_CSm = TRo_CSm, ETo_CSm = ETo_CSm,
    DIo_CSm = DIo_CSm, PI_abs = PI_abs
  )
  for (nm in names(out)) res[[nm]] <- rnd(out[[nm]])

  prior <- if ("qc_flags" %in% names(df)) as.character(df$qc_flags) else rep("ok", n)
  res$qc_flags <- vapply(seq_len(n), function(r) {
    old <- if (is.na(prior[r]) || prior[r] %in% c("", "ok")) character(0) else strsplit(prior[r], ";", fixed = TRUE)[[1]]
    all_flags <- unique(c(old, flags[[r]]))
    if (length(all_flags) == 0) "ok" else paste(all_flags, collapse = ";")
  }, character(1))

  attr(res, "fluorojip") <- list(
    version = fluorojip_version(),
    mo_interval_ms = mo_interval_ms,
    f300us_supplied = any(is.finite(F300))
  )
  res
}

# Package version string (robust when functions are sourced outside the package)
fluorojip_version <- function() {
  tryCatch(as.character(utils::packageVersion("fluorojip")), error = function(e) NA_character_)
}

#' Read a summary table and compute JIP-test parameters
#'
#' Convenience wrapper: `utils::read.csv(file, ...)` followed by
#' [calc_fluorojip()].
#'
#' @param file Path to the input summary table.
#' @param sep Field separator used in the input file. Defaults to `";"`.
#' @param dec Decimal mark used in the input file. Defaults to `"."`.
#' @param ... Additional arguments passed to [calc_fluorojip()] (for example
#'   `mo_interval_ms`).
#' @return The data frame returned by [calc_fluorojip()].
#' @importFrom utils read.csv
#' @examples
#' df <- data.frame(
#'   sample_id = c("S1", "S2"),
#'   fo = c(280, 300),
#'   f300us = c(340, 360),
#'   j = c(700, 730),
#'   i = c(950, 980),
#'   fm = c(1200, 1250),
#'   area = c(32000, 35000)
#' )
#'
#' f <- tempfile(fileext = ".csv")
#' utils::write.csv(df, f, row.names = FALSE)
#'
#' res <- calc_fluorojip_file(f, sep = ",")
#' res[, c("sample_id", "Fv_Fm", "PI_abs")]
#' @export
calc_fluorojip_file <- function(file, sep = ";", dec = ".", ...) {
  df <- utils::read.csv(
    file,
    sep = sep,
    dec = dec,
    header = TRUE,
    stringsAsFactors = FALSE
  )
  calc_fluorojip(df, ...)
}
