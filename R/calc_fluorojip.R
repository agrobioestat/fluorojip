#' Compute chlorophyll a fluorescence OJIP / JIP-test parameters
#'
#' Computes chlorophyll a fluorescence OJIP / JIP-test parameters from
#' fluorescence summary data.
#'
#' @param df A data frame containing fluorescence summary columns. At minimum,
#'   provide Fo (`fo` or `o`), Fm (`fm` or `p`), J (`j` or `fj`), and I (`i`
#'   or `fi`). Reliable `Mo`, `N`, `PI_abs`, and RC-based fluxes require a
#'   K-step / 300 us-equivalent measurement supplied as `k`, `fk`, `f300`, or
#'   `f300us`.
#' @return A data frame containing the original input columns plus calculated
#'   FluorOJIP / JIP-test parameters.
#' @details
#' The current implementation follows a summary-input workflow. Primary terms
#' such as `phi_Po`, `Vj`, `Vi`, `psi_Eo`, `phi_Eo`, `Mo`, and `PI_abs` are
#' computed explicitly from the supplied fluorescence summary values.
#'
#' `Mo` is derived from the K-step / F300 region using the standard relation
#' `4 * (F300 - Fo) / (Fm - Fo)`. If a K-step / 300 us-equivalent value is not
#' available, the function does not attempt to guess `Mo`; instead, `Mo`, `N`,
#' RC-based fluxes, and `PI_abs` are returned as `NA` with a warning.
#'
#' The cross-section outputs `ABS_CSm`, `TRo_CSm`, `ETo_CSm`, and `DIo_CSm`
#' are returned as operational package outputs for internal comparison. They are
#' computed consistently from the supplied summary data, but they should be
#' described carefully if compared against instrument-specific phenomenological
#' flux conventions.
#' @export
calc_fluorojip <- function(df) {

  # 1) Helpers
  safe_div <- function(a, b) {
    a <- as.numeric(a)
    b <- as.numeric(b)
    ifelse(!is.na(a) & !is.na(b) & b != 0, a / b, NA_real_)
  }
  r6 <- function(x) if (is.numeric(x)) round(x, 6) else x
  col_or_null <- function(choices) {
    hit <- choices[choices %in% names(df)][1]
    if (is.na(hit)) {
      return(NULL)
    }
    as.numeric(df[[hit]])
  }
  warn_bad_rows <- function(label, idx, detail) {
    if (length(idx) > 0) {
      warning(
        sprintf("%s invalid for %d row(s); %s.", label, length(idx), detail),
        call. = FALSE
      )
    }
  }

  # 2) Input resolution
  Fo <- col_or_null(c("fo", "o"))
  Fm <- col_or_null(c("fm", "p"))
  Fj <- col_or_null(c("fj", "j"))
  Fi <- col_or_null(c("fi", "i"))
  F300 <- col_or_null(c("k", "fk", "f300", "f300us", "f_300us", "f_300_us"))
  area <- col_or_null("area")

  if (is.null(Fo)) stop("Missing Fo. Provide `fo` or `o`.")
  if (is.null(Fm)) stop("Missing Fm. Provide `fm` or `p`.")
  if (is.null(Fj)) stop("Missing Fj. Provide `j` or `fj`.")
  if (is.null(Fi)) stop("Missing Fi. Provide `i` or `fi`.")

  # 3) Basic fluorescence levels
  Fv <- Fm - Fo
  n <- nrow(df)

  base_ok <- is.finite(Fo) & is.finite(Fm) & Fm > Fo & Fo >= 0
  warn_bad_rows("Fo/Fm pair", which(!base_ok), "affected parameters were set to NA")

  j_ok <- base_ok & is.finite(Fj) & Fj >= Fo & Fj <= Fm
  i_ok <- base_ok & is.finite(Fi) & Fi >= Fo & Fi <= Fm
  warn_bad_rows("Fj values", which(!j_ok), "Vj and parameters depending on Vj were set to NA")
  warn_bad_rows("Fi values", which(!i_ok), "Vi and parameters depending on Vi were set to NA")

  # 4) Primary yields and relative variable fluorescence
  phi_Po <- ifelse(base_ok, safe_div(Fv, Fm), NA_real_)
  Vj <- ifelse(j_ok, safe_div(Fj - Fo, Fv), NA_real_)
  Vi <- ifelse(i_ok, safe_div(Fi - Fo, Fv), NA_real_)

  # 5) Standard JIP-test quantities
  # Mo is defined from the first 250 us of the rise:
  # Mo ~ 4 * (F300us - Fo) / (Fm - Fo)
  if (is.null(F300)) {
    warning(
      "Missing K-step / F300 data; Mo, N, RC-based fluxes, and PI_abs were set to NA. Provide `k` or `f300us` for standard JIP-test calculations.",
      call. = FALSE
    )
    Mo <- rep(NA_real_, n)
  } else {
    k_ok <- base_ok & is.finite(F300) & F300 >= Fo & F300 <= Fm
    warn_bad_rows("K-step / F300 values", which(!k_ok), "Mo, RC-based fluxes, and PI_abs were set to NA")
    Mo <- ifelse(k_ok, 4 * safe_div(F300 - Fo, Fv), NA_real_)
  }

  psi_Eo <- ifelse(!is.na(Vj), 1 - Vj, NA_real_)
  phi_Eo <- phi_Po * psi_Eo

  # 6) Structural / turnover terms
  if (is.null(area)) {
    Sm <- rep(NA_real_, n)
  } else {
    area_ok <- base_ok & is.finite(area) & area >= 0
    warn_bad_rows("Area values", which(!area_ok), "Sm and N were set to NA")
    Sm <- ifelse(area_ok, safe_div(area, Fv), NA_real_)
  }

  # 7) Fluxes per reaction center (RC)
  TRo_RC <- ifelse(!is.na(Mo) & !is.na(Vj) & Vj > 0, safe_div(Mo, Vj), NA_real_)
  ABS_RC <- safe_div(TRo_RC, phi_Po)
  ETo_RC <- TRo_RC * psi_Eo
  DIo_RC <- ABS_RC - TRo_RC
  N <- Sm * TRo_RC

  # 8) Fluxes per cross-section (CS)
  # These are operational package outputs derived from the supplied summary
  # data and are useful for internal comparison across samples.
  ABS_CSm <- Fm
  TRo_CSm <- phi_Po * ABS_CSm
  ETo_CSm <- phi_Eo * ABS_CSm
  DIo_CSm <- ABS_CSm - TRo_CSm

  # 9) Performance Index (PI_abs)
  RC_ABS <- safe_div(1, ABS_RC)
  phi_Po_term <- safe_div(phi_Po, 1 - phi_Po)
  psi_Eo_term <- safe_div(psi_Eo, 1 - psi_Eo)
  PI_abs <- RC_ABS * phi_Po_term * psi_Eo_term

  # 10) kN / kP require additional rate-constant assumptions and are not
  # derivable from the summary input alone.
  Kn <- rep(NA_real_, n)
  Kp <- rep(NA_real_, n)

  # 11) Binding results
  res <- df
  res$Fv       <- r6(Fv)
  res$phi_Po   <- r6(phi_Po)
  res$Fv_Fm    <- res$phi_Po
  res$Vj       <- r6(Vj)
  res$Vi       <- r6(Vi)
  res$Mo       <- r6(Mo)
  res$Sm       <- r6(Sm)
  res$N        <- r6(N)
  res$ABS_RC   <- r6(ABS_RC)
  res$TRo_RC   <- r6(TRo_RC)
  res$ETo_RC   <- r6(ETo_RC)
  res$DIo_RC   <- r6(DIo_RC)
  res$phi_Eo   <- r6(phi_Eo)
  res$psi_Eo   <- r6(psi_Eo)
  res$ABS_CSm  <- r6(ABS_CSm)
  res$TRo_CSm  <- r6(TRo_CSm)
  res$ETo_CSm  <- r6(ETo_CSm)
  res$DIo_CSm  <- r6(DIo_CSm)
  res$PI_abs   <- r6(PI_abs)
  res$Kn       <- r6(Kn)
  res$Kp       <- r6(Kp)

  return(res)
}

#' Read a summary table and compute FluorOJIP parameters
#'
#' @param file Path to the input summary table.
#' @param sep Field separator used in the input file. Defaults to `";"`.
#' @param dec Decimal mark used in the input file. Defaults to `"."`.
#' @param ... Additional arguments passed to [utils::read.csv()].
#' @return A data frame containing the original input columns plus calculated
#'   FluorOJIP / JIP-test parameters.
#' @details
#' This is a convenience wrapper for summary-input workflows:
#' `read.csv(file, ...) -> calc_fluorojip(df)`.
#' @importFrom utils read.csv
#' @export
calc_fluorojip_file <- function(file, sep = ";", dec = ".", ...) {
  df <- utils::read.csv(
    file,
    sep = sep,
    dec = dec,
    header = TRUE,
    stringsAsFactors = FALSE,
    ...
  )
  calc_fluorojip(df)
}
