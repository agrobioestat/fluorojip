#' Compute chlorophyll a OJIP parameters
#'
#' Compute chlorophyll a fluorescence OJIP / JIP-test parameters
#' from raw fluorescence summary data with noise protection.
#'
#' @param df A data frame with at least: t_fm, area, fo (or o), fm (or p), j, i, s.
#' @return A data frame with calculated OJIP parameters.
#' @export
calc_fluorojip <- function(df) {

  # 1) Input validation
  required <- c("t_fm", "area", "j", "i", "s")
  missing_min <- setdiff(required, names(df))
  if (length(missing_min) > 0) {
    stop("Missing columns: ", paste(missing_min, collapse = ", "))
  }

  if (!("fo" %in% names(df)) && !("o" %in% names(df))) stop("Missing Fo.")
  if (!("fm" %in% names(df)) && !("p" %in% names(df))) stop("Missing Fm.")

  # 2) Helpers
  safe_div <- function(a, b) {
    a <- as.numeric(a)
    b <- as.numeric(b)
    ifelse(!is.na(a) & !is.na(b) & b != 0, a / b, NA_real_)
  }
  r6 <- function(x) if (is.numeric(x)) round(x, 6) else x

  # 3) Basic fluorescence levels
  Fo <- if ("fo" %in% names(df)) df[["fo"]] else df[["o"]]
  Fm <- if ("fm" %in% names(df)) df[["fm"]] else df[["p"]]
  Fv <- Fm - Fo

  # 4) Noise Protection Logic
  # We ensure Fj and Fi are at least slightly above Fo to prevent negative Vj/Vi
  Fj <- pmax(df[["j"]], Fo + (0.001 * Fv))
  Fi <- pmax(df[["i"]], Fo + (0.002 * Fv))
  Fs <- df[["s"]]

  # 5) Primary yields and relative variable fluorescence
  phi_Po <- safe_div(Fv, Fm)
  Vj <- safe_div(Fj - Fo, Fv)
  Vi <- safe_div(Fi - Fo, Fv)

  # 6) Initial slope (Mo)
  # Approx time for J is 2ms, but we use the standardized slope calculation
  Mo <- 4 * safe_div(Fj - Fo, Fv) # Standardized for 4 steps

  # 7) Structural / turnover terms
  Sm <- safe_div(df[["area"]], Fv)
  N  <- safe_div(Sm * Mo, Vj)

  # 8) Fluxes per reaction center (RC)
  # ABS/RC = Mo/Vj / phi_Po
  ABS_RC <- safe_div(safe_div(Mo, Vj), phi_Po)
  TRo_RC <- ABS_RC * phi_Po
  psi_Eo <- 1 - Vj
  ETo_RC <- TRo_RC * psi_Eo
  DIo_RC <- ABS_RC - TRo_RC

  # 9) Fluxes per cross-section (CS)
  ABS_CSm <- Fm
  TRo_CSm <- phi_Po * ABS_CSm
  phi_Eo  <- phi_Po * psi_Eo
  ETo_CSm <- phi_Eo * ABS_CSm
  DIo_CSm <- ABS_CSm - TRo_CSm

  # 10) Performance Index (PI_abs)
  # PI_abs = (RC/ABS) * (phi_Po / (1-phi_Po)) * (psi_Eo / (1-psi_Eo))
  RC_ABS <- safe_div(1, ABS_RC)
  phi_Po_term <- safe_div(phi_Po, 1 - phi_Po)
  psi_Eo_term <- safe_div(psi_Eo, 1 - psi_Eo)
  PI_abs <- RC_ABS * phi_Po_term * psi_Eo_term

  # 11) Kn / Kp terms
  Kn <- safe_div(1, Fm)
  Kp <- safe_div(1, Fv)

  # 12) Binding results
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

#' Wrapper to read CSV and calculate OJIP parameters
#'
#' @param file Path to the CSV file.
#' @param sep Separator (default ";").
#' @param dec Decimal point (default ".").
#' @param ... Additional arguments passed to read.csv.
#' @importFrom utils read.csv
#' @export
calc_fluorojip_file <- function(file, sep = ";", dec = ".", ...) {
  df <- utils::read.csv(file, sep = sep, dec = dec, header = TRUE, stringsAsFactors = FALSE, ...)
  calc_fluorojip(df)
}

#' Write Normalized JIP Table
#'
#' @param df The data frame to save.
#' @param file The path where the file will be saved.
#' @param ... Additional arguments passed to write.csv.
#' @export
write_normalized_jiptable <- function(df, file, ...) {
  utils::write.csv(df, file = file, row.names = FALSE, ...)
}
