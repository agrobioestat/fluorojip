#' Build a normalized JIP parameter table
#'
#' Creates a wide or long table of FluorOJIP / JIP-test parameters for each
#' sample, with optional normalization for exploratory analysis, comparison
#' across treatments, and export.
#'
#' @param df A data frame, typically the output of \code{calc_fluorojip()}.
#' @param params Character vector of parameter names to include. If \code{NULL},
#'   a default set of commonly used JIP-test parameters is selected.
#' @param sample_col Name of the sample identifier column.
#' @param group_col Name of the grouping column, typically a treatment column.
#' @param normalize Normalization method. One of \code{"none"},
#'   \code{"zscore"}, \code{"minmax"}, \code{"control_ratio"}, or
#'   \code{"control_then_zscore"}.
#' @param control_level Level of \code{group_col} to be used as the control when
#'   a control-based normalization method is requested.
#' @param output Output format: \code{"wide"} or \code{"long"}.
#' @param digits Number of decimal places used to round normalized parameter
#'   columns.
#'
#' @return A data frame in wide or long format containing the selected JIP-test
#'   parameters after the requested normalization step.
#'
#' @details
#' This function is intended for exploratory analysis and reporting workflows in
#' which selected FluorOJIP / JIP-test parameters need to be compared across
#' samples or treatments on a common scale.
#'
#' Parameters such as \code{PI_abs}, \code{Mo}, and RC-based fluxes depend on
#' the availability of a K-step / 300 us-equivalent input (for example,
#' \code{k} or \code{f300us}) in the original summary data used by
#' \code{calc_fluorojip()}.
#'
#' Cross-section outputs such as \code{ABS_CSm}, \code{TRo_CSm},
#' \code{ETo_CSm}, and \code{DIo_CSm} are package outputs intended for
#' operational comparison workflows and may not fully match every
#' instrument-specific convention.
#'
#' @export
normalized_jiptable <- function(df,
                                params = NULL,
                                sample_col = "sample_id",
                                group_col = "treatment",
                                normalize = c("none", "zscore", "minmax", "control_ratio", "control_then_zscore"),
                                control_level = NULL,
                                output = c("wide", "long"),
                                digits = 6) {

  normalize <- match.arg(normalize)
  output <- match.arg(output)

  if (is.null(params)) {
    params <- c("Fv_Fm", "PI_abs", "ABS_RC", "TRo_RC", "ETo_RC", "DIo_RC",
                "ABS_CSm", "TRo_CSm", "ETo_CSm", "DIo_CSm")
  }

  # Ensure requested parameter columns exist
  available <- intersect(params, names(df))
  if (length(available) < length(params)) {
    warning(
      "Some requested params were not found in 'df': ",
      paste(setdiff(params, available), collapse = ", ")
    )
  }
  params <- available

  # Subset to required columns only
  cols_keep <- c(sample_col, group_col, params)
  df_sub <- df[, intersect(cols_keep, names(df)), drop = FALSE]

  # Normalize selected parameters
  df_norm <- df_sub

  if (normalize != "none") {
    scale_vec <- function(x, method, ctrl_val = NA_real_) {
      if (all(is.na(x))) return(x)

      if (method == "zscore") {
        mu <- mean(x, na.rm = TRUE)
        sdv <- stats::sd(x, na.rm = TRUE)
        if (is.na(sdv) || sdv == 0) return(x * 0)
        return((x - mu) / sdv)
      }

      if (method == "minmax") {
        rng <- range(x, na.rm = TRUE)
        if (rng[2] == rng[1]) return(x * 0)
        return((x - rng[1]) / (rng[2] - rng[1]))
      }

      if (method == "control_ratio") {
        if (is.na(ctrl_val) || ctrl_val == 0) return(x)
        return(x / ctrl_val)
      }

      x
    }

    # Calculate control means when a control-based normalization is requested
    ctrl_means <- rep(NA_real_, length(params))
    names(ctrl_means) <- params

    if (grepl("control", normalize)) {
      if (is.null(control_level) || is.null(group_col)) {
        stop("Must provide 'control_level' and 'group_col' for control-based normalization.")
      }

      idx_ctrl <- which(df_sub[[group_col]] == control_level)
      if (length(idx_ctrl) == 0) stop("Control level not found in data.")

      for (p in params) {
        ctrl_means[p] <- mean(df_sub[idx_ctrl, p], na.rm = TRUE)
      }
    }

    for (p in params) {
      vals <- df_sub[[p]]

      if (normalize == "control_ratio") {
        df_norm[[p]] <- scale_vec(vals, "control_ratio", ctrl_means[p])
      } else if (normalize == "control_then_zscore") {
        vals_ratio <- scale_vec(vals, "control_ratio", ctrl_means[p])
        df_norm[[p]] <- scale_vec(vals_ratio, "zscore")
      } else {
        df_norm[[p]] <- scale_vec(vals, normalize)
      }
    }
  }

  # Round numeric parameter columns
  for (p in params) {
    if (is.numeric(df_norm[[p]])) {
      df_norm[[p]] <- round(df_norm[[p]], digits = digits)
    }
  }

  if (output == "wide") {
    return(df_norm)
  }

  # Convert to long format using base R
  df_long <- data.frame()
  for (p in params) {
    tmp <- df_norm[, c(sample_col, group_col), drop = FALSE]
    tmp$parameter <- p
    tmp$value <- df_norm[[p]]
    df_long <- rbind(df_long, tmp)
  }

  df_long
}

#' Write a normalized JIP parameter table
#'
#' Exports a normalized FluorOJIP / JIP-test parameter table to disk for use in
#' spreadsheet software or downstream statistical workflows.
#'
#' @param df A data frame to export, typically generated by
#'   \code{normalized_jiptable()}.
#' @param file Path to the output file.
#' @param ... Additional arguments passed to \code{utils::write.table()}.
#'
#' @return This function is called for its side effect of writing a file to
#'   disk. It invisibly returns the output path.
#'
#' @details
#' The table is written with a semicolon field separator (\code{sep = ";"})
#' and decimal point (\code{dec = "."}). This behavior is intentional and may
#' be convenient for spreadsheet workflows, but it differs from the default
#' behavior of \code{write.csv()}.
#'
#' @export
write_normalized_jiptable <- function(df, file, ...) {
  utils::write.table(
    df,
    file,
    sep = ";",
    dec = ".",
    row.names = FALSE,
    quote = FALSE,
    ...
  )

  invisible(file)
}
