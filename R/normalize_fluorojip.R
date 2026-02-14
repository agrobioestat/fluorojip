#' Normalized JIP-test parameter table
#'
#' Create a tidy (long) and/or wide table of (optionally normalized) JIP-test
#' parameters for each sample.
#'
#' @param df A data frame, typically the output of \code{calc_fluorojip()}.
#' @param params Character vector of parameters to include.
#' @param sample_col Sample identifier column name.
#' @param group_col Grouping column name.
#' @param normalize Normalization method: "none", "zscore", "minmax",
#'   "control_ratio" (value / mean_control), "control_then_zscore".
#' @param control_level Level of \code{group_col} considered as control.
#' @param output Return format: "wide" or "long".
#' @param digits Rounding digits.
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

  # Ensure columns exist
  available <- intersect(params, names(df))
  if (length(available) < length(params)) {
    warning("Some requested params not found in df: ", paste(setdiff(params, available), collapse=", "))
  }
  params <- available

  # Subset
  cols_keep <- c(sample_col, group_col, params)
  # CORREÇÃO AQUI: removido o espaço entre df_ e sub
  df_sub <- df[, intersect(cols_keep, names(df)), drop=FALSE]

  # Normalize
  df_norm <- df_sub

  if (normalize != "none") {
    # Helper to scale vector
    scale_vec <- function(x, method, ctrl_val = NA) {
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
      return(x)
    }

    # Calculate control means if needed
    ctrl_means <- rep(NA, length(params))
    names(ctrl_means) <- params
    if (grepl("control", normalize)) {
      if (is.null(control_level) || is.null(group_col)) {
        stop("Must provide control_level and group_col for control-based normalization")
      }
      idx_ctrl <- which(df_sub[[group_col]] == control_level)
      if (length(idx_ctrl) == 0) stop("Control level not found in data")

      for (p in params) {
        ctrl_means[p] <- mean(df_sub[idx_ctrl, p], na.rm = TRUE)
      }
    }

    # Apply normalization
    for (p in params) {
      vals <- df_sub[[p]]

      if (normalize == "control_ratio") {
        df_norm[[p]] <- scale_vec(vals, "control_ratio", ctrl_means[p])
      } else if (normalize == "control_then_zscore") {
        # First ratio, then zscore of the whole set
        vals_ratio <- scale_vec(vals, "control_ratio", ctrl_means[p])
        df_norm[[p]] <- scale_vec(vals_ratio, "zscore")
      } else {
        df_norm[[p]] <- scale_vec(vals, normalize)
      }
    }
  }

  if (output == "wide") {
    return(df_norm)
  } else {
    # Convert to long
    # Using base reshape logic or utils::stack
    df_long <- data.frame()
    for (p in params) {
      tmp <- df_norm[, c(sample_col, group_col)]
      tmp$parameter <- p
      tmp$value <- df_norm[[p]]
      df_long <- rbind(df_long, tmp)
    }
    return(df_long)
  }
}

#' Write Normalized JIP Table
#'
#' @param df The data frame to save.
#' @param file The path where the file will be saved.
#' @param ... Additional arguments passed to write.csv.
#' @export
write_normalized_jiptable <- function(df, file, ...) {
  utils::write.table(df, file, sep = ";", dec = ".", row.names = FALSE, quote = FALSE, ...)
}
