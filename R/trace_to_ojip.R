#' Extraction presets for OJIP fluorescence traces
#'
#' Returns the time targets, tolerance windows, and background rule used by
#' [trace_to_ojip()] to convert a fluorescence trace into the OJIP summary
#' values required by [calc_fluorojip()].
#'
#' @param name Preset name. `"handypea"` (alias `"biolyzer"`) follows the
#'   convention used by Biolyzer for Hansatech Handy-PEA traces: Fo at 20 us,
#'   early O-J point at 270 us (that is, 250 us after Fo), J at 2 ms, and I at
#'   30 ms, without background subtraction. `"fluorpen"` follows the convention
#'   documented for the Photon Systems Instruments FluorPen OJIP protocol:
#'   the background reading stored as the first point of the exported trace is
#'   subtracted, Fo is taken at the nominal 50 us position (exported sample
#'   labelled 41 us), the early point at 300 us, J at 2 ms, and I at 30 ms.
#' @return A list with elements `name`, `targets_ms` (named numeric vector
#'   with entries `fo`, `f300us`, `j`, `i`), `tolerance_ms` (same names),
#'   `background` (`"none"` or `"first_point"`), `time_unit` (`"auto"`, `"s"`,
#'   `"ms"`, or `"us"`), `mo_interval_ms`, and `note`.
#' @details In every preset, Fm is the maximum of the (background-corrected)
#'   trace and the area is integrated between the Fo point and the time of Fm.
#'   A preset can be modified by passing `targets_ms`, `tolerance_ms`, or
#'   `background` directly to [trace_to_ojip()].
#' @seealso [trace_to_ojip()]
#' @examples
#' ojip_preset("handypea")$targets_ms
#' ojip_preset("fluorpen")
#' @export
ojip_preset <- function(name = c("handypea", "biolyzer", "fluorpen")) {
  name <- match.arg(name)
  if (name %in% c("handypea", "biolyzer")) {
    return(list(
      name = name,
      targets_ms = c(fo = 0.02, f300us = 0.27, j = 2, i = 30),
      tolerance_ms = c(fo = 0.005, f300us = 0.005, j = 0.1, i = 1),
      background = "none",
      time_unit = "auto",
      mo_interval_ms = 0.25,
      note = paste(
        "Fo = F(20 us), early O-J point = F(270 us), J = F(2 ms), I = F(30 ms),",
        "Fm = trace maximum; no background subtraction (Handy-PEA / Biolyzer convention).",
        "Biolyzer itself may report an extrapolated Fo ('Fo calc'), which is not reproduced."
      )
    ))
  }
  list(
    name = name,
    targets_ms = c(fo = 0.04, f300us = 0.30, j = 2, i = 30),
    tolerance_ms = c(fo = 0.005, f300us = 0.005, j = 0.1, i = 1),
    background = "first_point",
    time_unit = "us",
    mo_interval_ms = 0.25,
    note = paste(
      "First exported point treated as background and subtracted; Fo = nominal F(50 us)",
      "(exported sample labelled 41 us), early O-J point = F(300 us), J = F(2 ms), I = F(30 ms),",
      "Fm = trace maximum (FluorPen OJIP protocol convention)."
    )
  )
}

#' Convert fluorescence traces to an OJIP summary table
#'
#' Extracts Fo, the early O-J point (F300us), Fj, Fi, Fm, the time of Fm, and
#' the area above the transient from one or more fluorescence traces, using
#' explicit time targets and tolerance windows. The result is ready for
#' [calc_fluorojip()] and documents, for every sample, the time points that
#' were actually used and any quality flags raised during extraction.
#'
#' @param times Numeric vector of time values, one per trace column.
#' @param mat Numeric matrix (or vector for a single trace) of fluorescence
#'   values, one row per sample and one column per time point.
#' @param preset Name of an extraction preset (see [ojip_preset()]) or a list
#'   with the same structure.
#' @param time_unit Unit of `times`: `"s"`, `"ms"`, `"us"`, or `"auto"`. With
#'   `"auto"` the unit is inferred by matching the time grid to the standard
#'   OJIP positions; the inferred unit is reported in the `extraction`
#'   attribute. `NULL` uses the preset value.
#' @param targets_ms,tolerance_ms Optional named numeric vectors (names `fo`,
#'   `f300us`, `j`, `i`) overriding the preset target times and the maximum
#'   accepted distance (in ms) between a target and the nearest recorded time.
#'   If the nearest point is farther than the tolerance, the value is returned
#'   as `NA` and flagged; it is never replaced by a more distant point.
#' @param background `"none"`, `"first_point"` (subtract the first recorded
#'   value of each trace, then drop that point), or a numeric vector with one
#'   background value per sample. `NULL` uses the preset value.
#' @param step_method `"fixed"` (default) uses the fixed J and I target times.
#'   `"derivative"` is an experimental alternative that locates J and I at the
#'   local minima of the smoothed derivative dF/dlog(t) inside the windows
#'   given by `step_windows_ms`; when no minimum is found the value is `NA` and
#'   flagged. Fixed times keep results comparable with instrument software;
#'   the derivative option is intended for inspecting step timing (for example
#'   when the I step is expected to shift with the size of the plastoquinone
#'   pool).
#' @param step_windows_ms List with numeric ranges `j` and `i` (in ms) used by
#'   `step_method = "derivative"`.
#' @param area_time_unit Time unit used for the area (`"ms"` or `"us"`).
#'   `Sm = area / Fv` inherits this unit. Biolyzer reports areas in F ms.
#' @param sample_id Optional sample identifiers; defaults to the row names of
#'   `mat`.
#' @return A data frame with one row per sample and columns `sample_id`, `fo`,
#'   `f300us`, `k` (backward-compatible alias of `f300us`), `j`, `i`, `fm`, `p`
#'   (alias of `fm`), `area`, the times actually used (`t_fo_ms`,
#'   `t_f300us_ms`, `t_j_ms`, `t_i_ms`, `t_fm_ms`, and `t_fm` as an alias),
#'   `background`, and `qc_flags`. The attribute `extraction` records the
#'   preset, targets, tolerances, inferred time unit, background rule, step
#'   method, area unit, and number of discarded time points.
#' @details
#' **Cleaning.** Time points that are missing, non-finite, or not positive
#' (for example zero padding at the end of a Biolyzer export) are discarded
#' before extraction. Traces are sorted by time if necessary.
#'
#' **Fm and area.** Fm is the maximum of the background-corrected trace;
#' `t_fm_ms` is the time of that maximum. The area is the trapezoidal
#' integral of (Fm - F(t)) on the recorded (possibly non-uniform) time grid
#' from the Fo point to `t_fm_ms`.
#'
#' **Quality flags** (column `qc_flags`, `"ok"` when none apply):
#' \itemize{
#'   \item `fo_time_out_of_tolerance`, `f300us_time_out_of_tolerance`,
#'     `j_time_out_of_tolerance`, `i_time_out_of_tolerance`: no recorded point
#'     within the tolerance window; the value is `NA`.
#'   \item `j_step_not_detected`, `i_step_not_detected`: derivative method
#'     found no minimum in the search window.
#'   \item `fm_at_last_point`: the maximum occurs at the last recorded point,
#'     so the P level may not have been reached within the record.
#'   \item `fm_before_i`: the maximum occurs before the I-step time, which is
#'     incompatible with the usual OJIP shape (for example a non-saturating
#'     pulse, a damaged sample, or a noise spike).
#'   \item `mo_interval_deviation`: the recorded Fo and F300us points are not
#'     separated by `mo_interval_ms` (within 0.015 ms), so the factor used for
#'     `Mo` does not correspond to the actual sampling interval.
#'   \item `few_points`: fewer than 20 valid time points.
#' }
#' These flags are diagnostics only. They cannot show that a pulse was
#' saturating or that Fm equals the true maximal fluorescence; such questions
#' require the measurement protocol and inspection of the raw curve (see
#' [plot_ojip_trace()]).
#' @seealso [ojip_preset()], [calc_fluorojip()], [plot_ojip_trace()]
#' @examples
#' times_ms <- c(0.01, 0.02, 0.05, 0.1, 0.27, 0.5, 1, 2, 5, 10, 30, 100, 200, 300)
#' trace <- c(500, 500, 540, 600, 760, 950, 1200, 1500, 1650, 1800,
#'            2100, 2450, 2500, 2480)
#' ojip <- trace_to_ojip(times_ms, trace, time_unit = "ms", preset = "handypea")
#' ojip[, c("fo", "f300us", "j", "i", "fm", "t_fm_ms", "qc_flags")]
#' calc_fluorojip(ojip)[, c("Fv_Fm", "Mo", "PI_abs")]
#' @export
trace_to_ojip <- function(times,
                          mat,
                          preset = "handypea",
                          time_unit = NULL,
                          targets_ms = NULL,
                          tolerance_ms = NULL,
                          background = NULL,
                          step_method = c("fixed", "derivative"),
                          step_windows_ms = list(j = c(0.8, 5), i = c(10, 100)),
                          area_time_unit = c("ms", "us"),
                          sample_id = NULL) {

  step_method <- match.arg(step_method)
  area_time_unit <- match.arg(area_time_unit)
  pre <- if (is.list(preset)) preset else ojip_preset(preset)
  if (is.null(time_unit)) time_unit <- pre$time_unit
  time_unit <- match.arg(time_unit, c("auto", "s", "ms", "us"))
  if (is.null(background)) background <- pre$background
  tg <- pre$targets_ms
  if (!is.null(targets_ms)) tg[names(targets_ms)] <- targets_ms
  tol <- pre$tolerance_ms
  if (!is.null(tolerance_ms)) tol[names(tolerance_ms)] <- tolerance_ms
  mo_int <- if (is.null(pre$mo_interval_ms)) 0.25 else pre$mo_interval_ms

  # --- inputs -------------------------------------------------------------
  if (is.null(dim(mat))) mat <- matrix(mat, nrow = 1)
  mat <- as.matrix(mat)
  storage.mode(mat) <- "double"
  times <- suppressWarnings(as.numeric(times))
  if (length(times) != ncol(mat)) {
    stop("`times` must have one value per column of `mat`.", call. = FALSE)
  }
  if (is.null(sample_id)) {
    sample_id <- rownames(mat)
    if (is.null(sample_id)) sample_id <- paste0("S", seq_len(nrow(mat)))
  }
  if (length(sample_id) != nrow(mat)) {
    stop("`sample_id` must have one value per row of `mat`.", call. = FALSE)
  }

  # --- cleaning -----------------------------------------------------------
  keep <- is.finite(times) & times > 0 & colSums(is.finite(mat)) > 0
  n_dropped <- sum(!keep)
  times <- times[keep]
  mat <- mat[, keep, drop = FALSE]
  if (length(times) < 2) stop("Fewer than two valid time points.", call. = FALSE)
  if (anyDuplicated(times)) stop("Duplicated time values in the trace.", call. = FALSE)
  if (is.unsorted(times)) {
    o <- order(times)
    times <- times[o]
    mat <- mat[, o, drop = FALSE]
    warning("Time values were not increasing and have been sorted.", call. = FALSE)
  }

  # --- time unit ----------------------------------------------------------
  factors <- c(s = 1000, ms = 1, us = 0.001)
  if (time_unit == "auto") {
    ref <- c(0.02, 0.27, 2, 30)
    score <- vapply(factors, function(f) {
      tt <- times * f
      sum(vapply(ref, function(r) min(abs(log10(tt) - log10(r))), numeric(1)))
    }, numeric(1))
    time_unit <- names(factors)[which.min(score)]
  }
  t_ms <- times * factors[[time_unit]]

  # --- background ---------------------------------------------------------
  if (is.numeric(background)) {
    if (length(background) == 1L) background <- rep(background, nrow(mat))
    if (length(background) != nrow(mat)) {
      stop("Numeric `background` must have one value per sample.", call. = FALSE)
    }
    bg <- background
    bg_rule <- "user-supplied"
  } else {
    background <- match.arg(background, c("none", "first_point"))
    bg_rule <- background
    if (background == "first_point") {
      bg <- mat[, 1]
      mat <- mat[, -1, drop = FALSE]
      t_ms <- t_ms[-1]
    } else {
      bg <- rep(0, nrow(mat))
    }
  }
  mat <- mat - bg

  n <- nrow(mat)
  flags <- vector("list", n)
  add_flag <- function(r, f) flags[[r]] <<- c(flags[[r]], f)

  pick <- function(target, tolerance, label) {
    idx <- which.min(abs(t_ms - target))
    ok <- abs(t_ms[idx] - target) <= tolerance + 1e-9
    list(idx = idx, ok = ok, time = t_ms[idx], label = label)
  }

  out <- data.frame(sample_id = as.character(sample_id), stringsAsFactors = FALSE)
  for (nm in c("fo", "f300us", "j", "i")) {
    p <- pick(tg[[nm]], tol[[nm]], nm)
    vals <- mat[, p$idx]
    tt <- rep(p$time, n)
    if (!p$ok) {
      vals[] <- NA_real_
      tt[] <- NA_real_
      for (r in seq_len(n)) add_flag(r, paste0(nm, "_time_out_of_tolerance"))
      warning(sprintf(
        "No recorded point within %.4g ms of the %s target (%.4g ms); nearest is %.4g ms. %s set to NA.",
        tol[[nm]], nm, tg[[nm]], p$time, nm
      ), call. = FALSE)
    }
    out[[nm]] <- vals
    out[[paste0("t_", nm, "_ms")]] <- tt
  }

  if (step_method == "derivative") {
    for (nm in c("j", "i")) {
      det <- t(vapply(seq_len(n), function(r) {
        derivative_step(t_ms, mat[r, ], step_windows_ms[[nm]])
      }, numeric(2)))
      miss <- !is.finite(det[, 1])
      out[[nm]] <- det[, 2]
      out[[paste0("t_", nm, "_ms")]] <- det[, 1]
      for (r in which(miss)) add_flag(r, paste0(nm, "_step_not_detected"))
    }
  }

  # --- Fm, t(Fm), area, QC ------------------------------------------------
  area_factor <- if (area_time_unit == "ms") 1 else 1000
  fm <- t_fm <- area <- rep(NA_real_, n)
  for (r in seq_len(n)) {
    y <- mat[r, ]
    okp <- is.finite(y)
    if (sum(okp) < 20) add_flag(r, "few_points")
    if (!any(okp)) next
    ix <- which.max(replace(y, !okp, -Inf))
    fm[r] <- y[ix]
    t_fm[r] <- t_ms[ix]
    if (ix == max(which(okp))) add_flag(r, "fm_at_last_point")
    if (is.finite(out$t_i_ms[r]) && t_fm[r] < out$t_i_ms[r]) add_flag(r, "fm_before_i")
    if (is.finite(out$t_fo_ms[r])) {
      sel <- which(t_ms >= out$t_fo_ms[r] & t_ms <= t_fm[r] & okp)
      if (length(sel) >= 2) {
        xx <- t_ms[sel] * area_factor
        yy <- fm[r] - y[sel]
        area[r] <- sum(diff(xx) * (utils::head(yy, -1) + utils::tail(yy, -1)) / 2)
      }
    }
    if (is.finite(out$t_fo_ms[r]) && is.finite(out$t_f300us_ms[r]) &&
        abs((out$t_f300us_ms[r] - out$t_fo_ms[r]) - mo_int) > 0.015) {
      add_flag(r, "mo_interval_deviation")
    }
  }

  res <- data.frame(
    sample_id = out$sample_id,
    fo = out$fo,
    f300us = out$f300us,
    k = out$f300us,
    j = out$j,
    i = out$i,
    fm = fm,
    p = fm,
    area = area,
    t_fo_ms = out$t_fo_ms,
    t_f300us_ms = out$t_f300us_ms,
    t_j_ms = out$t_j_ms,
    t_i_ms = out$t_i_ms,
    t_fm_ms = t_fm,
    t_fm = t_fm,
    background = bg,
    qc_flags = vapply(flags, function(f) if (length(f) == 0) "ok" else paste(unique(f), collapse = ";"), character(1)),
    stringsAsFactors = FALSE
  )
  attr(res, "extraction") <- list(
    preset = pre$name,
    targets_ms = tg,
    tolerance_ms = tol,
    time_unit = time_unit,
    background = bg_rule,
    step_method = step_method,
    area_time_unit = area_time_unit,
    n_time_points = length(t_ms),
    n_discarded_time_points = n_dropped,
    fluorojip_version = fluorojip_version()
  )
  res
}

# Locate a step as the local minimum of the smoothed derivative dF/dlog10(t)
# inside a time window. Returns c(time_ms, fluorescence) or c(NA, NA).
derivative_step <- function(t_ms, y, window) {
  ok <- is.finite(t_ms) & is.finite(y) & t_ms > 0
  if (sum(ok) < 12) return(c(NA_real_, NA_real_))
  lt <- log10(t_ms[ok])
  yy <- y[ok]
  fit <- tryCatch(
    stats::smooth.spline(lt, yy, df = min(25, floor(length(lt) / 3))),
    error = function(e) NULL
  )
  if (is.null(fit)) return(c(NA_real_, NA_real_))
  d1 <- stats::predict(fit, lt, deriv = 1)$y
  mins <- which(diff(sign(diff(d1))) > 0) + 1
  tt <- t_ms[ok]
  mins <- mins[tt[mins] >= window[1] & tt[mins] <= window[2]]
  if (length(mins) == 0) return(c(NA_real_, NA_real_))
  best <- mins[which.min(d1[mins])]
  c(tt[best], yy[best])
}
