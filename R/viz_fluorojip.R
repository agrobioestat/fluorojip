# Colour-blind-safe categorical palette (Okabe & Ito 2008)
okabe_ito <- function(n) {
  pal <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#F0E442", "#000000")
  rep_len(pal, n)
}

#' Plot a heatmap of JIP-test parameters
#'
#' Draws a clustered heatmap of selected parameters (columns) across samples
#' (rows) with a colour key. With z-score scaling the colour scale is
#' symmetric around zero, so equal colour intensities correspond to equal
#' standardized deviations above and below the mean.
#'
#' @param res Data frame with results, typically from [calc_fluorojip()].
#' @param params Character vector of parameters to plot.
#' @param sample_col Name of the sample identifier column.
#' @param group_col Name of the grouping column, used to label rows and to draw
#'   a colour bar; ignored if absent.
#' @param scale `"zscore"` (standardize each parameter across samples),
#'   `"none"` (plot raw values), or `"control_then_zscore"` (divide by the
#'   mean of `control_level`, then standardize).
#' @param control_level Control level of `group_col` for
#'   `"control_then_zscore"`.
#' @param cluster Logical; order rows and columns by hierarchical clustering
#'   (Euclidean distance, complete linkage).
#' @param palette Diverging palette name passed to [grDevices::hcl.colors()].
#' @param main Plot title.
#' @param key_label Label of the colour key.
#' @return Invisibly, a list with the plotted matrix (`matrix`), row and
#'   column orders, and the colour limits (`zlim`).
#' @details The heatmap is an exploratory display. With few samples and
#'   strongly inter-dependent parameters (many JIP-test parameters are
#'   functions of the same four or five fluorescence values), clustering
#'   patterns should not be interpreted as statistical evidence.
#' @examples
#' res <- calc_fluorojip(example_fluorojip)
#' plot_heatmap_fluorojip(
#'   res,
#'   params = c("Fv_Fm", "PI_abs", "ABS_RC", "TRo_RC", "ETo_RC", "DIo_RC"),
#'   group_col = "treatment"
#' )
#' @export
plot_heatmap_fluorojip <- function(res,
                                   params,
                                   sample_col = "sample_id",
                                   group_col = "treatment",
                                   scale = c("zscore", "none", "control_then_zscore"),
                                   control_level = "control",
                                   cluster = TRUE,
                                   palette = "Blue-Red 3",
                                   main = "JIP-test parameters",
                                   key_label = NULL) {
  scale <- match.arg(scale)
  if (!all(params %in% names(res))) {
    stop("Some requested parameters were not found in 'res': ",
         paste(setdiff(params, names(res)), collapse = ", "), call. = FALSE)
  }
  if (!sample_col %in% names(res)) {
    stop("Column specified in 'sample_col' was not found in 'res'.", call. = FALSE)
  }

  mat <- as.matrix(res[, params, drop = FALSE])
  storage.mode(mat) <- "double"
  has_group <- !is.null(group_col) && group_col %in% names(res)
  labs <- as.character(res[[sample_col]])
  if (has_group) labs <- paste0(labs, " (", res[[group_col]], ")")
  rownames(mat) <- labs

  if (scale == "control_then_zscore") {
    if (!has_group) stop("'group_col' is required for control-based scaling.", call. = FALSE)
    ctrl <- res[[group_col]] == control_level
    if (!any(ctrl)) stop("Control level not found in 'group_col'.", call. = FALSE)
    mat <- sweep(mat, 2, colMeans(mat[ctrl, , drop = FALSE], na.rm = TRUE), "/")
  }
  if (scale %in% c("zscore", "control_then_zscore")) {
    sds <- apply(mat, 2, stats::sd, na.rm = TRUE)
    mat <- sweep(sweep(mat, 2, colMeans(mat, na.rm = TRUE), "-"), 2, ifelse(sds > 0, sds, 1), "/")
    if (is.null(key_label)) key_label <- "z-score"
  }
  if (is.null(key_label)) key_label <- "value"

  ro <- seq_len(nrow(mat))
  co <- seq_len(ncol(mat))
  if (cluster && nrow(mat) > 2 && ncol(mat) > 1) {
    m0 <- mat
    m0[!is.finite(m0)] <- 0
    ro <- stats::hclust(stats::dist(m0), method = "complete")$order
    co <- stats::hclust(stats::dist(t(m0)), method = "complete")$order
  }
  pm <- mat[ro, co, drop = FALSE]

  if (scale == "none") {
    zlim <- range(pm, na.rm = TRUE)
  } else {
    lim <- max(abs(pm), na.rm = TRUE)
    zlim <- c(-lim, lim)
  }
  pal <- grDevices::hcl.colors(101, palette)
  brk <- seq(zlim[1], zlim[2], length.out = length(pal) + 1)

  old <- graphics::par(no.readonly = TRUE)
  on.exit({ graphics::layout(1); graphics::par(old) }, add = TRUE)
  graphics::layout(matrix(c(1, 2), nrow = 1), widths = c(5.4, 1))
  left <- max(4, 0.55 * max(nchar(rownames(pm))))
  bottom <- max(4.5, 0.8 * max(nchar(colnames(pm))) + 1)
  graphics::par(mar = c(bottom, left, 3, 0.5))
  graphics::image(seq_len(ncol(pm)), seq_len(nrow(pm)), t(pm[nrow(pm):1, , drop = FALSE]),
                  col = pal, breaks = brk, axes = FALSE, xlab = "", ylab = "", main = main)
  graphics::axis(1, at = seq_len(ncol(pm)), labels = colnames(pm), las = 2, tick = FALSE)
  graphics::axis(2, at = seq_len(nrow(pm)), labels = rev(rownames(pm)), las = 1, tick = FALSE)
  graphics::box()

  graphics::par(mar = c(bottom, 0.5, 3, 3.5))
  graphics::image(1, seq_along(pal), matrix(seq_along(pal), nrow = 1), col = pal,
                  axes = FALSE, xlab = "", ylab = "")
  ticks <- pretty(zlim, n = 5)
  ticks <- ticks[ticks >= zlim[1] & ticks <= zlim[2]]
  graphics::axis(4, at = 1 + (ticks - zlim[1]) / diff(zlim) * (length(pal) - 1), labels = ticks, las = 1)
  graphics::mtext(key_label, side = 3, line = 0.5, cex = 0.9)
  graphics::box()

  invisible(list(matrix = pm, row_order = ro, col_order = co, zlim = zlim))
}

#' @rdname plot_heatmap_fluorojip
#' @export
plot_param_heatmap <- plot_heatmap_fluorojip

#' Plot a 3D scatter plot of JIP-test parameters
#'
#' Exploratory 3D scatter plot of exactly three parameters, coloured and
#' shaped by group, with a legend. Requires the suggested package
#' \pkg{scatterplot3d}.
#'
#' @param res Data frame with results, typically from [calc_fluorojip()].
#' @param params Character vector of exactly three parameter names.
#' @param group_col Grouping column used for colours, symbols, and legend.
#' @param normalize Logical; z-score each parameter before plotting. Axes are
#'   then labelled "(z)" and are not in physiological units.
#' @param colors Optional vector of colours, one per group (defaults to the
#'   colour-blind-safe Okabe-Ito palette).
#' @param angle Viewing angle passed to [scatterplot3d::scatterplot3d()].
#' @param main Plot title.
#' @param legend_pos Legend position passed to [graphics::legend()].
#' @return Invisibly returns the object produced by
#'   [scatterplot3d::scatterplot3d()].
#' @details `plot_param_3d()` and `plot_param_surface3d()` are aliases kept
#'   for backward compatibility (the latter draws a scatter plot, not a
#'   surface).
#' @examples
#' if (requireNamespace("scatterplot3d", quietly = TRUE)) {
#'   res <- calc_fluorojip(example_fluorojip)
#'   plot_3d_fluorojip(res, params = c("Fv_Fm", "PI_abs", "ABS_RC"))
#' }
#' @export
plot_3d_fluorojip <- function(res,
                              params = c("Fv_Fm", "PI_abs", "ABS_RC"),
                              group_col = "treatment",
                              normalize = TRUE,
                              colors = NULL,
                              angle = 45,
                              main = NULL,
                              legend_pos = "topleft") {
  if (length(params) != 3) {
    stop("Please provide exactly 3 parameters for the 3D plot.", call. = FALSE)
  }
  if (!all(params %in% names(res))) {
    stop("Some requested parameters were not found in 'res'.", call. = FALSE)
  }
  if (!requireNamespace("scatterplot3d", quietly = TRUE)) {
    stop("Package 'scatterplot3d' is required. Please install it.", call. = FALSE)
  }

  d <- as.data.frame(lapply(res[, params, drop = FALSE], as.numeric))
  labs <- params
  if (normalize) {
    d <- as.data.frame(scale(d))
    labs <- paste0(params, " (z)")
  }
  groups <- if (!is.null(group_col) && group_col %in% names(res)) factor(res[[group_col]]) else factor(rep("all", nrow(res)))
  cols <- if (is.null(colors)) okabe_ito(nlevels(groups)) else rep_len(colors, nlevels(groups))
  pchs <- rep_len(c(16, 17, 15, 18, 1, 2, 0, 5), nlevels(groups))

  s3d <- scatterplot3d::scatterplot3d(
    d[[1]], d[[2]], d[[3]],
    color = cols[as.integer(groups)],
    pch = pchs[as.integer(groups)],
    type = "h", lty.hplot = 3,
    xlab = labs[1], ylab = labs[2], zlab = labs[3],
    main = if (is.null(main)) "" else main,
    angle = angle, box = TRUE
  )
  if (nlevels(groups) > 1) {
    graphics::legend(legend_pos, legend = levels(groups), col = cols, pch = pchs, bty = "n", inset = 0.02)
  }
  invisible(s3d)
}

#' @rdname plot_3d_fluorojip
#' @export
plot_param_3d <- plot_3d_fluorojip

#' @rdname plot_3d_fluorojip
#' @export
plot_param_surface3d <- plot_3d_fluorojip

#' Plot OJIP traces with the extracted points
#'
#' Plots fluorescence traces on a logarithmic time axis and marks the points
#' selected by [trace_to_ojip()] (O, early O-J point, J, I, and P). Useful for
#' checking extraction and for inspecting curve shape before biological
#' interpretation.
#'
#' @param times Numeric vector of times (any unit accepted by
#'   [trace_to_ojip()]).
#' @param mat Matrix of traces (one row per sample) or a single trace.
#' @param ojip Optional result of [trace_to_ojip()] for the same traces; if
#'   `NULL` it is computed with `...`.
#' @param samples Indices or identifiers of the traces to draw (default: the
#'   first five).
#' @param normalize `"none"` (background-corrected signal), `"fm"` (F / Fm),
#'   or `"vt"` (relative variable fluorescence Vt = (F - Fo)/(Fm - Fo)).
#' @param colors Optional colours, one per drawn sample.
#' @param main Plot title.
#' @param legend_pos Legend position passed to [graphics::legend()].
#' @param ... Arguments passed to [trace_to_ojip()] (for example `preset` and
#'   `time_unit`).
#' @return Invisibly, the OJIP summary used for the markers.
#' @examples
#' raw <- read_biolyzer_xls(fluorojip_example_biolyzer_file())
#' plot_ojip_trace(raw$times_ms, raw$mat, time_unit = "ms", preset = "biolyzer",
#'                 samples = 1:3, normalize = "vt")
#' @export
plot_ojip_trace <- function(times, mat, ojip = NULL, samples = NULL,
                            normalize = c("none", "fm", "vt"),
                            colors = NULL, main = "OJIP transients",
                            legend_pos = "bottomright", ...) {
  normalize <- match.arg(normalize)
  if (is.null(dim(mat))) mat <- matrix(mat, nrow = 1)
  if (is.null(ojip)) ojip <- trace_to_ojip(times, mat, ...)
  ext <- attr(ojip, "extraction")
  factors <- c(s = 1000, ms = 1, us = 0.001)
  unit <- if (!is.null(ext$time_unit)) ext$time_unit else "ms"
  t_ms <- as.numeric(times) * factors[[unit]]
  keep <- is.finite(t_ms) & t_ms > 0 & colSums(is.finite(mat)) > 0
  bg_first <- identical(ext$background, "first_point")
  if (bg_first) keep[which(keep)[1]] <- FALSE

  ids <- ojip$sample_id
  if (is.null(samples)) samples <- seq_len(min(5, nrow(mat)))
  if (is.character(samples)) samples <- match(samples, ids)
  samples <- samples[!is.na(samples)]
  cols <- if (is.null(colors)) okabe_ito(length(samples)) else rep_len(colors, length(samples))

  tr <- function(r) {
    y <- as.numeric(mat[r, keep]) - ojip$background[r]
    switch(normalize,
           none = y,
           fm = y / ojip$fm[r],
           vt = (y - ojip$fo[r]) / (ojip$fm[r] - ojip$fo[r]))
  }
  tx <- function(r, v) switch(normalize, none = v, fm = v / ojip$fm[r],
                              vt = (v - ojip$fo[r]) / (ojip$fm[r] - ojip$fo[r]))
  ys <- lapply(samples, tr)
  ylab <- switch(normalize, none = "Fluorescence (instrument units)", fm = "F / Fm",
                 vt = "Relative variable fluorescence, Vt")
  graphics::plot(range(t_ms[keep]), range(unlist(ys), na.rm = TRUE), type = "n", log = "x",
                 ann = FALSE)
  graphics::title(main = main, xlab = "Time (ms, log scale)")
  graphics::title(ylab = ylab, line = max(2, graphics::par("mar")[2] - 1.2))
  for (k in seq_along(samples)) graphics::lines(t_ms[keep], ys[[k]], col = cols[k], lwd = 1.5)
  pts <- c(O = "fo", K = "f300us", J = "j", I = "i", P = "fm")
  tcol <- c(O = "t_fo_ms", K = "t_f300us_ms", J = "t_j_ms", I = "t_i_ms", P = "t_fm_ms")
  for (k in seq_along(samples)) {
    r <- samples[k]
    xs <- vapply(tcol, function(cn) ojip[[cn]][r], numeric(1))
    yv <- vapply(pts, function(cn) tx(r, ojip[[cn]][r]), numeric(1))
    graphics::points(xs, yv, pch = 21, bg = "white", col = cols[k], cex = 1.1)
  }
  r1 <- samples[1]
  xs1 <- vapply(tcol, function(cn) ojip[[cn]][r1], numeric(1))
  graphics::abline(v = xs1[is.finite(xs1)], lty = 3, col = "grey60")
  graphics::mtext(names(pts)[is.finite(xs1)], side = 3, at = xs1[is.finite(xs1)], line = 0.1, cex = 0.8)
  graphics::legend(legend_pos, legend = ids[samples], col = cols, lwd = 1.5, bty = "n", cex = 0.8)
  invisible(ojip)
}
