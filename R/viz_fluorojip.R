# ==========================================
# 1. HEATMAP FUNCTION
# ==========================================
#' Plot a heatmap of OJIP parameters
#'
#' Creates a heatmap for selected FluorOJIP / JIP-test parameters across
#' samples, with optional normalization.
#'
#' @param res Data frame with OJIP / JIP-test results.
#' @param params Character vector of parameters to plot.
#' @param sample_col Name of the column with sample IDs.
#' @param group_col Name of the column with treatment or grouping labels.
#' @param scale Normalization method. Supported values are `"zscore"`,
#'   `"none"`, and `"control_then_zscore"`.
#' @param main Title of the plot.
#' @return Invisibly returns the object produced by [stats::heatmap()].
#' @examples
#' df <- data.frame(
#'   sample_id = c("S1", "S2", "S3"),
#'   treatment = c("control", "stress", "stress"),
#'   fo = c(280, 300, 295),
#'   fm = c(1200, 1250, 1230),
#'   j = c(700, 730, 720),
#'   i = c(950, 980, 970),
#'   k = c(340, 360, 350),
#'   area = c(32000, 35000, 34000)
#' )
#'
#' res <- calc_fluorojip(df)
#' plot_heatmap_fluorojip(
#'   res,
#'   params = c("Fv_Fm", "PI_abs", "ABS_RC"),
#'   group_col = "treatment",
#'   scale = "zscore"
#' )
#' @importFrom stats heatmap
#' @importFrom grDevices colorRampPalette
#' @export
plot_heatmap_fluorojip <- function(res,
                                   params,
                                   sample_col = "sample_id",
                                   group_col = "treatment",
                                   scale = "zscore",
                                   main = "Normalized JIP-test parameter heatmap") {

  if (!all(params %in% names(res))) {
    stop("Some requested parameters were not found in 'res'.")
  }
  if (!sample_col %in% names(res)) {
    stop("Column specified in 'sample_col' was not found in 'res'.")
  }
  if (!scale %in% c("zscore", "none", "control_then_zscore")) {
    stop("'scale' must be one of: 'zscore', 'none', or 'control_then_zscore'.")
  }

  # Extract only the requested numeric columns
  mat <- as.matrix(res[, params, drop = FALSE])
  rownames(mat) <- res[[sample_col]]

  # Normalize (Z-score)
  if (scale %in% c("zscore", "control_then_zscore")) {
    mat <- scale(mat)
  }

  heatmap_colors <- colorRampPalette(c("blue", "white", "red"))(100)

  # Define colors based on the treatment/group column
  if (group_col %in% names(res)) {
    groups <- as.factor(res[[group_col]])
    group_palette <- grDevices::rainbow(length(levels(groups)))
    side_colors <- group_palette[as.numeric(groups)]

    hm <- heatmap(
      mat,
      scale = "none",
      main = main,
      margins = c(10, 10),
      col = heatmap_colors,
      RowSideColors = side_colors
    )
  } else {
    hm <- heatmap(
      mat,
      scale = "none",
      main = main,
      margins = c(10, 10),
      col = heatmap_colors
    )
  }

  invisible(hm)
}

#' @rdname plot_heatmap_fluorojip
#' @export
plot_param_heatmap <- plot_heatmap_fluorojip


# ==========================================
# 2. 3D SCATTERPLOT FUNCTION
# ==========================================
#' Plot a 3D scatter plot of OJIP parameters
#'
#' Creates an exploratory 3D scatter plot for exactly three selected
#' FluorOJIP / JIP-test parameters.
#'
#' @param res Data frame with OJIP / JIP-test results.
#' @param params Character vector of exactly 3 parameters to plot.
#' @param group_col Name of the column with treatment or grouping labels.
#' @param normalize Logical indicating whether selected parameters should be
#'   z-score normalized before plotting.
#' @return Invisibly returns the object produced by
#'   [scatterplot3d::scatterplot3d()].
#' @details `plot_param_3d()` and `plot_param_surface3d()` are aliases of
#'   `plot_3d_fluorojip()`. The name `plot_param_surface3d()` is retained for
#'   backward compatibility, although the function produces a 3D scatter plot
#'   rather than an interpolated surface.
#' @examples
#' if (requireNamespace("scatterplot3d", quietly = TRUE)) {
#'   df <- data.frame(
#'     sample_id = c("S1", "S2", "S3"),
#'     treatment = c("control", "stress", "stress"),
#'     fo = c(280, 300, 295),
#'     fm = c(1200, 1250, 1230),
#'     j = c(700, 730, 720),
#'     i = c(950, 980, 970),
#'     k = c(340, 360, 350),
#'     area = c(32000, 35000, 34000)
#'   )
#'
#'   res <- calc_fluorojip(df)
#'   plot_3d_fluorojip(
#'     res,
#'     params = c("Fv_Fm", "PI_abs", "ABS_RC"),
#'     group_col = "treatment",
#'     normalize = TRUE
#'   )
#' }
#' @importFrom graphics legend
#' @importFrom grDevices rainbow
#' @export
plot_3d_fluorojip <- function(res,
                              params = c("Fv_Fm", "PI_abs", "area"),
                              group_col = "treatment",
                              normalize = TRUE) {

  if (length(params) != 3) {
    stop("Please provide exactly 3 parameters for the 3D plot.")
  }
  if (!all(params %in% names(res))) {
    stop("Some requested parameters were not found in 'res'.")
  }

  data_3d <- res[, params, drop = FALSE]

  if (normalize) {
    data_3d <- as.data.frame(scale(data_3d))
  }

  colors <- "blue"
  group_palette <- NULL
  groups <- NULL

  if (group_col %in% names(res)) {
    groups <- as.factor(res[[group_col]])
    group_palette <- rainbow(length(levels(groups)))
    colors <- group_palette[as.numeric(groups)]
  }

  if (!requireNamespace("scatterplot3d", quietly = TRUE)) {
    stop("Package 'scatterplot3d' is required. Please install it.")
  }

  s3d <- scatterplot3d::scatterplot3d(
    data_3d[, 1], data_3d[, 2], data_3d[, 3],
    color = colors,
    pch = 16,
    xlab = params[1],
    ylab = params[2],
    zlab = params[3],
    main = "3D OJIP Parameter Space",
    angle = 45
  )

  if (!is.null(groups)) {
    legend(
      "topright",
      legend = levels(groups),
      col = group_palette,
      pch = 16
    )
  }

  invisible(s3d)
}

#' @rdname plot_3d_fluorojip
#' @export
plot_param_3d <- plot_3d_fluorojip

#' @rdname plot_3d_fluorojip
#' @export
plot_param_surface3d <- plot_3d_fluorojip
