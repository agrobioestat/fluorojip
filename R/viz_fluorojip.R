# ==========================================
# 1. HEATMAP FUNCTION
# ==========================================
#' Plot Heatmap of OJIP Parameters
#'
#' @param res Data frame with OJIP results.
#' @param params Character vector of parameters to plot.
#' @param sample_col Name of the column with sample IDs.
#' @param group_col Name of the column with treatment groups.
#' @param scale Normalization method ("zscore" or "none").
#' @param main Title of the plot.
#' @importFrom stats heatmap
#' @importFrom grDevices colorRampPalette
#' @export
plot_heatmap_fluorojip <- function(res, params, sample_col = "sample_id", group_col = "treatment", scale = "zscore", main = "Normalized JIP-test parameter heatmap") {

  # Extract only the requested numeric columns
  mat <- as.matrix(res[, params])
  rownames(mat) <- res[[sample_col]]

  # Normalize (Z-score)
  if(scale %in% c("zscore", "control_then_zscore")) {
    mat <- scale(mat)
  }

  # Define colors based on the treatment group
  if(group_col %in% names(res)) {
    treatments <- as.factor(res[[group_col]])
    treatment_colors <- c("green", "orange", "purple", "brown", "red")[as.numeric(treatments)]

    heatmap(mat, scale="none", main=main, margins=c(10,10),
            col = colorRampPalette(c("blue", "white", "red"))(100),
            RowSideColors = treatment_colors)
  } else {
    heatmap(mat, scale="none", main=main, margins=c(10,10),
            col = colorRampPalette(c("blue", "white", "red"))(100))
  }
}

#' @rdname plot_heatmap_fluorojip
#' @export
plot_param_heatmap <- plot_heatmap_fluorojip


# ==========================================
# 2. 3D SCATTERPLOT FUNCTION
# ==========================================
#' Plot 3D Scatterplot of OJIP Parameters
#'
#' @param res Data frame with OJIP results.
#' @param params Character vector of exactly 3 parameters to plot.
#' @param group_col Name of the column with treatment groups.
#' @param normalize Logical indicating if data should be normalized.
#' @importFrom graphics legend
#' @export
plot_3d_fluorojip <- function(res, params = c("Fv_Fm", "PI_abs", "area"), group_col = "treatment", normalize = TRUE) {

  if(length(params) != 3) stop("Please provide exactly 3 parameters for the 3D plot.")

  data_3d <- res[, params]

  if(normalize) {
    data_3d <- as.data.frame(scale(data_3d))
  }

  colors <- "blue"
  if(group_col %in% names(res)) {
    treatments <- as.factor(res[[group_col]])
    color_palette <- c("green", "orange", "purple", "brown", "red")
    colors <- color_palette[as.numeric(treatments)]
  }

  if (!requireNamespace("scatterplot3d", quietly = TRUE)) {
    stop("Package 'scatterplot3d' is required. Please install it.")
  }

  s3d <- scatterplot3d::scatterplot3d(
    data_3d[,1], data_3d[,2], data_3d[,3],
    color = colors, pch = 16,
    xlab = params[1], ylab = params[2], zlab = params[3],
    main = "3D OJIP Parameter Space",
    angle = 45
  )

  if(group_col %in% names(res)) {
    legend("topright", legend = levels(treatments),
           col = color_palette[1:length(levels(treatments))], pch = 16)
  }
}

#' @rdname plot_3d_fluorojip
#' @export
plot_param_3d <- plot_3d_fluorojip

#' @rdname plot_3d_fluorojip
#' @export
plot_param_surface3d <- plot_3d_fluorojip
