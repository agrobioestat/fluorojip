#' Run the fluorojip Shiny application
#'
#' Launches the bundled Shiny interface for interactive FluorOJIP / JIP-test
#' workflows, including data import, parameter calculation, validation,
#' visualization, normalization, and export.
#'
#' @param ... Additional arguments passed to [shiny::runApp()].
#' @return This function is called for its side effect of launching the bundled
#'   Shiny application.
#' @details
#' The bundled Shiny application provides an interactive environment for working
#' with fluorescence summary inputs and supported import workflows. Depending on
#' the installed app version, available features may include OJIP curve
#' inspection, parameter selection, normalized 2D plots, heatmaps, 3D plots,
#' validation tabs, export tools, and built-in help content.
#'
#' This function starts the application distributed with the package and passes
#' any additional arguments directly to [shiny::runApp()].
#' @export
run_fluorojip_app <- function(...) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required. Please install it first.", call. = FALSE)
  }

  app_dir <- system.file("shiny", "fluorojip-app", package = "fluorojip")
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    stop("Could not find the bundled fluorojip Shiny application.", call. = FALSE)
  }

  shiny::runApp(app_dir, ...)
}
