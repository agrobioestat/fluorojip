#' Example OJIP summary data
#'
#' A dataset containing fluorescence summary values used to demonstrate
#' the calculation of FluorOJIP / JIP-test parameters.
#'
#' The data are organized in the format expected by \code{calc_fluorojip()},
#' including sample identifiers, treatment groups, and summary fluorescence
#' variables such as \code{fo}, \code{fm}, \code{j}, \code{i}, \code{area},
#' and, when available, \code{k}.
#'
#' @format A data frame with columns for sample identification, treatment
#' groups, and fluorescence summary values used in JIP-test calculations.
#' @usage data(example_fluorojip)
"example_fluorojip"

#' Get the bundled Biolyzer validation workbook path
#'
#' Returns the full path to the example Biolyzer workbook distributed with
#' the package in \code{inst/extdata}. This file can be used in
#' validation-oriented workflows that compare FluorOJIP outputs against
#' vendor-calculated JIP-test parameters.
#'
#' @return A length-1 character vector containing the normalized full file path.
#' @details
#' The bundled workbook is intended as a reproducible validation resource for
#' supported Biolyzer-based workflows.
#' @export
fluorojip_example_biolyzer_file <- function() {
  x <- system.file(
    "extdata",
    "OJIPExporttoExcelTest001-01062024at20h31.xls",
    package = "fluorojip"
  )

  if (!nzchar(x)) {
    stop(
      "Could not find the bundled Biolyzer example workbook in inst/extdata.",
      call. = FALSE
    )
  }

  normalizePath(x, winslash = "/", mustWork = TRUE)
}
