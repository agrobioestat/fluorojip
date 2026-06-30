## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(fluorojip)

## -----------------------------------------------------------------------------
data(example_fluorojip)
head(example_fluorojip)

## -----------------------------------------------------------------------------
res <- calc_fluorojip(example_fluorojip)
head(res)

## -----------------------------------------------------------------------------
params <- c("phi_Po", "PI_abs", "ABS_RC", "DIo_RC", "ETo_CSm")

plot_param_heatmap(
  res,
  group_col = "treatment",
  params    = params,
  scale     = "zscore",
  main      = "JIP-Test Parameters Heatmap"
)

## -----------------------------------------------------------------------------
plot_3d_fluorojip(res, params = c("Fv_Fm", "PI_abs", "area"))

## -----------------------------------------------------------------------------
# raw  <- read_handypea_csv("path/to/biolyzer_export.csv")
# ojip <- handypea_to_ojip(raw)
# res_biolyzer <- calc_fluorojip(ojip)

## -----------------------------------------------------------------------------
# fluorpen_data <- read_excel("path/to/fluorpen_export.xlsx")
# res_fluorpen  <- calc_fluorojip(fluorpen_data)

## -----------------------------------------------------------------------------
# run_fluorojip_app()

