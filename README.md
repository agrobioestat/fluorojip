fluorojip
![CRAN status](https://www.r-pkg.org/badges/version/fluorojip)
![License: GPL-3](https://img.shields.io/badge/license-GPL--3-blue.svg)
![R](https://img.shields.io/badge/R-%3E%3D%204.1.0-blue.svg)
`fluorojip` is an R package for reproducible analysis of chlorophyll a fluorescence OJIP transients and JIP-test parameters.
The package computes fluorescence-derived parameters from summary data, supports selected vendor-exported trace workflows, provides normalization and visualization helpers, and includes an optional Shiny interface for interactive analysis.
Installation
Install the released version from CRAN:
```r
install.packages("fluorojip")
```
Load the package:
```r
library(fluorojip)
```
Main features
calculation of OJIP/JIP-test parameters from fluorescence summary tables;
support for common fluorescence descriptors such as `fo`, `fm`, `j`, `i`, `k`, and `area`;
conservative handling of parameters that require K-step or 300-us-equivalent fluorescence information;
helper functions for supported Biolyzer-exported CSV trace tables;
helper functions for supported FluorPen Excel workbooks;
normalization of JIP-test parameter tables using z-score, min-max, control ratio, or control-then-z-score approaches;
heatmap and three-dimensional exploratory visualization;
export of normalized tables for downstream analysis;
bundled Shiny application for interactive workflows.
Basic example
```r
library(fluorojip)

df <- data.frame(
  sample_id = c("S1", "S2", "S3"),
  treatment = c("control", "stress", "stress"),
  fo = c(280, 300, 295),
  fm = c(1200, 1250, 1230),
  j = c(700, 730, 720),
  i = c(950, 980, 970),
  k = c(340, 360, 350),
  area = c(32000, 35000, 34000)
)

res <- calc_fluorojip(df)

res[, c("sample_id", "treatment", "Fv_Fm", "PI_abs")]
```
Normalized parameter table
```r
tab <- normalized_jiptable(
  res,
  params = c("Fv_Fm", "PI_abs", "ABS_RC", "TRo_RC", "ETo_RC", "DIo_RC"),
  group_col = "treatment",
  normalize = "zscore",
  output = "wide"
)

tab
```
Heatmap
```r
plot_heatmap_fluorojip(
  res,
  params = c("Fv_Fm", "PI_abs", "ABS_RC", "TRo_RC", "ETo_RC", "DIo_RC"),
  group_col = "treatment",
  scale = "zscore"
)
```
Three-dimensional exploratory plot
```r
if (requireNamespace("scatterplot3d", quietly = TRUE)) {
  plot_3d_fluorojip(
    res,
    params = c("Fv_Fm", "PI_abs", "ABS_RC"),
    group_col = "treatment",
    normalize = TRUE
  )
}
```
Supported import workflows
Summary-table workflow
```r
res <- calc_fluorojip_file("summary_table.csv", sep = ";", dec = ".")
```
Biolyzer-exported CSV workflow
```r
raw <- read_handypea_csv("biolyzer_export.csv")
ojip <- handypea_to_ojip(raw)
res <- calc_fluorojip(ojip)
```
or directly:
```r
res <- calc_fluorojip_handypea("biolyzer_export.csv")
```
FluorPen Excel workflow
```r
raw <- read_fluorpen_xlsx("FluorPen_export.xlsx")
ojip <- fluorpen_to_ojip(raw)
res <- calc_fluorojip(ojip)
```
or directly:
```r
res <- calc_fluorojip_fluorpen("FluorPen_export.xlsx")
```
Shiny application
The package includes a Shiny interface for users who prefer an interactive workflow:
```r
run_fluorojip_app()
```
Documentation
Package documentation is available on CRAN:
https://CRAN.R-project.org/package=fluorojip
Citation
To cite the package in publications, use:
```r
citation("fluorojip")
```
License
This package is distributed under the GPL-3 license.
Authors
Joao Everthon da Silva Ribeiro,
Toshik Iarley da Silva,
Ronald Maldonado Rodriguez
