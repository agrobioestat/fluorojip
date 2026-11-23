# fluorojip

![CRAN status](https://www.r-pkg.org/badges/version/fluorojip)
![License: GPL-3](https://img.shields.io/badge/license-GPL--3-blue.svg)
![R](https://img.shields.io/badge/R-%3E%3D%204.1.0-blue.svg)

`fluorojip` is an R package for reproducible calculation of chlorophyll *a*
fluorescence (OJIP) transient parameters of the JIP-test. It converts
exported fluorescence traces into OJIP summary values with documented,
instrument-specific rules, computes the parameters with the equations listed
in `?calc_fluorojip`, flags problematic inputs, compares results with values
exported by instrument software, and provides plots and a Shiny interface.

## Installation

Version 0.2.0 (described in the Royal Society Open Science article) from GitHub:

```r
# install.packages("remotes")
remotes::install_github("agrobioestat/fluorojip@v0.2.0")
```

The CRAN version (currently 0.1.1) is installed with:

```r
install.packages("fluorojip")
```

## From a summary table

```r
library(fluorojip)
res <- calc_fluorojip(example_fluorojip)
res[, c("sample_id", "treatment", "Fv_Fm", "Mo", "ABS_RC", "PI_abs", "qc_flags")]
```

The early O-J point is supplied as `f300us` (the 0.1.x name `k` is still
accepted). Without it, `Mo`, the specific fluxes per reaction centre, and
`PI_abs` are returned as `NA`.

## From exported traces

```r
# Biolyzer (Handy-PEA traces): CSV trace table or Excel export
raw  <- read_biolyzer_xls(fluorojip_example_biolyzer_file())
ojip <- biolyzer_to_ojip(raw)           # preset "biolyzer": 20 us, 270 us, 2 ms, 30 ms
res  <- calc_fluorojip(ojip)

# FluorPen OJIP workbook (Photon Systems Instruments)
xlsx <- system.file("extdata", "FluorPen_test.xlsx", package = "fluorojip")
res_fp <- calc_fluorojip_fluorpen(xlsx) # preset "fluorpen": background subtracted, 50 us, 300 us, 2 ms, 30 ms

# Any other trace: choose or define the time targets explicitly
ojip <- trace_to_ojip(times, mat, preset = "handypea", time_unit = "ms",
                      targets_ms = c(fo = 0.02, f300us = 0.27, j = 2, i = 30))
```

Each extracted table records the times actually used and a `qc_flags` column
(for example `fm_at_last_point`, `fm_before_i`, `fo_time_out_of_tolerance`).
Use `plot_ojip_trace()` to inspect the curves and the extracted points.

## Agreement with instrument software

```r
validate_biolyzer(mode = "engine")$metrics
validate_fluorpen(xlsx, mode = "importer")$metrics
```

## Exploration, export, and Shiny

```r
plot_heatmap_fluorojip(res, params = c("Fv_Fm", "Vj", "Mo", "ABS_RC", "DIo_RC", "PI_abs"))
tab <- normalized_jiptable(res, params = c("Fv_Fm", "PI_abs"), normalize = "zscore")
run_fluorojip_app()
```

## Scope

`fluorojip` works after acquisition. It does not check dark adaptation,
saturation of the excitation pulse, detector saturation, or other measurement
artefacts, and normalized values or clustering displays are exploratory.

## Citation

```r
citation("fluorojip")
```

License: GPL-3. Authors: Joao Everthon da Silva Ribeiro, Toshik Iarley da
Silva, Ronald Maldonado Rodriguez.
