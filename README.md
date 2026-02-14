# fluorojip

<!-- badges: start -->
[![License: GPL-3](https://img.shields.io/badge/License-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![GitHub repo](https://img.shields.io/badge/GitHub-agrobioestat%2Ffluorojip-black?logo=github)](https://github.com/agrobioestat/fluorojip)
<!-- badges: end -->

`fluorojip` computes **chlorophyll *a* fluorescence OJIP (JIP-test) parameters** from raw fluorescence summary data and provides utilities for **normalization** and **exploratory visualization** (heatmaps and 3D scatterplots).

It is designed for PSII performance assessment using inputs such as **Fo**, **Fm**, **area above the OJIP curve**, and fluorescence at the **O, J, I, P** steps.

---

## Features

- Compute common JIP-test indices (yields, efficiencies, performance index, fluxes per RC/CS)
- Read and process **HandyPEA** CSV exports (convert to an OJIP summary, then compute indices)
- Create:
  - **Heatmaps** of selected parameters across samples/treatments
  - **3D scatterplots** for multivariate exploration
- Build normalized parameter tables (z-score, min–max, control ratio, etc.) and export to CSV

---

## Installation

### From GitHub (development version)

```r
# install.packages("remotes")
remotes::install_github("agrobioestat/fluorojip")
```

### From CRAN (when available)

```r
install.packages("fluorojip")
```

### From a local source tarball

```r
install.packages("fluorojip_0.1.1.tar.gz", repos = NULL, type = "source")
```

---

## Quick start

### 1) Load example data and compute indices

```r
library(fluorojip)

data(example_fluorojip)

res <- calc_fluorojip(example_fluorojip)
head(res)
```

### 2) Heatmap of selected parameters

```r
params <- c("Fv_Fm", "PI_abs", "ABS_RC", "ETo_RC", "DIo_RC")

plot_heatmap_fluorojip(
  res,
  params     = params,
  sample_col = "sample_id",
  group_col  = "treatment",
  scale      = "zscore",
  main       = "OJIP parameters (z-score)"
)
```

### 3) 3D scatterplot

```r
plot_3d_fluorojip(
  res,
  params    = c("Fv_Fm", "PI_abs", "area"),
  group_col = "treatment",
  normalize = TRUE
)
```

---

## Input data format

`calc_fluorojip()` expects a data frame with (at minimum):

- `t_fm` – time to reach Fm  
- `area` – area above the induction curve (commonly integrated up to Fm)  
- `j`, `i`, `s` – fluorescence at J, I, and S steps  

And either:

- `fo` **or** `o` (Fo / O step)  
- `fm` **or** `p` (Fm / P step)

Optional (recommended for plotting and tidy outputs):

- `sample_id` – sample identifier  
- `treatment` – group/treatment label

---

## HandyPEA workflow (optional)

If you have a HandyPEA CSV export, you can compute indices directly:

```r
res_hp <- calc_fluorojip_handypea("path/to/handypea_export.csv")
head(res_hp)
```

Or run step-by-step:

```r
raw  <- read_handypea_csv("path/to/handypea_export.csv")
ojip <- handypea_to_ojip(raw)      # builds a summary table (Fo, Fm, J, I, area, ...)
res  <- calc_fluorojip(ojip)
```

---

## Normalized parameter tables

Build a wide or long table with optional normalization:

```r
tab_wide <- normalized_jiptable(
  res,
  params    = c("Fv_Fm", "PI_abs", "ABS_RC", "TRo_RC", "ETo_RC", "DIo_RC"),
  normalize = "zscore",            # "none", "zscore", "minmax", "control_ratio", "control_then_zscore"
  output    = "wide"
)

head(tab_wide)
```

Export to CSV:

```r
write_normalized_jiptable(tab_wide, "normalized_jiptable.csv")
```

---

## What gets computed

`calc_fluorojip()` returns the original data frame plus common JIP-test variables, including:

- Basic: `Fv`, `phi_Po` (`Fv_Fm`)
- Relative variable fluorescence: `Vj`, `Vi`
- Structure/turnover: `Mo`, `Sm`, `N`
- Fluxes per reaction center: `ABS_RC`, `TRo_RC`, `ETo_RC`, `DIo_RC`
- Fluxes per cross-section: `ABS_CSm`, `TRo_CSm`, `ETo_CSm`, `DIo_CSm`
- Efficiencies: `psi_Eo`, `phi_Eo`
- Performance index: `PI_abs`
- Additional: `Kn`, `Kp`

---

## Documentation

- Function help: `?calc_fluorojip`, `?plot_heatmap_fluorojip`, `?normalized_jiptable`
- Vignette (if installed with vignettes): `vignette("fluorojip-intro")`

---

## Citation

If you use `fluorojip` in academic work, please cite the package and its authors.

In R:

```r
citation("fluorojip")
```

Authors:
- Joao Everthon da Silva Ribeiro  
- Ronald Maldonado Rodriguez
- Toshik Iarley da Silva  

---

## License

GPL-3. See `LICENSE` or package `DESCRIPTION`.

## Contributing / Issues

Bug reports and feature requests: <https://github.com/agrobioestat/fluorojip/issues>
