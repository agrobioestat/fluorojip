# fluorojip

[![License: GPL-3](https://img.shields.io/badge/License-GPL--3-blue.svg)](https://opensource.org/licenses/GPL-3.0)
[![GitHub](https://img.shields.io/badge/GitHub-agrobiostat%2Ffluorojip-black?logo=github)](https://github.com/agrobiostat/fluorojip)

**Analysis of Chlorophyll *a* Fluorescence Transient Parameters in R**

`fluorojip` is an R package for computing **chlorophyll *a* fluorescence OJIP / JIP-test parameters** from fluorescence summary data and supported exported trace tables. The package also provides tools for **normalization**, **heatmaps**, **3D exploratory plots**, **validation-oriented workflows**, and an **interactive Shiny application** for users who prefer a graphical interface.

The main goal of `fluorojip` is to support reproducible analysis of PSII performance from fluorescence measurements, from raw imported files to ready-to-plot parameter tables.

---

## Main features

- Compute core OJIP / JIP-test parameters from fluorescence summary inputs
- Support workflows based on **summary tables**, **Biolyzer-exported CSV trace tables**, and **FluorPen Excel exports**
- Generate derived parameters such as `Fv_Fm`, `Vj`, `Vi`, `Mo`, `phi_Eo`, `psi_Eo`, `ABS_RC`, `TRo_RC`, `ETo_RC`, `DIo_RC`, `PI_abs`, `Sm`, and `N`
- Build normalized tables for exploratory analysis and downstream statistics
- Create heatmaps and 3D plots for multivariate interpretation
- Export normalized results for spreadsheet and reporting workflows
- Launch an interactive Shiny app for data import, calculation, plotting, validation, and export
- Use bundled example files for testing and validation

---

## Installation

### From CRAN

After the CRAN release, install it with:

```r
install.packages("fluorojip")
```

### From GitHub

If you want the development version from GitHub:

```r
install.packages("remotes")
remotes::install_github("agrobiostat/fluorojip")
```


### Optional packages

Some features rely on optional packages suggested by `fluorojip`:

```r
install.packages(c("scatterplot3d", "shiny"))
```

- `scatterplot3d` is used for static 3D plots
- `shiny` is required to launch the interactive app

---

## Quick start

```r
library(fluorojip)

data(example_fluorojip)

res <- calc_fluorojip(example_fluorojip)

head(
  res[, c("sample_id", "treatment", "Fv_Fm", "Vj", "Vi", "Mo", "PI_abs")]
)
```

This workflow uses the bundled example dataset and calculates a standard set of fluorescence-derived parameters.

---

## Supported workflows

### 1. Summary-table workflow

Use this workflow when you already have a summary table containing at least `Fo`, `Fm`, `J`, and `I` values.

```r
library(fluorojip)

res <- calc_fluorojip(example_fluorojip)
head(res)
```

You can also calculate directly from a delimited file:

```r
res <- calc_fluorojip_file("my_summary_table.csv", sep = ",")
```

### 2. Biolyzer-exported trace workflow

For supported **Biolyzer-exported CSV trace tables**, the recommended workflow is:

```r
raw  <- read_handypea_csv("biolyzer_export.csv")
ojip <- handypea_to_ojip(raw)
res  <- calc_fluorojip(ojip)
```

Or, using the convenience wrapper:

```r
res <- calc_fluorojip_handypea("biolyzer_export.csv")
```

> The function names retain the historical `handypea` prefix for backward compatibility, but the current supported workflow is based on **Biolyzer-exported trace tables**.

### 3. FluorPen workflow

For supported **FluorPen Excel exports**:

```r
raw  <- read_fluorpen_xlsx("fluorpen_export.xlsx")
ojip <- fluorpen_to_ojip(raw)
res  <- calc_fluorojip(ojip)
```

Or, using the convenience wrapper:

```r
res <- calc_fluorojip_fluorpen("fluorpen_export.xlsx")
```

---

## Key outputs

`fluorojip` can compute and organize several important parameters for OJIP / JIP-test interpretation, including:

### Primary fluorescence and relative variable fluorescence

- `Fv`
- `phi_Po`
- `Fv_Fm`
- `Vj`
- `Vi`

### Structure and turnover-related terms

- `Mo`
- `Sm`
- `N`

### Fluxes per reaction center

- `ABS_RC`
- `TRo_RC`
- `ETo_RC`
- `DIo_RC`

### Fluxes per cross section

- `ABS_CSm`
- `TRo_CSm`
- `ETo_CSm`
- `DIo_CSm`

### Performance and efficiencies

- `phi_Eo`
- `psi_Eo`
- `PI_abs`

---

## Important notes for users

- `Fo`, `Fm`, `J`, and `I` are the minimum required inputs for the core calculation workflow.
- Reliable calculation of `Mo`, `N`, RC-based fluxes, and `PI_abs` requires a **K-step / F300 / 300 µs-equivalent measurement** provided as `k`, `fk`, `f300`, `f300us`, `f_300us`, or `f_300_us`.
- When K-step / F300 information is not available, `fluorojip` does **not** guess those values. Instead, the package returns `NA` for dependent terms and issues a warning.
- Cross-section outputs such as `ABS_CSm`, `TRo_CSm`, `ETo_CSm`, and `DIo_CSm` are useful package outputs for comparison workflows, but they should be interpreted carefully when compared with instrument-specific conventions.

---

## Normalization and export

The package provides tools to prepare parameter tables for comparison across samples and treatments.

```r
norm_tab <- normalized_jiptable(
  res,
  params = c("Fv_Fm", "PI_abs", "ABS_RC", "ETo_RC", "DIo_RC"),
  sample_col = "sample_id",
  group_col = "treatment",
  normalize = "zscore",
  output = "wide"
)

write_normalized_jiptable(norm_tab, "fluorojip_normalized.csv")
```

Supported normalization modes include:

- `"none"`
- `"zscore"`
- `"minmax"`
- `"control_ratio"`
- `"control_then_zscore"`

---

## Visualization

### Heatmap

```r
plot_heatmap_fluorojip(
  res,
  params = c("DIo_RC", "ABS_RC", "PI_abs", "ETo_RC", "Fv_Fm"),
  sample_col = "sample_id",
  group_col = "treatment",
  scale = "zscore",
  main = "fluorojip heatmap"
)
```

### 3D exploratory plot

```r
plot_3d_fluorojip(
  res,
  params = c("Fv_Fm", "PI_abs", "area")
)
```

These plotting functions are useful for identifying multivariate patterns across treatments and samples.

---

## Interactive Shiny app

To launch the bundled graphical interface:

```r
run_fluorojip_app()
```

The Shiny app provides an interactive environment for:

- loading example data, summary tables, and FluorPen workbooks
- calculating OJIP / JIP-test parameters
- inspecting OJIP curves
- selecting and plotting parameters
- generating normalized plots, heatmaps, and 3D views
- running validation-oriented workflows
- exporting results

This interface is especially helpful for users who prefer not to build the full analysis pipeline manually in scripts.

---

## Bundled example resources

`fluorojip` includes example datasets and files that help users test workflows and validate outputs:

- `example_fluorojip` — example summary dataset included in the package
- `fluorojip_example_biolyzer_file()` — returns the bundled Biolyzer validation workbook path
- bundled example files in `inst/extdata/` for reproducible testing and validation

Example:

```r
library(fluorojip)

biolyzer_file <- fluorojip_example_biolyzer_file()
biolyzer_file
```

---

## Example analysis workflow

```r
library(fluorojip)

data(example_fluorojip)

res <- calc_fluorojip(example_fluorojip)

plot_heatmap_fluorojip(
  res,
  params = c("Fv_Fm", "PI_abs", "ABS_RC", "ETo_RC", "DIo_RC"),
  sample_col = "sample_id",
  group_col = "treatment",
  scale = "zscore",
  main = "OJIP parameter heatmap"
)

norm_tab <- normalized_jiptable(
  res,
  params = c("Fv_Fm", "PI_abs", "ABS_RC", "ETo_RC", "DIo_RC"),
  sample_col = "sample_id",
  group_col = "treatment",
  normalize = "zscore",
  output = "wide"
)

head(norm_tab)
```

---

## Scientific background

The package is grounded in the OJIP / JIP-test framework widely used for the interpretation of chlorophyll *a* fluorescence induction curves and PSII performance.

Key references include:

- Ripoll, J., Bertin, N., Bidel, L. P. R., & Urban, L. (2016). *A user’s view of the parameters derived from the induction curves of maximal chlorophyll a fluorescence: Perspectives for analyzing stress*. Frontiers in Plant Science, 7, 1679.
- Stirbet, A., & Govindjee. (2011). *On the relation between the Kautsky effect (chlorophyll a fluorescence induction) and photosystem II: Basics and applications of the OJIP fluorescence transient*. Journal of Photochemistry and Photobiology B: Biology, 104(1-2), 236-257.
- Strasser, R. J., Srivastava, A., & Govindjee. (1995). *Polyphasic chlorophyll a fluorescence transient in plants and cyanobacteria*. Photochemistry and Photobiology, 61(1), 32-42.
- Strasser, R. J., Tsimilli-Michael, M., & Srivastava, A. (2004). *Analysis of the chlorophyll a fluorescence transient*. In *Chlorophyll a fluorescence: A signature of photosynthesis*.

---

## License

`fluorojip` is distributed under the **GPL-3** license.

---

## Authors

- **Joao Everthon da Silva Ribeiro** — Author and maintainer (j.everthon@hotmail.com)
- **Toshik Iarley da Silva** — Author
- **Ronald Maldonado Rodriguez** — Author

---

## Final note

`fluorojip` was designed to provide a practical and reproducible bridge between fluorescence measurements, JIP-test parameter calculation, exploratory visualization, and export-ready outputs. It is suitable for researchers, students, and analysts working with chlorophyll fluorescence data and PSII performance assessment.
