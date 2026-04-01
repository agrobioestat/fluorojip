# fluorojip User Guide: First Steps in R and RStudio

## Purpose

This guide is for newcomers and first-time RStudio users. It shows how to:

1. install R and RStudio
2. open the `fluorojip` project
3. install the packages needed for the project
4. load the package from the project source
5. run the example dataset
6. calculate JIP-test parameters
7. launch the Shiny app
8. make a heatmap and a 3D plot
9. export a normalized JIP parameter table
10. load and analyze your own data files
11. work with supported Biolyzer-exported trace tables
12. work with FluorPen Excel files
13. reproduce bundled validation workflows

This guide is written for Windows because the reviewed setup was on Windows.

`fluorojip` computes chlorophyll *a* fluorescence OJIP / JIP-test parameters from
fluorescence summary data and supported exported trace tables. The package includes
parameter calculation, normalization, exploratory visualization, validation helpers,
and a Shiny interface.

In prose, this guide refers to the performance index as **PI(Abs)**. In code and
output tables, the corresponding variable name is `PI_abs`.

## 1. Install R

Go to the official R website:

- <https://cran.r-project.org/>

For Windows:

1. Click `Download R for Windows`
2. Click `base`
3. Download the current Windows installer
4. Run the installer
5. Accept the default installation settings unless you have a reason to change them

After installation, R is ready to use.

## 2. Install RStudio Desktop

Go to the official RStudio Desktop page:

- <https://posit.co/download/rstudio-desktop/>

On that page:

1. First make sure R is already installed
2. Then download the Windows installer for RStudio Desktop
3. Run the installer
4. Start RStudio

In the reviewed environment, the installed version was:

- RStudio Desktop `2026.01.1 Build 403`

## 3. Open the fluorojip project

The easiest way to work with the package source is to open the project file
`fluorojip.Rproj`.

In RStudio:

1. Click `File -> Open Project...`
2. Select `fluorojip.Rproj`

After opening the project, the Console should be working inside the package folder.
Check with:

```r
getwd()
```

In the reviewed setup, the expected result was:

```r
[1] "D:/FLUOROJIP"
```

If not, set it manually:

```r
setwd("D:/FLUOROJIP")
```

## 4. Install the extra R packages needed

The package source is already in the project folder, but a few extra packages are useful.

Install them once:

```r
install.packages(c("pkgload", "scatterplot3d", "readxl"))
```

What they are used for:

- `pkgload`: load the package directly from the project folder while developing or reviewing
- `scatterplot3d`: required for the static 3D plot
- `readxl`: useful for reading Excel files such as Biolyzer and FluorPen exports

## 5. Load the fluorojip package from the project

In the RStudio Console, run:

```r
options(renv.config.auto.snapshot = FALSE)
pkgload::load_all(".")
```

This loads the package from the current project source.

If you previously ran `source()` on package files and get conflict warnings, the easiest fix is:

1. `Session -> Restart R`
2. Then run:

```r
options(renv.config.auto.snapshot = FALSE)
pkgload::load_all(".")
```

## 6. Run the example dataset

Load the example dataset:

```r
data(example_fluorojip)
example_fluorojip
View(example_fluorojip)
```

Now calculate the JIP-test parameters:

```r
res <- calc_fluorojip(example_fluorojip)
res
View(res)
```

To inspect some main outputs:

```r
res[, c("sample_id", "treatment", "Fv_Fm", "Vj", "Vi", "Mo", "ABS_RC", "TRo_RC", "ETo_RC", "DIo_RC", "PI_abs")]
```

## 7. Launch the Shiny app

If you want a graphical interface with tabs, buttons, validation tools, plots,
and export controls, run:

```r
fluorojip::run_fluorojip_app()
```

From the project root in RStudio, you can also run:

```r
source("launch_fluorojip_app.R")
```

The app includes tabs or workflows for:

- data loading and calculation
- OJIP curve plotting
- parameter selection
- normalized 2D parameter plotting
- heatmap plotting
- 3D plotting
- Biolyzer validation
- FluorPen validation
- export
- help

This is useful for users who prefer an interactive workflow instead of writing all steps manually in scripts.

## 8. What fluorojip calculates

Typical outputs include:

- `Fv_Fm`
- `Vj`
- `Vi`
- `Mo`
- `ABS_RC`
- `TRo_RC`
- `ETo_RC`
- `DIo_RC`
- `phi_Eo`
- `psi_Eo`
- `PI_abs`

To list all result columns:

```r
names(res)
```

For interpretation notes, see the package notes or cheatsheets available in the project.

## 9. Make the heatmap

Run:

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

If the plot is not visible:

1. Check that RStudio has a `Plots` pane
2. Or open a separate graphics window with:

```r
windows()
plot_heatmap_fluorojip(
  res,
  params = c("DIo_RC", "ABS_RC", "PI_abs", "ETo_RC", "Fv_Fm"),
  sample_col = "sample_id",
  group_col = "treatment",
  scale = "zscore",
  main = "fluorojip heatmap"
)
```

What the heatmap shows:

- rows = samples
- columns = selected parameters
- red = relatively higher values
- blue = relatively lower values
- white = values near the dataset average

Important:

- the heatmap usually shows scaled values, not raw values

## 10. Make a 3D scatter plot of OJIP parameters

Run:

```r
windows()
plot_3d_fluorojip(
  res,
  params = c("Fv_Fm", "PI_abs", "area"),
  group_col = "treatment",
  normalize = TRUE
)
```

If you do not want a separate graphics window, remove `windows()` and plot directly.

What the 3D plot shows:

- one point per sample
- axes for `Fv_Fm`, `PI_abs`, and `area`
- point colors by group if `treatment` exists

## 11. Write a normalized JIP parameter table

Run:

```r
tab <- normalized_jiptable(
  res,
  params = c("Fv_Fm", "PI_abs", "ABS_RC", "TRo_RC", "ETo_RC", "DIo_RC"),
  normalize = "zscore",
  output = "wide"
)

View(tab)
write_normalized_jiptable(tab, "normalized_jiptable.csv")
```

This creates a text table that can be opened in Excel or imported into other statistical software.

Important:

- the current export helper writes a **semicolon-delimited** table
- depending on your Excel regional settings, this may open correctly by default
- if not, import the file as a delimited text file and select semicolon as the separator

## 12. Use your own data: summary table route

The easiest way to use your own data is to create a CSV file with one row per sample.

For reliable `PI_abs` and RC-based fluxes, your file should include at least:

- `fo` or `o`
- `k` or `f300us`
- `j`
- `i`
- `fm` or `p`

Optional but useful:

- `area`
- `t_fm`
- `sample_id`
- `treatment`

Example CSV structure:

```text
sample_id,treatment,t_fm,area,fo,k,j,i,fm
S1,control,230,22974.78,1287,2030,2450,3136,3224
S2,stress,70,49706.28,964,1935,2745,4028,4073
```

If your file is comma-separated:

```r
mydata <- read.csv("my_ojip_summary.csv", stringsAsFactors = FALSE)
res_my <- calc_fluorojip(mydata)
View(res_my)
```

If your file uses semicolons:

```r
mydata <- read.csv("my_ojip_summary.csv", sep = ";", stringsAsFactors = FALSE)
res_my <- calc_fluorojip(mydata)
View(res_my)
```

## 13. Use your own data: Excel route

If your data are in Excel, you can read them with `readxl`.

Example:

```r
library(readxl)

mydata <- read_excel("my_ojip_summary.xlsx", sheet = 1)
mydata <- as.data.frame(mydata, stringsAsFactors = FALSE)

res_my <- calc_fluorojip(mydata)
View(res_my)
```

If the Excel file contains raw vendor outputs rather than a ready summary table, you may need a small preparation step to rename the relevant columns to:

- `fo`
- `k`
- `j`
- `i`
- `fm`
- optional `area`, `t_fm`, `sample_id`, `treatment`

## 14. Use your own data: Biolyzer export workflow

If you have a supported **Biolyzer-exported CSV trace table**, you can process it as follows:

```r
raw <- read_handypea_csv("my_biolyzer_export.csv")
ojip <- handypea_to_ojip(raw)
res_my <- calc_fluorojip(ojip)
View(ojip)
View(res_my)
```

Important:

- although some helper function names retain the historical `handypea` prefix for backward compatibility, the supported import workflow is based on **Biolyzer-exported trace tables**, not direct parsing of proprietary raw instrument files
- the raw-trace importer supports traces whose time axis is in seconds, milliseconds, or microseconds
- it targets standard OJIP positions near:
  - O at `0.02 ms`
  - K at `0.27 ms`
  - J at `2 ms`
  - I at `30 ms`

## 15. Run the Biolyzer validation example

If you want to reproduce the Biolyzer comparison used in the review:

1. Get the bundled workbook path:

```r
fluorojip_example_biolyzer_file()
```

2. Then run:

```r
source("tools/compare_biolyzer_pi_abs.R")
```

This writes the comparison output to a CSV file in the `tools` folder.

The comparison script automatically looks for the bundled workbook in:

```r
inst/extdata/OJIPExporttoExcelTest001-01062024at20h31.xls
```

For an installed package, the equivalent lookup is:

```r
system.file(
  "extdata",
  "OJIPExporttoExcelTest001-01062024at20h31.xls",
  package = "fluorojip"
)
```

There is also a more general validator:

```r
source("tools/validate_handypea_against_reference.R")
```

This helper was designed for future comparisons against reference tables from external software.

## 16. Use FluorPen Excel files

The package can also process FluorPen `.xlsx` exports.

These files have a specific structure:

- one column per measurement
- the first column stores the OJIP time grid
- the top rows contain measurement metadata
- the footer rows contain vendor-calculated JIP parameters such as `Fv/Fm`, `Mo`, `Pi_Abs`, `ABS/RC`, `ETo/RC`, and `DIo/RC`

To read one FluorPen workbook:

```r
raw_fp <- read_fluorpen_xlsx("fluorpen/FluorPen - test 2.xlsx")
ojip_fp <- fluorpen_to_ojip(raw_fp)
res_fp <- calc_fluorojip(ojip_fp)

View(raw_fp$summary_numeric)
View(res_fp)
```

To run the project-local FluorPen validation:

```r
source("tools/compare_fluorpen_validation.R")
```

This writes a comparison CSV file in the `tools` folder.

The FluorPen reader currently targets:

- O near `0.02 ms`
- K at `0.27 ms`
- J at `2 ms`
- I at `30 ms`

The FluorPen exports reviewed here use a microsecond time grid, so the reader converts:

- `21` to `0.021 ms`
- `271` to `0.271 ms`
- `2001` to `2.001 ms`
- `30001` to `30.001 ms`

## 17. Minimal copy-paste script

If you want one quick script to run the example from scratch in RStudio:

```r
options(renv.config.auto.snapshot = FALSE)
setwd("D:/FLUOROJIP")
pkgload::load_all(".")

data(example_fluorojip)
res <- calc_fluorojip(example_fluorojip)

print(
  res[, c("sample_id", "treatment", "Fv_Fm", "Vj", "Vi", "Mo", "ABS_RC", "TRo_RC", "ETo_RC", "DIo_RC", "PI_abs")]
)

plot_heatmap_fluorojip(
  res,
  params = c("DIo_RC", "ABS_RC", "PI_abs", "ETo_RC", "Fv_Fm"),
  sample_col = "sample_id",
  group_col = "treatment",
  scale = "zscore",
  main = "fluorojip heatmap"
)

windows()
plot_3d_fluorojip(
  res,
  params = c("Fv_Fm", "PI_abs", "area"),
  group_col = "treatment",
  normalize = TRUE
)

tab <- normalized_jiptable(
  res,
  params = c("Fv_Fm", "PI_abs", "ABS_RC", "TRo_RC", "ETo_RC", "DIo_RC"),
  normalize = "zscore",
  output = "wide"
)

write_normalized_jiptable(tab, "normalized_jiptable.csv")
```

## 18. Common problems and fixes

### Problem: `pkgload::load_all(".")` cannot find `DESCRIPTION`

Cause:

- you are not in the project folder

Fix:

```r
setwd("D:/FLUOROJIP")
pkgload::load_all(".")
```

### Problem: plot functions seem to run but no plot appears

Cause:

- RStudio `Plots` pane is hidden
- or graphics are opening outside the main IDE

Fix:

- check `Tools -> Global Options -> Pane Layout`
- make sure one pane contains `Plots`
- or use `windows()` before the plotting function

### Problem: RStudio gives graphics-state or shadow-device errors

Cause:

- unstable graphics device in the current session

Fix:

1. restart RStudio
2. reopen the project
3. run:

```r
options(renv.config.auto.snapshot = FALSE)
pkgload::load_all(".")
```

### Problem: `plot_3d_fluorojip()` says `scatterplot3d` is required

Fix:

```r
install.packages("scatterplot3d")
```

## 19. Final notes

The current `fluorojip` package:

- computes OJIP / JIP-test parameters from summary inputs and supported exported trace tables
- includes validation workflows based on Biolyzer and FluorPen example files
- provides exploratory plots and a Shiny interface
- exports normalized parameter tables for further analysis

For the broader scientific review, see the project review report if it is included in your working copy.

## Sources for installation guidance

Official download pages used for the setup guidance in this document:

- CRAN R download page: <https://cran.r-project.org/>
- Posit RStudio Desktop download page: <https://posit.co/download/rstudio-desktop/>

## References and supporting packages

The scientific interpretation of OJIP / JIP-test parameters in `fluorojip`
is aligned with standard references such as Stirbet and Govindjee (2011),
Strasser et al. (1995), Strasser et al. (2004), and Ripoll et al. (2016).

The package also relies on the R ecosystem and supporting packages including
`readxl`, `shiny`, `scatterplot3d`, `rmarkdown`, and `testthat`.

For package-specific citation information, run:

```r
citation("fluorojip")
