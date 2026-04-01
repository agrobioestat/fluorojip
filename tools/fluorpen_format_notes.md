# FluorPen Format Notes

## Purpose

This note describes the structure of the FluorPen Excel files reviewed in:

- `D:/FLUOROJIP/fluorpen`

and explains how `fluorojip` now reads them for secondary external validation.

Biolyzer remains the primary validation reference for the package. The FluorPen files are useful as an additional vendor-export comparison source.

## Files Reviewed

The reviewed FluorPen workbooks were:

- `FluorPen - test 1.xlsx`
- `FluorPen - test 2.xlsx`
- `FluorPen - test 3.xlsx`

The folder also contains:

- `FluorPen_Manual.pdf`

The file structure was inferred mainly from the `.xlsx` exports themselves.

## Workbook Structure

The FluorPen exports use a wide ASCII-style table layout inside Excel:

- one sheet per workbook
- one measurement per column
- the first column stores the OJIP time grid
- the first rows contain measurement metadata
- the last rows contain vendor-calculated summary parameters

The key header rows are:

- row with `index` or `index/Leitura`: measurement numbers
- row with `time`: acquisition timestamps
- row with `id`: protocol labels such as `OJIP`

The raw fluorescence trace starts immediately after those header rows.

## Time Axis

The first column is the OJIP trace time axis.

In the reviewed FluorPen files, this axis is in microseconds:

- `11`
- `21`
- `31`
- `...`
- `271`
- `2001`
- `30001`
- `230001`

For `fluorojip`, these are interpreted as:

- `11 us`
- `21 us`
- `31 us`
- `271 us`
- `2001 us`
- `30001 us`
- `230001 us`

or, in milliseconds:

- `0.011 ms`
- `0.021 ms`
- `0.031 ms`
- `0.271 ms`
- `2.001 ms`
- `30.001 ms`
- `230.001 ms`

This is important because the package targets:

- O near `0.02 ms`
- K at `0.27 ms`
- J at `2 ms`
- I at `30 ms`

## Footer Summary Parameters

The footer rows contain vendor-calculated quantities including:

- `Fm`
- `Fv`
- `Vj`
- `Vi`
- `Fm/Fo`
- `Fv/Fo`
- `Fv/Fm`
- `Mo`
- `Area`
- `Sm`
- `N`
- `Phi_Po`
- `Psi_o`
- `Phi_Eo`
- `Phi_Do`
- `Phi_Pav`
- `Pi_Abs`
- `ABS/RC`
- `TRo/RC`
- `ETo/RC`
- `DIo/RC`

This makes the format suitable for direct comparison between:

- vendor summary values
- `fluorojip` values calculated from the raw traces

## Formatting Caveat

`FluorPen - test 1.xlsx` contains inconsistent footer number formatting.

Examples observed:

- values already written with decimals, such as `0.775`
- values apparently missing decimal points, such as `4145` for `Pi_Abs`
- mixed formatting within the same parameter row

Because of this, the FluorPen reader includes cautious normalization heuristics for selected vendor summary fields such as:

- `Fv/Fm`
- `Mo`
- `Pi_Abs`
- `ABS/RC`
- `TRo/RC`
- `ETo/RC`
- `DIo/RC`

These heuristics are only used to interpret the vendor footer for validation. They do not affect the core `fluorojip` formulas.

## New fluorojip Support Added

The package now includes:

- `read_fluorpen_xlsx()`
- `fluorpen_to_ojip()`
- `calc_fluorojip_fluorpen()`

These functions are implemented in:

- [fluorpen_io.R](D:/FLUOROJIP/R/fluorpen_io.R)

The generic raw-trace reader logic was also extended in:

- [handypea_io.R](D:/FLUOROJIP/R/handypea_io.R)

so that raw traces can now be interpreted in:

- seconds
- milliseconds
- microseconds

## Validation Workflow

To read one FluorPen workbook:

```r
pkgload::load_all("D:/FLUOROJIP")

raw_fp <- read_fluorpen_xlsx("D:/FLUOROJIP/fluorpen/FluorPen - test 2.xlsx")
ojip_fp <- fluorpen_to_ojip(raw_fp)
res_fp <- calc_fluorojip(ojip_fp)
```

To run the folder-wide project-local comparison:

```r
source("D:/FLUOROJIP/tools/compare_fluorpen_validation.R")
```

This writes:

- [fluorpen_comparison.csv](D:/FLUOROJIP/tools/fluorpen_comparison.csv)

## Validation Interpretation

The FluorPen comparison is useful as a secondary validation layer.

It shows that:

- the raw trace format can be parsed reliably
- the O, K, J, I, and P steps can be extracted consistently
- `fluorojip` outputs are generally close to the FluorPen vendor footer values

But it is not as strong a reference as the Biolyzer comparison because:

- the reviewed FluorPen footer contains formatting inconsistencies
- one reviewed trace is invalid flat data
- the vendor-export representation itself required normalization heuristics

Therefore:

- Biolyzer remains the primary external validation reference
- FluorPen now serves as a useful secondary validation source and import format

