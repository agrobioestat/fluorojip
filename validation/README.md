# Validation of fluorojip 0.2.0

Scripts and outputs of the validation reported in the Royal Society Open Science
article on fluorojip (da Silva Ribeiro, da Silva & Maldonado Rodriguez).

- `run_validation.R`: reproduces all validation results from the reference files
  bundled with the package (`inst/extdata`). Usage: `Rscript run_validation.R <output_dir>`
- `make_figures.R`: reproduces figures 1, 2, 3, 5 and 6 of the article.
  Usage: `Rscript make_figures.R run_validation.R <output_dir>`
- `validation_output/`: agreement metrics, sample-level comparisons, OJIP input
  values, convention-sensitivity results, figures, MD5 checksums of the reference
  files (`input_md5.txt`) and the R session information (`sessionInfo.txt`).

The `validation/` folder is excluded from the CRAN build (`.Rbuildignore`).
