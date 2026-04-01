# Script to launch the app locally (Ignored by CRAN)
if (!requireNamespace("pkgload", quietly = TRUE)) {
     stop("Please install 'pkgload' first with install.packages('pkgload').", call. = FALSE)
}

script_dir <- function() {
     file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
     if (length(file_arg) > 0) {
          return(dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = TRUE)))
     }
     normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

# Load package functions
pkgload::load_all(script_dir())

# Launch the app
run_fluorojip_app()
