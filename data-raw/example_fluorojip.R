example_fluorojip <- utils::read.csv(
  "data-raw/example_fluorojip.csv",
  sep = ";",
  stringsAsFactors = FALSE
)

usethis::use_data(example_fluorojip, overwrite = TRUE)

