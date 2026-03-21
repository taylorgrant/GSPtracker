library(readxl)
library(usethis)

creative_benchmarks_2025 <- list(
  Overall = readxl::read_excel(
    "data-raw/benchmarks_2025.xlsx",
    sheet = "overall"
  ),
  Women = readxl::read_excel(
    "data-raw/benchmarks_2025.xlsx",
    sheet = "women"
  ),
  AAPI = readxl::read_excel(
    "data-raw/benchmarks_2025.xlsx",
    sheet = "aapi"
  ),
  GenZMill = readxl::read_excel(
    "data-raw/benchmarks_2025.xlsx",
    sheet = "genz_mill"
  )
) ## code to prepare `DATASET` dataset goes here

usethis::use_data(creative_benchmarks_2025, overwrite = TRUE)
