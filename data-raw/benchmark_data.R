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
  ),
  Millennials = readxl::read_excel(
    "data-raw/benchmarks_2025.xlsx",
    sheet = "millennials"
  ),
  GenZ = readxl::read_excel(
    "data-raw/benchmarks_2025.xlsx",
    sheet = "gen_z"
  )
)

usethis::use_data(creative_benchmarks_2025, overwrite = TRUE)

# BMW benchmarks 2025
bmw_benchmarks_2025 <- list(
  Overall = readxl::read_excel(
    "data-raw/bmw_benchmarks_2025.xlsx",
    sheet = "bmw_overall"
  ),
  Women = readxl::read_excel(
    "data-raw/bmw_benchmarks_2025.xlsx",
    sheet = "bmw_female"
  ),
  AAPI = readxl::read_excel(
    "data-raw/bmw_benchmarks_2025.xlsx",
    sheet = "bmw_aapi"
  ),
  GenZMill = readxl::read_excel(
    "data-raw/bmw_benchmarks_2025.xlsx",
    sheet = "bmw_zm"
  ),
  Millennials = readxl::read_excel(
    "data-raw/bmw_benchmarks_2025.xlsx",
    sheet = "bmw_millennial"
  ),
  GenZ = readxl::read_excel(
    "data-raw/bmw_benchmarks_2025.xlsx",
    sheet = "bmw_genz"
  )
)

usethis::use_data(bmw_benchmarks_2025, overwrite = TRUE)

# BMW Quarterly Benchmarks
bmw_qtrly_benchmarks_2025 <- list(
  Overall = readxl::read_excel(
    "data-raw/bmw_qtrly_benchmarks_2025.xlsx",
    sheet = "bmw_overallq"
  ),
  Women = readxl::read_excel(
    "data-raw/bmw_qtrly_benchmarks_2025.xlsx",
    sheet = "bmw_femaleq"
  ),
  AAPI = readxl::read_excel(
    "data-raw/bmw_qtrly_benchmarks_2025.xlsx",
    sheet = "bmw_aapiq"
  ),
  GenZMill = readxl::read_excel(
    "data-raw/bmw_qtrly_benchmarks_2025.xlsx",
    sheet = "bmw_zmq"
  ),
  Millennials = readxl::read_excel(
    "data-raw/bmw_qtrly_benchmarks_2025.xlsx",
    sheet = "bmw_millennialq"
  ),
  GenZ = readxl::read_excel(
    "data-raw/bmw_qtrly_benchmarks_2025.xlsx",
    sheet = "bmw_genzq"
  )
) |>
  purrr::map(
    ~ {
      .x |>
        dplyr::mutate(
          quarter = as.Date(quarter)
        )
    }
  )

usethis::use_data(bmw_qtrly_benchmarks_2025, overwrite = TRUE)
