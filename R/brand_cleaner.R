#' Clean and standardize brand-level survey outputs
#'
#' `brand_cleaner()` filters and reshapes the output of [brand_summary()]
#' for a specific brand, standardizing brand names, relabeling key responses,
#' and optionally returning a wide format for easier side-by-side comparison
#' across brands.
#'
#' The function maps common user inputs (e.g., "bmw", "mercedes", "mb")
#' to canonical brand names, filters the relevant survey responses, and
#' prepares the data for downstream analysis or visualization.
#'
#' @param tbl A data frame returned from [brand_summary()], containing at minimum
#'   `svy_q`, `proportion`, `n`, `total`, `question`, and `var`.
#' @param brand A character string specifying the brand of interest. Input is
#'   case-insensitive and supports common aliases (e.g., "bmw", "mercedes",
#'   "mb").
#' @param wide Logical. If `TRUE`, returns a wide-format data frame with
#'   brand-prefixed column names (e.g., `BMW_proportion`). If `FALSE` (default),
#'   returns a tidy data frame with a `brand` column.
#'
#' @return
#' A tibble containing cleaned brand-level results:
#' \itemize{
#'   \item In tidy format (`wide = FALSE`): includes a `brand` column and
#'   standard metric columns (`proportion`, `n`, `total`).
#'   \item In wide format (`wide = TRUE`): metric columns are prefixed with the
#'   brand name (e.g., `BMW_proportion`, `BMW_n`, `BMW_total`).
#' }
#'
#' @details
#' The function:
#' \itemize{
#'   \item Normalizes user-supplied brand names using a lookup table.
#'   \item Filters to the selected brand and relevant derived categories
#'   (e.g., "On its way up - Top 2 Box").
#'   \item Replaces the brand label in `svy_q` with the corresponding question
#'   text for readability.
#'   \item Optionally reshapes output for easier comparison across brands.
#' }
#'
#' @examples
#' \dontrun{
#' # Tidy output
#' brand_cleaner(tbl, "bmw")
#'
#' # Wide output for joining multiple brands
#' dplyr::left_join(
#'   brand_cleaner(tbl, "bmw", wide = TRUE),
#'   brand_cleaner(tbl, "audi", wide = TRUE),
#'   by = c("month", "matched_control_xmedia", "svy_q")
#' )
#' }
#'
#' @export
brand_cleaner <- function(tbl, brand, wide = FALSE) {
  brand_lookup <- c(
    "bmw" = "BMW",
    "mercedes" = "Mercedes Benz",
    "mercedes benz" = "Mercedes Benz",
    "mb" = "Mercedes Benz",
    "lexus" = "Lexus",
    "audi" = "Audi",
    "tesla" = "Tesla"
  )

  brand_std <- brand_lookup[[stringr::str_to_lower(brand)]]

  if (is.null(brand_std)) {
    rlang::abort(paste0("Unknown brand: '", brand, "'"))
  }

  out <- tbl |>
    dplyr::filter(
      .data$svy_q == brand_std | .data$svy_q == "On its way up - Top 2 Box"
    ) |>
    dplyr::mutate(
      svy_q = dplyr::case_when(
        .data$svy_q == brand_std ~ .data$question,
        TRUE ~ .data$svy_q
      ),
      brand = brand_std
    ) |>
    dplyr::select(-c(.data$question, .data$var))

  if (wide) {
    out <- out |>
      dplyr::rename_with(
        ~ paste0(brand_std, "_", .x),
        .cols = c("proportion", "n", "total")
      ) |>
      dplyr::select(-.data$brand)
  }

  out
}
