#' Flag unaided brand mentions from survey responses
#'
#' `flag_unaided_brands()` processes unaided awareness responses across multiple
#' columns (e.g., `awr_u_1` to `awr_u_n`), standardizes brand names, and returns
#' respondent-level indicator variables for whether each tracked brand was
#' mentioned.
#'
#' The function reshapes the data to long format, applies
#' `standardize_brand()` to handle case differences and common misspellings,
#' and then summarizes results back to one row per respondent.
#'
#' @param data A data frame containing a unique respondent identifier and one
#'   or more unaided awareness columns (e.g., `awr_u_1`, `awr_u_2`, etc.).
#' @param cols Tidy-select specification for the unaided awareness columns.
#'   Defaults to `dplyr::starts_with("awr_u_")`.
#'
#' @return
#' A tibble with one row per respondent and logical indicator columns for each
#' standardized brand (e.g., `awr_ua_BMW`, `awr_ua_Audi`). Each column is `TRUE`
#' if the respondent mentioned the brand in any unaided awareness field and
#' `FALSE` otherwise.
#'
#' @details
#' The function:
#' \itemize{
#'   \item pivots unaided awareness columns to long format
#'   \item standardizes raw responses using `standardize_brand()`
#'   \item removes non-matching or invalid responses (e.g., `-99`)
#'   \item collapses back to respondent level with one column per brand
#' }
#'
#' A unique respondent identifier (e.g., `response_id`) must be present in
#' the input data to ensure correct aggregation.
#'
#' @examples
#' \dontrun{
#' flags <- flag_unaided_brands(df)
#'
#' df_out <- df |>
#'   dplyr::left_join(flags, by = "response_id")
#' }
#'
#' @keywords internal
flag_unaided_brands <- function(data, cols = dplyr::starts_with("awr_u_")) {
  data |>
    tidyr::pivot_longer(
      cols = {{ cols }},
      names_to = "slot",
      values_to = "brand_raw"
    ) |>
    dplyr::mutate(
      brand_std = standardize_brand(.data$brand_raw)
    ) |>
    dplyr::filter(!is.na(.data$brand_std)) |>
    dplyr::distinct(.data$response_id, .data$brand_std) |>
    dplyr::mutate(named = TRUE) |>
    tidyr::pivot_wider(
      names_from = .data$brand_std,
      values_from = .data$named,
      names_prefix = "awr_ua_",
      values_fill = FALSE
    ) |>
    dplyr::mutate(
      awr_ua_tesla = dplyr::if_else(.data$awr_ua_tesla, "Tesla", "NULL"),
      awr_ua_bmw = dplyr::if_else(.data$awr_ua_bmw, "BMW", "NULL"),
      awr_ua_mercedes_benz = dplyr::if_else(
        .data$awr_ua_mercedes_benz,
        "Mercedes Benz",
        "NULL"
      ),
      awr_ua_audi = dplyr::if_else(.data$awr_ua_audi, "Audi", "NULL"),
      awr_ua_lexus = dplyr::if_else(.data$awr_ua_lexus, "Lexus", "NULL")
    )
}


#' Standardize raw brand names
#'
#' `standardize_brand()` cleans and standardizes raw text responses from
#' unaided awareness questions into a consistent set of canonical brand names.
#' It handles case normalization, whitespace, and common misspellings or
#' variants.
#'
#' @param x A character vector of raw brand responses.
#'
#' @return
#' A character vector of standardized brand names. Values that do not match a
#' tracked brand are returned as `NA`.
#'
#' @details
#' The function:
#' \itemize{
#'   \item converts text to lowercase
#'   \item removes extra whitespace and normalizes formatting
#'   \item maps known variants and misspellings to canonical brand names
#'   \item returns `NA` for invalid or unrecognized responses (e.g., `-99`)
#' }
#'
#' @examples
#' \dontrun{
#' standardize_brand(c("BMW", "bmw", "Bmw", "Testla"))
#' }
#'
#' @keywords internal
standardize_brand <- function(x) {
  x <- x |>
    stringr::str_to_lower() |>
    stringr::str_squish()

  dplyr::case_when(
    x %in% c("-99", "", "null") ~ NA_character_,
    stringr::str_detect(x, "bm") ~ "bmw",
    stringr::str_detect(x, "aud") ~ "audi",
    stringr::str_detect(x, "lex") ~ "lexus",
    stringr::str_detect(x, "tes") ~ "tesla",
    stringr::str_detect(
      x,
      "merc|^mb|benz"
    ) ~ "mercedes_benz",
    TRUE ~ NA_character_
  )
}
