#' Return tracker variable mappings for a selected brand
#'
#' `brand_choice()` standardizes a user-supplied brand name and returns the
#' tracker variable names associated with that brand. The output is a named list
#' of tibbles covering core brand funnel variables, brand personality traits,
#' and brand functional attributes.
#'
#' Supported brands are BMW, Audi, Lexus, Mercedes Benz, Tesla, and
#' "None of the Above".
#'
#' @param brand A character string giving the brand name. Input is
#'   case-insensitive and may include partial matches such as `"merc"` for
#'   Mercedes Benz or `"none"` for None of the Above.
#'
#' @return
#' A named list with three tibbles:
#' \itemize{
#'   \item `brand_vars`: core brand funnel and momentum variables
#'   \item `brand_traits`: brand personality trait variables
#'   \item `brand_attrs`: brand functional attribute variables
#' }
#'
#' Each tibble contains:
#' \itemize{
#'   \item `var`: the tracker variable name
#'   \item `q`: a human-readable question or attribute label
#' }
#'
#' @details
#' The function:
#' \itemize{
#'   \item normalizes the brand input to lower case
#'   \item validates that the selected brand is included in the tracker
#'   \item maps common aliases such as `"merc"` and `"none"` to their canonical
#'   labels
#'   \item constructs brand-specific tracker variable names by combining
#'   variable prefixes with brand-specific numeric or text suffixes
#' }
#'
#' @examples
#' \dontrun{
#' # BMW variables
#' brand_choice("bmw")
#'
#' # Mercedes Benz using a partial input
#' brand_choice("merc")
#'
#' # None of the Above
#' brand_choice("none")
#'}
#' @export
brand_choice <- function(brand) {
  brand_clean <- tolower(brand)

  if (!stringr::str_detect(brand_clean, "mercedes|tesla|bmw|audi|lexus|none")) {
    stop("Select a make included in the tracker")
  }

  if (stringr::str_detect(brand_clean, "merc")) {
    brand <- "Mercedes Benz"
  } else if (stringr::str_detect(brand_clean, "none")) {
    brand <- "None of the Above"
  }

  brand_clean <- tolower(brand)

  bv_n <- switch(
    brand_clean,
    "none of the above" = c("6", "7", "7", "", "x6"),
    "audi" = c("audi", "4", "28", "31", "", "x27"),
    "bmw" = c("client", "1", "5", "8", "", "x4"),
    "lexus" = c("lexus", "5", "29", "32", "", "x28"),
    "mercedes benz" = c("mercedes_benz", "2", "26", "29", "", "x25"),
    "tesla" = c("tesla", "3", "27", "30", "", "x26")
  )

  bpt_n <- switch(
    brand_clean,
    "none of these" = rep("7", 15),
    "audi" = rep("28", 15),
    "bmw" = rep("5", 15),
    "lexus" = rep("29", 15),
    "mercedes benz" = rep("26", 15),
    "tesla" = rep("27", 15)
  )

  fba_n <- switch(
    brand_clean,
    "none of the above" = rep("7", 15),
    "audi" = rep("28", 15),
    "bmw" = rep("5", 15),
    "lexus" = rep("29", 15),
    "mercedes benz" = rep("26", 15),
    "tesla" = rep("27", 15)
  )

  bv_vec <- c(
    "unaided_awareness_",
    "awr_a_",
    "awr_aad_",
    "con_br_",
    "pi_br",
    "momentum_br_"
  )
  bpt_vec <- c(
    "att_brpersonality_1_",
    "att_brpersonality_2_",
    "att_brpersonality_3_",
    "att_brpersonality_4_",
    "att_brpersonality_5_",
    "att_brpersonality_6_",
    "att_brpersonality_7_",
    "att_brpersonality_8_",
    "att_brpersonality_9_",
    "att_brpersonality_10_",
    "att_brpersonality_11_",
    "att_brpersonality_12_",
    "att_brpersonality_13_",
    "att_brpersonality_14_",
    "att_brpersonality_15_"
  )
  fba_vec <- c(
    "att_brfunctional_1_",
    "att_brfunctional_2_",
    "att_brfunctional_3_",
    "att_brfunctional_4_",
    "att_brfunctional_5_",
    "att_brfunctional_6_",
    "att_brfunctional_7_",
    "att_brfunctional_8_",
    "att_brfunctional_9_",
    "att_brfunctional_10_",
    "att_brfunctional_11_",
    "att_brfunctional_12_",
    "att_brfunctional_13_",
    "att_brfunctional_14_",
    "att_brfunctional_15_"
  )

  brand_vars <- dplyr::tibble(
    var = glue::glue("{bv_vec}{bv_n}"),
    q = c(
      "Unaided Awareness",
      "Aided Awareness",
      "Aided Ad Awareness",
      "Purchase Consideration",
      "Purchase Intent",
      "Brand Momentum"
    )
  )

  brand_traits <- dplyr::tibble(
    var = glue::glue("{bpt_vec}{bpt_n}"),
    q = c(
      "Adventurous",
      "Aggressive",
      "Arrogant",
      "Confident",
      "Distinctive",
      "Exciting",
      "Innovative",
      "Passionate",
      "Practical",
      "Responsible",
      "Trusted",
      "Leader",
      "Youthful",
      "Classy",
      "Traditional"
    )
  )

  brand_attrs <- dplyr::tibble(
    var = glue::glue("{fba_vec}{fba_n}"),
    q = c(
      "Advanced safety features",
      "Advanced tech features",
      "Attractive styling",
      "Comfortable",
      "Customer-oriented dealerships",
      "Customizable",
      "Reliable",
      "Environmentally friendly",
      "Fun to drive",
      "Lasts a long time",
      "Prestigious",
      "Quality materials, fit, and finish",
      "Responsive handling",
      "Strong brand heritage",
      "Good value for the money"
    )
  )

  list(
    brand_vars = brand_vars,
    brand_traits = brand_traits,
    brand_attrs = brand_attrs
  )
}
