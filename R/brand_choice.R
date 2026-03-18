#' Tracker questions per brand
#'
#' Based on inputted brand name, the function returns the survey variable names use to
#' measure brand health metrics, key attributes, and brand perceptions in the tracker data.
#'
#' Supported brands include: Audi, BMW, Lexus, Mercedes-Benz, Tesla. The function is
#' case insensitive and includes alias handling (e.g., "merc" = "Mercedes Benz")
#'
#' @param brand Character. Name of the brand of interest
#'
#' @returns A named list of three tibbles:
#' \describe{
#'   \item{brand_vars}{Variables and question labels for awareness and momentum.}
#'   \item{key_attrs}{Variables and question labels for key model attributes.}
#'   \item{brand_attrs}{Variables and question labels for brand perceptions.}
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' brand_choice("BMW")
#' brand_choice("mercedes")
#' }
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
    "tesla" = rep("30", 15)
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
