#' Read in tracker data from Cint
#'
#' This function reads in the raw data and weights the data using the weights provided by Cint in the file.
#'
#' @param file_loc Give the location of the tracker file
#'
#' @returns Named list. Raw datafile (df), all others are in srvyr survey format
#'
#' @export
#' @examples
#' \dontrun{
#' file_loc <- "/folder/director/file.csv"
#' cint <- read_cint(file_loc)
#' }
read_cint <- function(file_loc) {
  gen_labels = c(
    "Gen Alpha",
    "Gen Z",
    "Millennials",
    "Gen X",
    "Boomers",
    "Silent",
    "Greatest"
  )
  premium_makes <- c(
    "car_make_2", # audi
    "car_make_3", # bmw
    "car_make_13", # lexus
    "car_make_14", # lucid
    "car_make_16", # mercedes
    "car_make_20", # porsche
    "car_make_21", # range rover
    "car_make_22", # rivian
    "car_make_24" # tesla
  )

  bmw_series <- paste0("cons_bmw_model_", 5:11)
  bmw_xrange <- paste0("cons_bmw_model_", c(13:15, 17:20))
  bmw_iseries <- paste0("cons_bmw_model_", c(21:23, 27))

  df <- readxl::read_excel(file_loc) |>
    janitor::clean_names() |>
    dplyr::rename_with(~ stringr::str_replace_all(.x, "brfuncitonal", ""))
  dplyr::distinct(.data$response_id, .keep_all = TRUE) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("weights_"),
        ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)
      ),
      yob = lubridate::year(Sys.Date()) - .data$age,
      generations = dplyr::case_when(
        yob < 2013 & yob > 1996 ~ "Gen Z",
        yob < 1997 & yob > 1980 ~ "Millennials",
        yob < 1981 & yob > 1964 ~ "Gen X",
        yob < 1965 & yob > 1945 ~ "Boomers",
        yob < 1946 & yob > 1927 ~ "Silent",
        yob < 1928 ~ "Greatest",
        yob > 2012 ~ "Gen Alpha"
      ),
      demo_region = dplyr::case_when(
        demo_state %in%
          c(
            "ALABAMA",
            "ARKANSAS",
            "DISTRICT OF COLUMBIA",
            "FLORIDA",
            "GEORGIA",
            "KENTUCKY",
            "LOUISIANA",
            "MISSISSIPPI",
            "NORTH CAROLINA",
            "OKLAHOMA",
            "SOUTH CAROLINA",
            "TENNESSEE",
            "TEXAS",
            "VIRGINIA",
            "WEST VIRGINIA",
            "DELAWARE"
          ) ~ "South",
        demo_state %in%
          c(
            "ILLINOIS",
            "INDIANA",
            "IOWA",
            "KANSAS",
            "MICHIGAN",
            "MINNESOTA",
            "MISSOURI",
            "NEBRASKA",
            "NORTH DAKOTA",
            "OHIO",
            "SOUTH DAKOTA",
            "WISCONSIN"
          ) ~ "Midwest",
        demo_state %in%
          c(
            "CONNECTICUT",
            "MAINE",
            "MASSACHUSETTS",
            "MARYLAND",
            "NEW HAMPSHIRE",
            "NEW JERSEY",
            "NEW YORK",
            "PENNSYLVANIA",
            "RHODE ISLAND",
            "VERMONT"
          ) ~ "Northeast",
        TRUE ~ "West"
      ),
      genz_millen = dplyr::if_else(
        stringr::str_detect(.data$generations, "Z|Mill"),
        "Gen Z/Millennial",
        "Gen X/Boomer"
      ),
      generations = factor(.data$generations, levels = gen_labels),
      demo_premium = dplyr::case_when(
        demo_income != "$100k-149k" &
          rowSums(dplyr::across(
            dplyr::any_of(premium_makes),
            ~ as.integer(. != "0")
          )) >
            0 ~ 'Premium',
        TRUE ~ "Non-Premium"
      ),
      date = as.Date(.data$end_date, format = "%m/%d/%Y"),
      month = lubridate::month(date, label = TRUE),
      ev_intender = ifelse(
        .data$vehicle_type_1 == "Fully electric",
        "EVIntender",
        "Non"
      ),
      series_consider = dplyr::case_when(
        rowSums(
          dplyr::across(
            dplyr::any_of(bmw_series),
            ~ as.integer(!is.na(.) & . != "NULL" & . != "")
          ),
          na.rm = TRUE
        ) >
          0 ~ "BMW Series",
        TRUE ~ "0"
      ),
      iseries_consider = dplyr::case_when(
        rowSums(
          dplyr::across(
            dplyr::any_of(bmw_iseries),
            ~ as.integer(!is.na(.) & . != "NULL" & . != "")
          ),
          na.rm = TRUE
        ) >
          0 ~ "BMW iSeries",
        TRUE ~ "0"
      ),
      xrange_consider = dplyr::case_when(
        rowSums(
          dplyr::across(
            dplyr::any_of(bmw_xrange),
            ~ as.integer(!is.na(.) & . != "NULL" & . != "")
          ),
          na.rm = TRUE
        ) >
          0 ~ "BMW XRange",
        TRUE ~ "0"
      ),
    )

  out <- list(df = df)

  weight_map <- c(
    digital = "weights_digital",
    social = "weights_social",
    campaign = "weights_xmedia",
    tv = "weights_tv",
    ooh = "weights_ooh",
    you_tube = "weights_you_tube"
  )

  for (nm in names(weight_map)) {
    wt_col <- weight_map[[nm]]

    if (wt_col %in% names(df)) {
      out[[nm]] <- srvyr::as_survey_design(
        df,
        ids = 1,
        weight = !!rlang::sym(wt_col)
      )
    }
  }

  out$unweighted <- srvyr::as_survey_design(df, ids = NULL)
  out
}
