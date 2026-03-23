#' Read and prepare Cint tracker data
#'
#' `read_cint()` imports a Cint tracker export from Excel, performs initial
#' cleaning and feature engineering, and returns both the cleaned respondent-level
#' data and a set of `srvyr` survey designs for weighted and unweighted analysis.
#'
#' The function standardizes column names, removes duplicate respondents,
#' repairs known naming issues, derives demographic and behavioral variables, and
#' constructs survey designs for any available weight columns in the file.
#'
#' @param file_loc A character string giving the path to the Cint Excel file.
#'
#' @return
#' A named list containing:
#' \itemize{
#'   \item `df`: the cleaned respondent-level data frame
#'   \item `digital`: a `srvyr` survey design weighted by `weights_digital`,
#'   if present
#'   \item `social`: a `srvyr` survey design weighted by `weights_social`,
#'   if present
#'   \item `campaign`: a `srvyr` survey design weighted by `weights_xmedia`,
#'   if present
#'   \item `tv`: a `srvyr` survey design weighted by `weights_tv`,
#'   if present
#'   \item `ooh`: a `srvyr` survey design weighted by `weights_ooh`,
#'   if present
#'   \item `you_tube`: a `srvyr` survey design weighted by
#'   `weights_you_tube`, if present
#'   \item `unweighted`: an unweighted `srvyr` survey design
#' }
#'
#' @details
#' The function performs the following preparation steps:
#' \itemize{
#'   \item reads the Excel file with [readxl::read_excel()]
#'   \item standardizes column names with [janitor::clean_names()]
#'   \item corrects known column name spelling issues
#'   \item removes duplicate rows based on `response_id`
#'   \item coerces weight columns to numeric and replaces missing weights with `0`
#'   \item derives year of birth and generation labels from `age`
#'   \item creates regional groupings from `demo_state`
#'   \item creates collapsed demographic flags such as `genz_millen` and
#'   `demo_premium`
#'   \item parses `end_date` into `date` and derives `month`
#'   \item creates EV intent and BMW model consideration flags
#'   \item builds weighted survey designs for any supported weight columns found
#'   in the data
#' }
#'
#' Supported weighted survey outputs are generated for these weight columns when
#' available:
#' \itemize{
#'   \item `weights_digital`
#'   \item `weights_social`
#'   \item `weights_xmedia`
#'   \item `weights_tv`
#'   \item `weights_ooh`
#'   \item `weights_you_tube`
#' }
#'
#' @examples
#' \dontrun{
#' cint <- read_cint("data/cint_tracker.xlsx")
#'
#' # Clean respondent-level data
#' cint$df
#'
#' # Weighted survey design
#' cint$campaign
#'
#' # Unweighted survey design
#' cint$unweighted
#' }
#'
#' @export
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
    dplyr::rename_with(
      ~ stringr::str_replace_all(.x, "brfuncitonal", "brfunctional")
    ) |>
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

  # flag unaided awareness by brands
  unaided_flags <- df |>
    dplyr::select(.data$response_id, dplyr::starts_with("awr_u_")) |>
    flag_unaided_brands()

  # join back to main data and convert NA to NULL
  df <- df |>
    dplyr::left_join(unaided_flags, by = c("response_id" = "response_id")) |>
    dplyr::mutate(dplyr::across(
      dplyr::starts_with("awr_ua_"),
      ~ tidyr::replace_na(., "NULL")
    ))

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
