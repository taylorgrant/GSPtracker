#' Summarize tracker data for a specific brand
#'
#' Summarizes the control/exposed across brand metrics, brand traits, and brand attributes. If using
#' the unweighted data set, it returns total population percentages.
#'
#' @param data Tracker dataset to use
#' @param groups Group to cut by, or NULL
#' @param qq Tracker question of interest
#' @param include_month Logical - break data out by month of tracker
#' @param include_quarter Logical - roll data up to quarters
#' @param moving_average Logical - include 3 month rolling average
#'
#' @returns Dataframe
#'
#' @export
#' @examples
#' \dontrun{
#'# individual question
#' qsummary(cint$campaign, groups = NULL, qq = "unaided_awareness_client")
#' # multiple questions (using map::purrr)
#' c("unaided_awareness_client", "awr_a_1", "awr_aad_5") |>
#' purrr::map_dfr(~ qsummary(data = cint$unweighted, groups = NULL, qq = .x))
#' }
qsummary <- function(
  data,
  groups = NULL,
  qq,
  include_month = FALSE,
  include_quarter = FALSE,
  moving_average = FALSE
) {
  # check that moving average can be run (only if including months)
  if (moving_average && !include_month) {
    rlang::abort("`moving_average = TRUE` requires `include_month = TRUE`.")
  }

  if (moving_average && include_quarter) {
    rlang::abort("`moving_average = TRUE` requires `include_quarter = FALSE`.")
  }

  # proper control/exposed by ad source
  match_control <- switch(
    names(data$allprob),
    "weights_xmedia" = "matched_control_xmedia",
    "weights_digital" = "matched_control_digital",
    "weights_social" = 'matched_control_social',
    "weights_tv" = "matched_control_tv",
    "weights_you_tube" = "matched_control_you_tube",
    "weights_ooh" = "matched_control_ooh",
    "probs" = NULL
  )

  # renaming for variable as 'svy_q'
  tmp <- data |>
    dplyr::mutate(
      month = lubridate::floor_date(date, "month"),
      quarter = lubridate::floor_date(date, "quarter")
    ) |>
    dplyr::rename(svy_q = !!rlang::sym(qq))

  # if brand momentum, convert to top 2 / bottom 2 box
  if (stringr::str_detect(qq, "momentum_br")) {
    tmp <- tmp |>
      dplyr::mutate(
        svy_q = dplyr::case_when(
          .data$svy_q %in%
            c(
              "On its way up, a lot going for it",
              "On its way up, a little going for it"
            ) ~
            "On its way up - Top 2 Box",
          .data$svy_q %in%
            c(
              "On its way down, losing a little",
              "On its way down, nothing going for it"
            ) ~
            "On its way down - Bottom 2 Box",
          .data$svy_q == "It's holding its ground" ~ "It's holding its ground"
        ),
        svy_q = factor(
          .data$svy_q,
          levels = c(
            "On its way up - Top 2 Box",
            "It's holding its ground",
            "On its way down - Bottom 2 Box"
          )
        )
      )
  }

  # if not unaided awareness, drop NULL (NULL are unaware of BMW; don't want them in our denominator)
  if (!stringr::str_detect(qq, "unaided_awareness")) {
    tmp <- tmp |>
      dplyr::filter(.data$svy_q != "NULL")
  }

  # set up groupings for the data (if groups are there)
  group_vars <- groups # Start with user-supplied groups

  if (include_month) {
    group_vars <- c(group_vars, "month")
  }

  if (include_quarter) {
    group_vars <- c(group_vars, "quarter")
  }

  group_vars <- c(group_vars, match_control, "svy_q") # Always group by svy_q

  tmp <- tmp |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars)))

  # process for proportion and n count
  tmp <- tmp |>
    srvyr::summarise(
      proportion = srvyr::survey_mean(),
      n = srvyr::survey_total()
    )

  # drop unclassified from the data
  if (!is.null(match_control)) {
    tmp <- tmp |>
      dplyr::filter(!!rlang::sym(match_control) != "unclassified")
  }

  # add totals for easier significance testing later
  tmp <- tmp |>
    dplyr::mutate(
      total = sum(.data$n),
      var = qq,
      svy_q = stringr::str_remove_all(.data$svy_q, "\r|\n")
    ) |>
    dplyr::filter(.data$svy_q != "NULL") |> # dropping null from unaided awareness
    dplyr::filter(.data$svy_q != 0) |>
    dplyr::select(-c(.data$proportion_se, .data$n_se))

  if (moving_average) {
    ma_group_vars <- setdiff(group_vars, "month")

    tmp |>
      dplyr::group_by(dplyr::across(ma_group_vars)) |>
      dplyr::arrange(.data$month, .by_group = TRUE) |>
      dplyr::mutate(
        ma_n = zoo::rollapplyr(
          .data$n,
          width = 3,
          FUN = sum,
          fill = NA,
          partial = FALSE
        ),
        ma_total = zoo::rollapplyr(
          .data$total,
          width = 3,
          FUN = sum,
          fill = NA,
          partial = FALSE
        ),
        ma3 = .data$ma_n / .data$ma_total
      ) |>
      dplyr::ungroup()
  } else {
    tmp
  }
}
