#' Summarize brand survey responses
#'
#' `brand_summary()` summarizes a survey question from a `srvyr` survey object,
#' returning weighted proportions, estimated counts, and denominators by response
#' option. The function can optionally include monthly or quarterly time cuts,
#' user-supplied grouping variables, and a rolling 3-period moving average.
#'
#' For most brand questions, respondents flagged as unaware are dropped from the
#' denominator using `awr_a_1`. Brand momentum questions are also recoded into
#' Top 2 Box / Bottom 2 Box categories.
#'
#' @param data A `srvyr` survey object containing the survey data.
#' @param groups Optional character vector of grouping variables to include in
#'   the summary.
#' @param qq A character string giving the survey question variable to summarize.
#' @param include_month Logical. If `TRUE`, adds a monthly time grouping based on
#'   `date`.
#' @param include_quarter Logical. If `TRUE`, adds a quarterly time grouping
#'   based on `date`.
#' @param moving_average Logical. If `TRUE`, computes a 3-period moving average
#'   using rolling sums of `n` and `total`. Requires `include_month = TRUE` and
#'   `include_quarter = FALSE`.
#' @param drop_unaware Logical. If `TRUE` (default), respondents with
#'   `awr_a_1` missing or equal to `0` are removed before summarization for all
#'   questions except unaided awareness / `awr_a_1`.
#'
#' @return
#' A tibble with one row per response level and grouping combination. Output
#' includes:
#' \itemize{
#'   \item `svy_q`: response level for the summarized question
#'   \item `proportion`: weighted survey proportion
#'   \item `n`: weighted survey total for that response
#'   \item `total`: weighted denominator within the grouping
#'   \item `var`: the input variable name supplied to `qq`
#' }
#'
#' If `moving_average = TRUE`, the output also includes:
#' \itemize{
#'   \item `ma_n`: rolling 3-period sum of `n`
#'   \item `ma_total`: rolling 3-period sum of `total`
#'   \item `ma3`: rolling 3-period proportion, calculated as `ma_n / ma_total`
#' }
#'
#' @details
#' The function:
#' \itemize{
#'   \item derives `month` and `quarter` from `date`
#'   \item optionally removes unaware respondents using `awr_a_1`
#'   \item renames the selected survey variable to `svy_q`
#'   \item recodes brand momentum responses into Top 2 Box / Bottom 2 Box
#'   \item summarizes weighted proportions and totals using `srvyr`
#'   \item removes `"NULL"`, `0`, and `"unclassified"` categories from the final
#'   output where applicable
#' }
#'
#' @examples
#' \dontrun{
#' # Overall summary for one question
#' brand_summary(
#'   data = cint$unweighted,
#'   qq = "purchase_intent"
#' )
#'
#' # Summary by month
#' brand_summary(
#'   data = cint$unweighted,
#'   qq = "consideration",
#'   include_month = TRUE
#' )
#'
#' # Summary by custom grouping variable and month
#' brand_summary(
#'   data = cint$unweighted,
#'   groups = "demo_gender",
#'   qq = "momentum_br",
#'   include_month = TRUE
#' )
#'
#' # Monthly summary with moving average
#' brand_summary(
#'   data = cint$unweighted,
#'   qq = "purchase_intent",
#'   include_month = TRUE,
#'   moving_average = TRUE
#' )
#' }
#'
#' @export
brand_summary <- function(
  data,
  groups = NULL,
  qq,
  include_month = FALSE,
  include_quarter = FALSE,
  moving_average = FALSE,
  drop_unaware = TRUE
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
    )

  if (drop_unaware && !stringr::str_detect(qq, "awr_ua_|awr_a_1")) {
    tmp <- tmp |>
      dplyr::filter(!is.na(.data[["awr_a_1"]]), .data[["awr_a_1"]] != 0)
  }

  tmp <- tmp |>
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

  # if not unaided awareness, drop "NULL" (this is old filter for unaware of bmw)
  # if (!stringr::str_detect(qq, "unaided_awareness")) {
  #   tmp <- tmp |>
  #     dplyr::filter(.data$svy_q != "NULL")
  # }

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
