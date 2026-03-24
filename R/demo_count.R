#' Summarize demographic proportions for Lucid/Cint tracker data
#'
#' Computes survey proportions and totals by demographic group, with optional
#' additional grouping by a date-based break variable. The function returns
#' results for both the unweighted survey data and the campaign-level
#' control/exposed data.
#'
#'
#' @param data A list containing survey datasets. Expected to include at least:
#' \describe{
#'   \item{`unweighted`}{A survey object or data frame compatible with
#'   `srvyr` verbs.}
#'   \item{`campaign`}{A survey object or data frame compatible with `srvyr`
#'   verbs and containing `matched_control_xmedia`.}
#' }
#' @param demo A character string giving the name of the demographic variable
#'   to group by.
#' @param date_break An optional character string giving the name of an
#'   additional grouping variable, typically a date-derived period such as
#'   `"quarter"`, `"month"`. Defaults to `NULL`.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{`unweighted`}{A tibble of survey proportions and totals by the
#'   requested grouping variables for `data$unweighted`.}
#'   \item{`control_exposed`}{A tibble of survey proportions and totals by
#'   `matched_control_xmedia` and the requested grouping variables for
#'   `data$campaign`, excluding rows where `matched_control_xmedia` is
#'   `"unclassified"`.}
#' }
#'
#' Each returned tibble includes:
#' \describe{
#'   \item{grouping columns}{The variables supplied in `demo` and optionally
#'   `date_break`.}
#'   \item{`proportion`}{Estimated survey proportion from
#'   `srvyr::survey_mean()`.}
#'   \item{`n`}{Estimated survey total from `srvyr::survey_total()`.}
#' }
#'
#' Because grouping uses `dplyr::across(dplyr::all_of(...))`, `demo` and
#' `date_break` should be supplied as character strings naming columns present
#' in the input data.
#'
#' @examples
#' \dontrun{
#' demo_count(data = survey_data, demo = "demo_gender")
#'
#' demo_count(
#'   data = survey_data,
#'   demo = "demo_race_ethnicity",
#'   date_break = "quarter"
#' )
#' }
#'
#' @export
demo_count <- function(data, demo, date_break = NULL) {
  group_vars <- c(date_break, demo)
  a <- data$unweighted |>
    dplyr::mutate(quarter = lubridate::floor_date(date, "quarter")) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    srvyr::summarise(
      proportion = srvyr::survey_mean(),
      n = srvyr::survey_total()
    ) |>
    dplyr::select(-c(.data$proportion_se, .data$n_se))

  b <- data$campaign |>
    dplyr::mutate(quarter = lubridate::floor_date(date, "quarter")) |>
    dplyr::group_by(
      .data$matched_control_xmedia,
      dplyr::across(dplyr::all_of(group_vars))
    ) |>
    srvyr::summarise(
      proportion = srvyr::survey_mean(),
      n = srvyr::survey_total()
    ) |>
    dplyr::select(-c(.data$proportion_se, .data$n_se)) |>
    dplyr::filter(.data$matched_control_xmedia != "unclassified")

  list(unweighted = a, control_exposed = b)
}
