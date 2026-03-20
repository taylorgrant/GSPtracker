#' Summarize creative diagnostic results across brand tracker questions
#'
#' Aggregates creative-level diagnostic metrics for all configured creative
#' diagnostic questions in the brand tracker. The function applies
#' question-specific scoring rules across ad opinion, ad recall, purchase
#' intent, and additional diagnostic statements, then appends human-readable
#' statement labels to the final output.
#'
#' For each creative, the function returns the number of qualifying responses,
#' the response base, and the resulting proportion. Depending on the question,
#' the qualifying response is defined as either:
#' \itemize{
#'   \item `"Yes"` for ad recall (`"ad_rec"`),
#'   \item top-2-box positive responses for ad opinion (`"ad_opn"`),
#'   \item top-2-box likely responses for purchase intent (`"ad_pi"`), or
#'   \item top-2-box agreement responses for additional diagnostic items.
#' }
#'
#' @param data A data frame or tibble containing brand tracker survey data.
#'   The input must include `creative_assignment` and the creative diagnostic
#'   variables referenced internally by the function.
#' @param group An optional character scalar giving the name of a grouping
#'   variable. If supplied, results are summarized within levels of that
#'   variable. If `NULL`, results are summarized across the full sample.
#'
#' @return A tibble containing summarized creative diagnostic results. The
#'   returned data includes:
#'   \describe{
#'     \item{creative}{Creative identifier from `creative_assignment`.}
#'     \item{n}{Number of qualifying responses.}
#'     \item{total}{Total number of non-`"NULL"` responses used as the base.}
#'     \item{frac}{Proportion of qualifying responses.}
#'     \item{qq}{Diagnostic question variable name.}
#'     \item{statement}{Human-readable diagnostic statement label.}
#'   }
#'   If `group` is used in the function implementation, the grouping variable
#'   is also included in the output.
#'
#' @details
#' The function evaluates the following creative diagnostic variables:
#' \itemize{
#'   \item `"ad_opn"`
#'   \item `"ad_rec"`
#'   \item `"ad_pi"`
#'   \item `"ad_diag2_1"` through `"ad_diag2_11"`
#'   \item `"ad_diag2_19"`
#' }
#'
#' Responses equal to `"NULL"` are excluded before the response base and
#' proportions are calculated.
#'
#' The function uses an internal helper to apply different scoring rules by
#' question type and then joins the summarized results to a lookup table of
#' statement labels.
#'
#' @importFrom rlang .env
#'
#' @examples
#' \dontrun{
#' creative_summary(data = tracker_df)
#'
#' creative_summary(data = tracker_df, group = "wave")
#'}
#' @export
creative_summary <- function(data, group = NULL) {
  # helper function
  creative_check <- function(data, qq, group = NULL) {
    if (is.null(group)) {
      if (any(qq == "ad_rec")) {
        data |>
          dplyr::group_by(.data$creative_assignment, !!rlang::sym(qq)) |>
          dplyr::summarise(n = dplyr::n()) |>
          dplyr::filter(!!rlang::sym(qq) != "NULL") |>
          dplyr::mutate(total = sum(.data$n), frac = .data$n / sum(.data$n)) |>
          dplyr::filter(!!rlang::sym(qq) == "Yes") |>
          dplyr::rename(creative = .data$creative_assignment) |>
          dplyr::mutate(qq = .env$qq) |>
          dplyr::ungroup() |>
          dplyr::select(-.data$ad_rec)
        # select(creative_assignment, !!qq := frac)
      } else if (any(qq == "ad_opn")) {
        data |>
          dplyr::group_by(.data$creative_assignment, !!rlang::sym(qq)) |>
          dplyr::summarise(n = dplyr::n()) |>
          dplyr::filter(!!rlang::sym(qq) != "NULL") |>
          dplyr::mutate(
            total = sum(.data$n),
            box = dplyr::case_when(
              stringr::str_detect(!!sym(qq), "positive") ~ "T2B",
              TRUE ~ "not"
            )
          ) |>
          dplyr::group_by(.data$creative_assignment, .data$box, .data$total) |>
          dplyr::summarise(n = sum(.data$n)) |>
          dplyr::mutate(frac = .data$n / .data$total) |>
          dplyr::filter(.data$box == "T2B") |>
          dplyr::rename(creative = .data$creative_assignment) |>
          dplyr::mutate(qq = "ad_opn") |>
          dplyr::ungroup() |>
          dplyr::select(-.data$box)
      } else if (any(qq == "ad_pi")) {
        data |>
          dplyr::group_by(.data$creative_assignment, !!rlang::sym(qq)) |>
          dplyr::summarise(n = dplyr::n()) |>
          dplyr::filter(!!rlang::sym(qq) != "NULL") |>
          dplyr::mutate(
            total = sum(.data$n),
            box = dplyr::case_when(
              !!rlang::sym(qq) %in%
                c("Somewhat likely", "Very likely") ~ "T2B",
              TRUE ~ 'not'
            )
          ) |>
          dplyr::group_by(.data$creative_assignment, .data$box, .data$total) |>
          dplyr::summarise(n = sum(.data$n)) |>
          dplyr::mutate(frac = .data$n / .data$total) |>
          dplyr::filter(.data$box == "T2B") |>
          dplyr::rename(creative = .data$creative_assignment) |>
          dplyr::mutate(qq = "ad_pi") |>
          dplyr::ungroup() |>
          dplyr::select(-.data$box)
      } else {
        data |>
          dplyr::group_by(.data$creative_assignment, !!rlang::sym(qq)) |>
          dplyr::summarise(n = dplyr::n()) |>
          dplyr::filter(!!rlang::sym(qq) != "NULL") |>
          dplyr::mutate(
            total = sum(.data$n),
            box = dplyr::case_when(
              !!sym(qq) %in%
                c("Somewhat Agree", "Strongly Agree") ~ "T2B",
              TRUE ~ 'not'
            )
          ) |>
          dplyr::group_by(.data$creative_assignment, .data$box, .data$total) |>
          dplyr::summarise(n = sum(.data$n)) |>
          dplyr::mutate(frac = .data$n / .data$total) |>
          dplyr::filter(.data$box == "T2B") |>
          dplyr::rename(creative = .data$creative_assignment) |>
          dplyr::mutate(qq = .env$qq) |>
          dplyr::ungroup() |>
          dplyr::select(-.data$box)
      }
    } else {
      if (any(qq == "ad_rec")) {
        data |>
          dplyr::group_by(
            .data$creative_assignment,
            !!rlang::sym(group),
            !!rlang::sym(qq)
          ) |>
          dplyr::summarise(n = dplyr::n()) |>
          dplyr::filter(!!rlang::sym(qq) != "NULL") |>
          dplyr::mutate(total = sum(.data$n), frac = .data$n / sum(.data$n)) |>
          dplyr::filter(!!rlang::sym(qq) == "Yes") |>
          dplyr::rename(creative = .data$creative_assignment) |>
          dplyr::mutate(qq = .env$qq) |>
          dplyr::ungroup() |>
          dplyr::select(-.data$ad_rec)
        # select(creative_assignment, !!qq := frac)
      } else if (any(qq == "ad_opn")) {
        data |>
          dplyr::group_by(
            .data$creative_assignment,
            !!rlang::sym(group),
            !!rlang::sym(qq)
          ) |>
          dplyr::summarise(n = dplyr::n()) |>
          dplyr::filter(!!rlang::sym(qq) != "NULL") |>
          dplyr::mutate(
            total = sum(.data$n),
            box = dplyr::case_when(
              stringr::str_detect(!!sym(qq), "positive") ~ "T2B",
              TRUE ~ "not"
            )
          ) |>
          dplyr::group_by(
            .data$creative_assignment,
            !!rlang::sym(group),
            .data$box,
            .data$total
          ) |>
          dplyr::summarise(n = sum(.data$n)) |>
          dplyr::mutate(frac = .data$n / .data$total) |>
          dplyr::filter(.data$box == "T2B") |>
          dplyr::rename(creative = .data$creative_assignment) |>
          dplyr::mutate(qq = "ad_opn") |>
          dplyr::ungroup() |>
          dplyr::select(-.data$box)
      } else if (any(qq == "ad_pi")) {
        data |>
          dplyr::group_by(
            .data$creative_assignment,
            !!rlang::sym(group),
            !!rlang::sym(qq)
          ) |>
          dplyr::summarise(n = dplyr::n()) |>
          dplyr::filter(!!rlang::sym(qq) != "NULL") |>
          dplyr::mutate(
            total = sum(.data$n),
            box = dplyr::case_when(
              !!rlang::sym(qq) %in%
                c("Somewhat likely", "Very likely") ~ "T2B",
              TRUE ~ 'not'
            )
          ) |>
          dplyr::group_by(
            .data$creative_assignment,
            !!rlang::sym(group),
            .data$box,
            .data$total
          ) |>
          dplyr::summarise(n = sum(.data$n)) |>
          dplyr::mutate(frac = .data$n / .data$total) |>
          dplyr::filter(.data$box == "T2B") |>
          dplyr::rename(creative = .data$creative_assignment) |>
          dplyr::mutate(qq = "ad_pi") |>
          dplyr::ungroup() |>
          dplyr::select(-.data$box)
      } else {
        data |>
          dplyr::group_by(
            .data$creative_assignment,
            !!rlang::sym(group),
            !!rlang::sym(qq)
          ) |>
          dplyr::summarise(n = dplyr::n()) |>
          dplyr::filter(!!rlang::sym(qq) != "NULL") |>
          dplyr::mutate(
            total = sum(.data$n),
            box = dplyr::case_when(
              !!rlang::sym(qq) %in%
                c("Somewhat Agree", "Strongly Agree") ~ "T2B",
              TRUE ~ 'not'
            )
          ) |>
          dplyr::group_by(
            .data$creative_assignment,
            !!rlang::sym(group),
            .data$box,
            .data$total
          ) |>
          dplyr::summarise(n = sum(.data$n)) |>
          dplyr::mutate(frac = .data$n / .data$total) |>
          dplyr::filter(.data$box == "T2B") |>
          dplyr::rename(creative = .data$creative_assignment) |>
          dplyr::mutate(qq = .env$qq) |>
          dplyr::ungroup() |>
          dplyr::select(-.data$box)
      }
    }
  }

  diagnostics <- tibble::tibble(
    var = c("ad_opn", "ad_rec", "ad_pi", paste0("ad_diag2_", c(1:11, 19))),
    statement = c(
      "Ad change opinion of BMW (T2B Better)",
      "Seen ad past 2 weeks",
      "Ad change purchase consideration (T2B More Likely)",
      "Is unique and different",
      "Is believable",
      "Is enjoyable",
      "Is relevant to me",
      "Makes me think this is a brand I identify with",
      "Fits the way I feel about the brand",
      "Makes me think about the brand in a new way",
      "I was emotionally moved by the ad",
      "Makes me want to learn more about the product/brand",
      "Is confusing",
      "Is irritating",
      "Makes me think this is a brand for me"
    )
  )
  # comparing results to benchmarks (rather than stat sig)
  tmp <- purrr::map_dfr(
    diagnostics$var,
    ~ creative_check(data, qq = .x, group = NULL)
  ) |>
    dplyr::left_join(diagnostics, by = c("qq" = "var"))
  return(tmp)
}
