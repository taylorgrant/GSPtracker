#' Summarize and compare all tracked brands against BMW
#'
#' `all_brand_summary()` runs [brand_summary()] across all tracked brands,
#' cleans the outputs with [brand_cleaner()], performs pairwise proportion tests
#' within each metric, and returns a wide comparison table showing each brand's
#' estimate relative to BMW along with significance indicators.
#'
#' The function compares Audi, BMW, Lexus, Mercedes Benz, and Tesla. Results are
#' tested within each grouping combination and reshaped so that non-BMW brands
#' appear in separate columns.
#'
#' @param data Dataset to be used. Likely the `unweighted` data.
#' @param groups Optional character vector of grouping variables to include in
#'   the summaries before testing.
#' @param include_month Logical. If `TRUE`, includes monthly groupings in the
#'   summaries and downstream comparisons.
#' @param include_quarter Logical. If `TRUE`, includes quarterly groupings in
#'   the summaries and downstream comparisons.
#'
#' @return
#' A tibble with one row per metric and grouping combination. Output includes:
#' \itemize{
#'   \item grouping columns carried through from the summary step, such as
#'   `question_group`, `svy_q`, and optionally `month`, `quarter`, or user-supplied
#'   grouping variables
#'   \item `bmw_est`: BMW estimate for the metric
#'   \item one estimate column per comparison brand, such as
#'   `Audi_other_est`, `Lexus_other_est`, etc.
#'   \item one significance label column per comparison brand, such as
#'   `Audi_sig_label`, `Lexus_sig_label`, etc.
#' }
#'
#' Significance labels are coded as:
#' \itemize{
#'   \item `"+"`: BMW is significantly higher than the comparison brand
#'   \item `"-"`: BMW is significantly lower than the comparison brand
#'   \item `""`: no statistically significant difference
#' }
#'
#' @details
#' The function:
#' \itemize{
#'   \item retrieves brand-specific tracker variables using [brand_choice()]
#'   \item removes Unaided Awareness from `brand_vars`
#'   \item summarizes each variable using [brand_summary()]
#'   \item standardizes outputs using [brand_cleaner()]
#'   \item performs pairwise [stats::prop.test()] comparisons across brands
#'   \item filters comparisons to those involving BMW
#'   \item reshapes results into a wide comparison table
#' }
#'
#' Pairwise tests are based on `n` and `total` returned by [brand_summary()].
#' Because these are survey-based estimates, `prop.test()` should be interpreted
#' as an approximation rather than a fully design-based significance test.
#'
#' @examples
#' \dontrun{
#' # Overall BMW vs. other-brand comparisons
#' all_brand_summary()
#'
#' # Quarterly comparisons
#' all_brand_summary(include_quarter = TRUE)
#'
#' # Monthly comparisons by gender
#' all_brand_summary(
#'   groups = "demo_gender",
#'   include_month = TRUE
#' )
#' }
#'
#' @seealso [brand_choice()], [brand_summary()], [brand_cleaner()]
#'
#' @export
all_brand_summary <- function(
  data,
  groups = NULL,
  include_month = FALSE,
  include_quarter = FALSE
) {
  if (moving_average && include_quarter) {
    rlang::abort("`moving_average = TRUE` requires `include_quarter = FALSE`.")
  }

  brands <- c("Audi", "BMW", "Lexus", "Mercedes Benz", "Tesla")

  # internal helper function
  get_all <- function(
    oem,
    groups,
    include_month,
    include_quarter
  ) {
    qs <- brand_choice(oem)

    # qs$brand_vars <- qs$brand_vars |>
    #   dplyr::filter(.data$q != "Unaided Awareness")

    qs |>
      purrr::imap_dfr(function(q_df, grp) {
        purrr::pmap_dfr(q_df, function(question, var, ...) {
          brand_summary(
            data = data,
            groups = groups,
            qq = var,
            include_month = include_month,
            include_quarter = include_quarter
          ) |>
            dplyr::mutate(
              question = question,
              question_group = grp
            )
        })
      }) |>
      brand_cleaner(brand = oem)
  }

  tmp_out <- purrr::map_dfr(
    brands,
    get_all,
    groups = groups,
    include_month = include_month,
    include_quarter = include_quarter
  )

  # set up for prop.testing
  group_vars <- setdiff(names(tmp_out), c("proportion", "n", "total", "brand"))

  grouped_test <- tmp_out |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars)))

  group_keys <- dplyr::group_keys(grouped_test)
  group_splits <- dplyr::group_split(grouped_test)

  prop_tests <- purrr::map2_dfr(
    group_splits,
    seq_len(nrow(group_keys)),
    function(d, i) {
      key_row <- group_keys[i, , drop = FALSE]

      # protect against fewer than two brands for metric
      if (dplyr::n_distinct(d$brand) < 2) {
        return(tibble::tibble())
      }

      pair_results <- utils::combn(d$brand, 2, simplify = FALSE) |>
        purrr::map_dfr(function(pair) {
          d_sub <- d |>
            dplyr::filter(.data$brand %in% pair)

          test_out <- stats::prop.test(
            x = d_sub$n,
            n = d_sub$total
          )

          tibble::tibble(
            brand_1 = pair[1],
            brand_2 = pair[2],
            p_value = test_out$p.value,
            estimate_1 = d_sub$proportion[1],
            estimate_2 = d_sub$proportion[2]
          )
        })

      dplyr::bind_cols(
        key_row[rep(1, nrow(pair_results)), , drop = FALSE],
        pair_results
      )
    }
  )

  # set so BMW is anchor brand
  prop_bmw <- prop_tests |>
    dplyr::filter(.data$brand_1 == "BMW" | .data$brand_2 == "BMW") |>
    dplyr::mutate(
      other_brand = dplyr::if_else(
        .data$brand_1 == "BMW",
        .data$brand_2,
        .data$brand_1
      ),
      bmw_est = dplyr::if_else(
        .data$brand_1 == "BMW",
        .data$estimate_1,
        .data$estimate_2
      ),
      other_est = dplyr::if_else(
        .data$brand_1 == "BMW",
        .data$estimate_2,
        .data$estimate_1
      ),
      sig = .data$p_value < 0.05,
      sig_label = dplyr::case_when(
        !.data$sig ~ "",
        .data$bmw_est > .data$other_est ~ "+",
        TRUE ~ "-"
      )
    )

  # widen data
  prop_wide <- prop_bmw |>
    dplyr::select(
      dplyr::all_of(group_vars),
      .data$other_brand,
      .data$bmw_est,
      .data$other_est,
      .data$sig_label
    ) |>
    tidyr::pivot_wider(
      names_from = .data$other_brand,
      values_from = c(.data$other_est, .data$sig_label),
      names_glue = "{other_brand}_{.value}"
    )
}
