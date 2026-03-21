#' Get creative benchmark data
#'
#' Returns the packaged creative benchmark dataset.
#'
#' @return A data frame of creative benchmark values.
#' @export
get_creative_benchmarks <- function() {
  utils::data(
    "creative_benchmarks_2025",
    package = "GSPtracker",
    envir = environment()
  )

  get("creative_benchmarks_2025", envir = environment(), inherits = FALSE)
}
