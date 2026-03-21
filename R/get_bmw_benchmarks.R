#' Get BMW benchmark data
#'
#' Returns BMW benchmark data, either annual or quarterly, for a given year.
#'
#' @param type Type of benchmark to return. `"overall"` for annual benchmarks,
#'   `"quarter"` for quarterly benchmarks.
#' @param year Benchmark year (character or numeric).
#'
#' @return A data frame of BMW benchmark data.
#' @export
get_bmw_benchmarks <- function(type = c("overall", "quarter"), year = "2025") {
  type <- rlang::arg_match(type)
  year <- as.character(year)

  obj_name <- switch(
    type,
    overall = paste0("bmw_benchmarks_", year),
    quarter = paste0("bmw_qtrly_benchmarks_", year)
  )

  utils::data(list = obj_name, package = "GSPtracker", envir = environment())

  get(obj_name, envir = environment(), inherits = FALSE)
}
