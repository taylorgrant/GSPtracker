
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GSPtracker

`GSPtracker` provides helper functions for reading, cleaning, and
summarizing Lucid/Cint brand tracker data. The package supports a
typical tracker workflow: import an Excel export, build weighted survey
objects, summarize brand measures, clean brand-level outputs for
reporting, summarize creative diagnostics, and retrieve packaged
benchmark datasets for comparison.

## Installation

``` r
remotes::install_github("taylorgrant/GSPtracker")
```

## Typical Workflow

### Read in and prepare tracker data

Use `read_cint()` to import a tracker export and prepare cleaned
respondent-level data plus survey designs. The object `cint` is a named
list. The list will include a raw file `df`, an `unweighted` that’s used
for overall results, and then a series of other weighted datasets used
for control/exposed analysis.

``` r
cint <- read_cint("path/to/cint_tracker_file.xlsx")
names(cint)
```

### Get variable mappings for a brand

Use `brand_choice()` to retrieve tracker variable mappings for supported
brands (Audi, BMW, Lexus, Mercedes, Tesla).

``` r
bmw_qs <- brand_choice("bmw")
```

### Summarize brand questions

Use `brand_summary()` on a `srvyr` survey object to summarize a tracker
question.

There are a number of arguments in the summary function:

- data: e.g., cint$campaign, cint$unweighted, etc.
- include_month: TRUE/FALSE, do you want data at the monthly level
- inlcude_quarter: TRUE/FALSE, do you want data rolled up to the
  quarterly level (if `include_month` and `include_quarter` are both
  FALSE, all metrics will be aggregated to single values)
- moving_average: TRUE/FALSE, if `include_month` is also TRUE, a 3-month
  moving average will be estimated
- drop_unaware: TRUE/FALSE, drops those that aren’t aware of BMW.
  Recommend keeping this as TRUE

Assume we’re interested in overall results (not C/E).

To run over a single metric:

``` r
brand_summary(
  cint$campaign,
  groups = NULL,
  qq = bmw_qs$brand_vars[bmw_qs$brand_vars$q == "Unaided Awareness", ]$var
)
```

To run over all metrics for a brand for a specific question bank
(metrics, traits, attributes):

``` r
bmw_metrics <- bmw_qs$brand_vars |>
  purrr::pmap_dfr(function(question, var, ...) {
    brand_summary(
      data = cint$unweighted,
      groups = NULL,
      qq = var,
      include_month = FALSE,
      include_quarter = FALSE,
      moving_average = FALSE
    ) |>
      dplyr::mutate(question = question)
  })
```

To run over all metrics in the tracker for a brand:

``` r
bmw_all <- bmw_qs |>
  purrr::imap_dfr(function(q_df, grp) {
    purrr::pmap_dfr(q_df, function(question, var, ...) {
      brand_summary(
        data = cint$unweighted,
        groups = NULL,
        qq = var,
        include_month = FALSE,
        include_quarter = FALSE,
        moving_average = FALSE
      ) |>
        dplyr::mutate(
          question = question,
          question_group = grp
        )
    })
  })
```

### Clean output for selected brand

`brand_cleaner()` is a helper function. Simply include the brand that is
being asked about and it will clean the data into a tidy data frame.

``` r
bmw_clean <- bmw_all |>
  brand_cleaner("bmw")
```

If you want a wide output for joining multiple brands:

``` r
bmw_wide <- brand_cleaner(bmw_all, brand = "bmw", wide = TRUE)
head(bmw_wide)
```

### Creative summary

Creative assets are also put into testing each month in the tracker.
These are easily summarized.

``` r
creative_overall <- creative_summary(data = cint$df, group = NULL)
```

If you want to split out by a demographic group, simply include it in
the group argument:

``` r
creative_overall <- creative_summary(data = cint$df, group = demo_gender)
```

## Packaged benchmark data

The package also includes benchmark data saved as package datasets.

### Creative benchmarks

Creative benchmarks from 2025 are included for the total audience
(overall), female, AAPI, and Gen Z/Millennials. The benchmarks split out
by Social assets (Social) and other ads, normally TV and CTV (Other).

``` r
creative_bench <- get_creative_benchmarks()
overall <- creative_bench$overall
overall
```

### BMW annual benchmarks

Benchmarks for the 2025 tracker are also included. These are also broken
out by overall, female, AAPI, and Gen Z / Millennials.

For the annual average:

``` r
bmw_annual_bench <- get_bmw_benchmarks(type = "overall")
bmw_annual_bench
```

For quarterly:

``` r
bmw_quarterly_bench <- get_bmw_benchmarks(type = "quarter")
bmw_quarterly_bench
```
