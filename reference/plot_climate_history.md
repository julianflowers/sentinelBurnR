# Plot rainfall history

Compare rainfall in the analysis year with equivalent periods during a
historical baseline.

## Usage

``` r
plot_climate_history(
  rainfall,
  date,
  baseline_years = 1991:2020,
  statistic = c("cumulative", "rolling"),
  window_days = 30,
  period_days = 90,
  show_years = TRUE
)
```

## Arguments

- rainfall:

  Daily rainfall data containing `date` and `precipitation_mm`.

- date:

  Analysis date.

- baseline_years:

  Integer vector defining the historical baseline years.

- statistic:

  Rainfall statistic to display. One of `"cumulative"` or `"rolling"`.

- window_days:

  Rolling window length in days. Used when `statistic = "rolling"`.

- period_days:

  Number of days before `date` to display.

- show_years:

  Logical. If `TRUE`, show individual baseline years as faint lines.

## Value

A `ggplot` object.

## Details

The plot can show either cumulative rainfall over the analysis period or
rolling rainfall totals. Individual baseline years, the baseline median
and interquartile range, and the current year are displayed for
comparison.
