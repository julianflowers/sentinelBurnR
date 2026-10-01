# Plot climate conditions

Plot rainfall or temperature conditions from an `sbr_climate` analysis.

## Usage

``` r
plot_climate(x, metric = c("rainfall", "temperature"))
```

## Arguments

- x:

  An `sbr_climate` object returned by
  [`analyse_climate()`](analyse_climate.md).

- metric:

  Climate metric to plot. One of `"rainfall"` or `"temperature"`.

## Value

A `ggplot` object.
