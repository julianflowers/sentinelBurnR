# Plot a Sentinel-2 spectral index time series

Plots the median spectral index through time together with the
interquartile range.

## Usage

``` r
plot_timeseries(x, index = "NBR")
```

## Arguments

- x:

  A data frame returned by
  [`sentinel_timeseries()`](sentinel_timeseries.md) or
  `index_timeseries()`.

- index:

  Name of the spectral index, used for the y-axis label.

## Value

A `ggplot` object.
