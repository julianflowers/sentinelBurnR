# Calculate change in a spectral index through time

Calculates the change in median spectral index between consecutive
observations in a time series.

## Usage

``` r
timeseries_change(x)
```

## Arguments

- x:

  A data frame returned by
  [`sentinel_timeseries()`](sentinel_timeseries.md) or
  `index_timeseries()`.

## Value

A data frame containing the start and end dates of each interval, the
median index values and the change between them.
