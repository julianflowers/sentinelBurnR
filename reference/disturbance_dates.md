# Select pre- and post-disturbance dates

Identifies the strongest candidate disturbance in a spectral index time
series and returns the acquisitions immediately before and after the
detected change.

## Usage

``` r
disturbance_dates(x, threshold = -0.2)
```

## Arguments

- x:

  A data frame returned by
  [`sentinel_timeseries()`](sentinel_timeseries.md) or
  `index_timeseries()`.

- threshold:

  Minimum negative change required to identify a disturbance.

## Value

A list containing `pre`, `post` and `change`.
