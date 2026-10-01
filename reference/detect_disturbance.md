# Detect disturbances in a spectral index time series

Identifies intervals containing a large negative change in a spectral
index time series.

## Usage

``` r
detect_disturbance(x, threshold = -0.2)
```

## Arguments

- x:

  A data frame returned by
  [`sentinel_timeseries()`](sentinel_timeseries.md) or
  `index_timeseries()`.

- threshold:

  Minimum negative change required to identify a disturbance.

## Value

A data frame containing intervals identified as candidate disturbances.

## Details

This function identifies candidate disturbance events. A detected
disturbance does not necessarily represent fire and should be
interpreted using the underlying imagery and other evidence.
