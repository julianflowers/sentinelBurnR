# Summarise antecedent vegetation conditions

Summarises vegetation condition and moisture during a period preceding a
specified assessment date.

## Usage

``` r
antecedent_conditions(ndvi, ndmi, date, window = 45)
```

## Arguments

- ndvi:

  An NDVI time series returned by
  [`sentinel_timeseries()`](sentinel_timeseries.md) or
  `index_timeseries()`.

- ndmi:

  An NDMI time series returned by
  [`sentinel_timeseries()`](sentinel_timeseries.md) or
  `index_timeseries()`.

- date:

  Assessment date.

- window:

  Number of days preceding `date` to include.

## Value

A data frame containing antecedent vegetation-condition metrics.

## Details

For NDVI and NDMI the function reports the current value, maximum value
within the look-back window, change from that maximum, and the linear
trend through observations in the window.
