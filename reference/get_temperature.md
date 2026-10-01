# Retrieve temperature for a boundary

Downloads 2 m air-temperature data for a spatial boundary and extracts
temperature values for the requested period.

## Usage

``` r
get_temperature(
  boundary,
  start,
  end,
  statistic = "daily_mean",
  source = "era5"
)
```

## Arguments

- boundary:

  Spatial boundary defining the area for which temperature should be
  retrieved. The boundary is processed by
  [`read_boundary()`](read_boundary.md).

- start:

  Start date of the requested period. Coercible to `Date`.

- end:

  End date of the requested period. Coercible to `Date`.

- statistic:

  Temperature statistic to retrieve. Defaults to `"daily_mean"`.

- source:

  Climate data source. Defaults to `"era5"`.

## Value

A data frame containing temperature values for the requested period. The
result includes `source`, `boundary`, and `statistic` attributes.
