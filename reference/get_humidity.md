# Retrieve humidity and vapour pressure deficit for a boundary

Downloads daily-mean 2 m air temperature and dewpoint temperature for a
spatial boundary and derives relative humidity and vapour pressure
deficit for the requested period.

## Usage

``` r
get_humidity(boundary, start, end, source = "era5")
```

## Arguments

- boundary:

  Spatial boundary defining the area for which humidity should be
  calculated. The boundary is processed by
  [`read_boundary()`](read_boundary.md).

- start:

  Start date of the requested period. Coercible to `Date`.

- end:

  End date of the requested period. Coercible to `Date`.

- source:

  Climate data source. Defaults to `"era5"`.

## Value

An `sbr_humidity` data frame containing `date`, `temperature_c`,
`dewpoint_c`, `relative_humidity`, and `vpd_kpa`. The result also
contains `source` and `humidity_method` attributes.
