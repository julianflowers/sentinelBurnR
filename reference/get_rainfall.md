# Retrieve rainfall for a boundary

Downloads climate data for a spatial boundary and extracts daily
rainfall for the requested period.

## Usage

``` r
get_rainfall(boundary, start, end, source = "era5")
```

## Arguments

- boundary:

  Spatial boundary defining the area for which rainfall should be
  retrieved. The boundary is processed by
  [`read_boundary()`](read_boundary.md).

- start:

  Start date of the requested period. Coercible to `Date`.

- end:

  End date of the requested period. Coercible to `Date`.

- source:

  Climate data source. Defaults to `"era5"`.

## Value

A data frame containing daily rainfall values for the requested period.
The result includes `source` and `boundary` attributes.
