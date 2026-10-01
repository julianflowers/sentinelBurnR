# Extract rainfall time series

Extract the mean daily precipitation for a boundary from an ERA5
precipitation raster.

## Usage

``` r
extract_rainfall(climate, boundary, fun = mean)
```

## Arguments

- climate:

  A SpatRaster returned by [`read_climate()`](read_climate.md).

- boundary:

  Boundary polygon.

- fun:

  Summary function used when extracting rainfall.

## Value

An object of class `sbr_rainfall`.
