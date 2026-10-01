# Calculate seasonal baseline

Combines annual seasonal index rasters into a historical baseline.

## Usage

``` r
seasonal_baseline(x, method = c("median", "mean"), min_years = 5)

seasonal_baseline(x, method = c("median", "mean"), min_years = 5)
```

## Arguments

- x:

  Named or unnamed list of single-layer `SpatRaster` objects, normally
  one raster per year.

- method:

  Summary statistic used across years: `"median"` or `"mean"`.

- min_years:

  Minimum number of valid years required for a pixel.

## Value

raster,

A list containing `baseline`, `n_years`, `method`, and `min_years`.
