# Calculate an index anomaly from a historical baseline

Calculates the difference between a current index raster and a
historical seasonal baseline.

## Usage

``` r
index_anomaly(current, baseline)
```

## Arguments

- current:

  Single-layer `SpatRaster` containing the current index.

- baseline:

  Either a single-layer `SpatRaster` or the object returned by
  [`seasonal_baseline()`](seasonal_baseline.md).

## Value

A single-layer `SpatRaster`. Negative values indicate values below the
historical baseline.
