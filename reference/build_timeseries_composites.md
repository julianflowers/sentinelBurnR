# Build dated Sentinel-2 composites

Builds one Sentinel-2 composite for each acquisition date in a
downloaded collection.

## Usage

``` r
build_timeseries_composites(
  collection,
  assets = s2_burn_assets,
  cache = TRUE,
  overwrite = FALSE
)
```

## Arguments

- collection:

  An `sbr_collection`.

- assets:

  Character vector of Sentinel-2 assets to include.

- cache:

  Logical; whether to use the persistent composite cache.

- overwrite:

  Logical; whether to overwrite an existing cached composite.

## Value

A named list of `SpatRaster` composites, with names corresponding to
acquisition dates.

## Details

Each date is processed independently using the standard
[`build_composite()`](build_composite.md) pipeline, including SCL
masking, tile mosaicking, band alignment and AOI masking.
