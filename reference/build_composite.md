# Build a median Sentinel-2 composite

Build a median Sentinel-2 composite

## Usage

``` r
build_composite(
  collection,
  assets = s2_burn_assets,
  cache = TRUE,
  overwrite = FALSE
)
```

## Arguments

- collection:

  An sbr_collection.

- assets:

  Assets to include.

- cache:

  Logical; whether to use the persistent composite cache.

- overwrite:

  Logical; whether to overwrite an existing cached composite.

## Value

A SpatRaster.
