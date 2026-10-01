# Analyse vegetation condition

Build a Sentinel-2 composite and calculate vegetation condition indices.

## Usage

``` r
analyse_vegetation(
  collection,
  assets = s2_vegetation_assets,
  cache = TRUE,
  overwrite = FALSE
)
```

## Arguments

- collection:

  An `sbr_collection`.

- assets:

  Sentinel-2 assets used to build the composite.

- cache:

  Cache

- overwrite:

  overwrite cache

## Value

An object of class `sbr_vegetation`.
