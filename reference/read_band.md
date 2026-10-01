# Read one asset from a downloaded Sentinel-2 collection

Returns one SpatRaster stack per MGRS tile.

## Usage

``` r
read_band(collection, asset)
```

## Arguments

- collection:

  An sbr_collection.

- asset:

  Sentinel-2 asset name.

## Value

A named list of SpatRaster objects, one per tile.
