# Filter raster patches by minimum area

Filter raster patches by minimum area

## Usage

``` r
filter_patches_by_area(patches, min_area_ha = 0)
```

## Arguments

- patches:

  A single-layer terra SpatRaster containing patch IDs.

- min_area_ha:

  Minimum patch area in hectares. Defaults to zero.

## Value

A SpatRaster with undersized patches set to NA. Original patch IDs are
preserved.
