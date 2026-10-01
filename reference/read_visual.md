# Read Sentinel-2 true-colour imagery

Reads the Sentinel-2 `visual` asset for a single acquisition, crops the
imagery to the collection AOI, and mosaics multiple Sentinel-2 tiles
where necessary.

## Usage

``` r
read_visual(collection, aoi, date = NULL)
```

## Arguments

- collection:

  An `sbr_collection` containing the `visual` asset.

- aoi:

  Area of interest

- date:

  Date of the acquisition to read. If the collection contains only one
  acquisition, this may be omitted.

## Value

A three-layer
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
containing red, green and blue true-colour imagery.
