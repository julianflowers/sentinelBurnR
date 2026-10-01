# Read a boundary

Reads a vector boundary from disk or converts an existing spatial object
to a terra::SpatVector.

## Usage

``` r
read_boundary(x, template = NULL)
```

## Arguments

- x:

  A file path, an sf object or a terra::SpatVector.

- template:

  Optional raster used to define the output CRS.

## Value

A terra::SpatVector.
