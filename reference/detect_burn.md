# Detect burned area from dNBR

Creates a burned / not-burned raster from a dNBR raster.

## Usage

``` r
detect_burn(x, threshold = 0.27)
```

## Arguments

- x:

  A single-layer dNBR SpatRaster.

- threshold:

  Numeric dNBR threshold above which cells are classified as burned.

## Value

A single-layer SpatRaster named `burned`.
