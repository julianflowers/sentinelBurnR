# Detect burned area from dNBR

Creates a burned / not-burned raster from a dNBR raster.

## Usage

``` r
burn_area(x, unit = "ha")
```

## Arguments

- x:

  A single-layer dNBR SpatRaster.

- unit:

  Area units to return (e.g. "ha", "m2", "km2").

## Value

A single-layer SpatRaster named `burned`.
