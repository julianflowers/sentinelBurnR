
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sentinelBurnR

<!-- badges: start -->

[![R-CMD-check](https://github.com/julianflowers/sentinelBurnR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/julianflowers/sentinelBurnR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# sentinelBurnR

An R package for ecological monitoring using Sentinel-2 imagery.

## Installation

``` r
remotes::install_github("julianflowers/sentinelBurnR")
```

## Example

``` r
library(sentinelBurnR)

aoi <- read_aoi(
    system.file(
        "extdata",
        "sizewell_aoi.gpkg",
        package = "sentinelBurnR"
    )
)
```
