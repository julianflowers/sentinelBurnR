
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sentinelBurnR

<!-- badges: start -->

[![R-CMD-check](https://github.com/julianflowers/sentinelBurnR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/julianflowers/sentinelBurnR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# sentinelBurnR

An R package for detecting and mapping wildfire impacts from Sentinel-2
imagery.

sentinelBurnR provides an end-to-end workflow for wildfire analysis
using Sentinel-2 Level-2A imagery. It handles image discovery, download,
cloud masking, compositing, burn index calculation and visualisation,
allowing users to move from an area of interest to burn severity
products with only a few lines of code.

Features

Create areas of interest from coordinates or spatial objects

Search Sentinel-2 STAC catalogues

Parallel image downloads

Local or project-based image storage

Pixel-level cloud masking using the Sentinel-2 Scene Classification
Layer (SCL)

Multi-date median compositing

Multi-tile mosaicking

Calculate:

Normalized Burn Ratio (NBR)

Differenced Normalized Burn Ratio (dNBR)

Publication-quality plotting using ggplot2 and tidyterra

Project-based workflow for reproducible analyses \## Installation

``` r
# install.packages("remotes")
remotes::install_github("julianflowers/sentinelBurnR")
```

## Example workflow

``` r
library(sentinelBurnR)

#
# Configure package
#

sbr_options(

    project_dir = "~/sentinelBurnR",

    temp_dir = "~/Library/Caches/sentinelBurnR/tmp"

)

#
# Create a project
#

project <- create_project(
    "Brandon_2026"
)

#
# Create an AOI
#

aoi <- create_aoi(

    xmin = 1.618117,

    ymin = 52.246700,

    xmax = 1.636083,

    ymax = 52.257699

)

#
# Run the complete workflow
#

fire <- detect_burn(

    project = project,

    aoi = aoi,

    pre = date_range(
        "2026-07-01",
        "2026-07-28"
    ),

    post = date_range(
        "2026-07-29",
        "2026-08-09"
    ),

    max_cloud = 40,

    workers = 6

)
```

## Plot results

``` r
plot_nbr(
    fire$nbr_pre
)

plot_dnbr(
    fire$dnbr
)
```
