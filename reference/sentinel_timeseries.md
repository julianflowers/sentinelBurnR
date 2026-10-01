# Build a Sentinel-2 spectral index time series

Builds one composite for each acquisition date in a Sentinel-2
collection and calculates a spatial summary of the requested spectral
index.

## Usage

``` r
sentinel_timeseries(
  collection,
  index = c("nbr", "ndvi", "ndmi", "msi"),
  boundary = NULL
)
```

## Arguments

- collection:

  An `sbr_collection`.

- index:

  Spectral index to calculate. One of `"nbr"`, `"ndvi"` `"msi"` or
  `"ndmi"`.

- boundary:

  Optional spatial boundary used to crop and mask each index raster.

## Value

A data frame containing one row per acquisition date, with the median,
interquartile range and number of valid pixels.
