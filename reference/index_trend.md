# Calculate spatial trends in a spectral index

Calculates the linear trend in a spectral index through time for every
raster cell in a sequence of dated Sentinel-2 composites.

## Usage

``` r
index_trend(
  composites,
  index = c("nbr", "ndvi", "ndmi", "msi"),
  start = NULL,
  end = NULL,
  min_obs = 3
)
```

## Arguments

- composites:

  A named list of `SpatRaster` composites. Names must be valid dates.

- index:

  Spectral index to calculate. One of `"nbr"`, `"ndvi"`, `"ndmi"` or
  `"msi"`.

- start:

  Optional start date.

- end:

  Optional end date.

- min_obs:

  Minimum number of valid observations required to calculate a trend.

## Value

A `SpatRaster` containing the per-cell linear trend in index units per
day.

## Details

The returned raster contains the slope of the spectral index against
time, expressed as index units per day. Negative values indicate a
declining index and positive values indicate an increasing index.
