# Count valid Sentinel-2 observations

Counts acquisition dates with at least one SCL-valid observation.
Overlapping tiles from the same date count only once.

## Usage

``` r
valid_observation_count(collection, keep = c(4L, 5L, 6L), boundary = NULL)
```

## Arguments

- collection:

  An sbr_collection containing SCL files.

- keep:

  SCL classes treated as valid.

- boundary:

  Optional sf or SpatVector boundary.

## Value

A SpatRaster named valid_count.
