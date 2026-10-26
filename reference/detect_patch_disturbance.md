# Detect persistent vegetation disturbance in patch time series

Identifies candidate disturbance events from declines in median NDVI and
evaluates whether the decline persists in subsequent observations.

## Usage

``` r
detect_patch_disturbance(
  x,
  min_decline = 0.25,
  min_confirmations = 2,
  min_persistence = 0.75,
  min_coverage = 80
)
```

## Arguments

- x:

  Data frame containing patch, date, median_ndvi and coverage_pct.

- min_decline:

  Minimum absolute NDVI decline defining a candidate.

- min_confirmations:

  Minimum number of subsequent confirming observations.

- min_persistence:

  Minimum proportion of subsequent observations confirming the
  disturbance.

- min_coverage:

  Minimum percentage of valid pixels per observation.

## Value

A tibble containing one selected event per patch.
