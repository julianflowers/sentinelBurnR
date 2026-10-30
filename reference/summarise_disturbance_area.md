# Summarise disturbance classifications by patch area

Summarise disturbance classifications by patch area

## Usage

``` r
summarise_disturbance_area(disturbances, patches, by_date = FALSE)
```

## Arguments

- disturbances:

  A data frame containing `patch`, `classification`, and
  `disturbance_date`.

- patches:

  A single-layer SpatRaster containing patch IDs.

- by_date:

  Whether to group results by disturbance date.

## Value

A tibble containing patch counts, total patch area and median patch area
in hectares.
