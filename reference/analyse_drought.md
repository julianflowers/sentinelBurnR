# Analyse vegetation moisture anomaly

Compares current NDMI with a seasonally matched historical Sentinel-2
baseline. Historical observations are combined within years before the
baseline is calculated, so each year receives equal weight.

## Usage

``` r
analyse_drought(
  historical,
  current,
  current_date,
  window_days = 30,
  min_coverage = 0.9,
  min_years = 3,
  boundary = NULL,
  min_sd = 0.01
)
```

## Arguments

- historical:

  Historical `sbr_collection`.

- current:

  Current `sbr_collection`.

- current_date:

  Date to analyse.

- window_days:

  Seasonal window around `current_date`.

- min_coverage:

  Minimum proportion of valid pixels required.

- min_years:

  Minimum number of historical years required.

- boundary:

  Optional analysis boundary.

- min_sd:

  Minimum historical SD used for standardisation.

## Value

An object of class `sbr_drought`.
