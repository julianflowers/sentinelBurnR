# Get climate data for baseline analysis

Downloads and extracts rainfall and temperature data required for
climate analysis for the current year and a set of baseline years.

## Usage

``` r
get_climate_baseline(
  boundary,
  date,
  baseline_years = 1991:2020,
  windows = c(30, 60, 90),
  dry_spell_window = 90,
  source = "era5",
  workers = 4
)
```

## Arguments

- boundary:

  Spatial boundary.

- date:

  Analysis date.

- baseline_years:

  Years used for the historical baseline.

- windows:

  Rainfall and temperature comparison windows in days.

- dry_spell_window:

  Window used for dry-spell analysis.

- source:

  Climate data source.

- workers:

  Number of parallel download workers.

## Value

A list containing `rainfall` and `temperature`.
