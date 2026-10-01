# Analyse climate conditions

Summarises recent rainfall and, optionally, temperature conditions
relative to a historical baseline.

## Usage

``` r
analyse_climate(
  rainfall,
  temperature = NULL,
  humidity = NULL,
  date,
  baseline_years,
  windows = c(30, 60, 90),
  dry_spell_window = 90,
  dry_threshold_mm = 1,
  hot_threshold = 25,
  very_hot_threshold = 30
)
```

## Arguments

- rainfall:

  Rainfall data used to calculate recent rainfall, rainfall anomalies,
  and dry-spell statistics.

- temperature:

  Optional temperature data used to calculate temperature summaries. If
  `NULL`, temperature analysis is omitted.

- humidity:

  Humidity data use to calculate humidity summaries. If `NULL`, humidity
  analysis is omitted.

- date:

  Date for which climate conditions are assessed.

- baseline_years:

  Years used to define the historical climate baseline.

- windows:

  Numeric vector giving the time windows, in days, over which recent
  climate conditions are summarised.

- dry_spell_window:

  Number of days preceding `date` used to assess dry-spell conditions.

- dry_threshold_mm:

  Daily rainfall threshold, in millimetres, below which a day is
  considered dry.

- hot_threshold:

  Temperature threshold, in degrees Celsius, used to identify hot days.

- very_hot_threshold:

  Temperature threshold, in degrees Celsius, used to identify very hot
  days.

## Value

An object containing climate summaries for the requested date and
historical baseline.
