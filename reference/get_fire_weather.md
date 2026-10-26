# Get hourly fire-weather conditions

Downloads ERA5 hourly temperature, dewpoint temperature, and 10 m wind
components for a specified time window and derives relative humidity,
VPD, wind speed, and wind direction.

## Usage

``` r
get_fire_weather(
  boundary,
  start,
  end,
  event_datetime = NULL,
  tz = "UTC",
  cache = cache_climate()
)
```

## Arguments

- boundary:

  Spatial boundary.

- start:

  Start of the fire-weather window.

- end:

  End of the fire-weather window.

- event_datetime:

  Optional event time, such as the reported fire time.

- tz:

  Time zone used for `start`, `end`, and `event_datetime`.

- cache:

  Directory used for cached ERA5 downloads.

## Value

An object of class `sbr_fire_weather`.
