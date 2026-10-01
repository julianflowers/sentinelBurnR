# Calculate wind speed and direction

Calculates wind speed and meteorological wind direction from eastward
(u) and northward (v) wind components.

## Usage

``` r
calc_wind(u_ms, v_ms)
```

## Arguments

- u_ms:

  Numeric vector of eastward wind components in m/s.

- v_ms:

  Numeric vector of northward wind components in m/s.

## Value

A data frame containing:

- wind_speed_ms:

  Wind speed in m/s.

- wind_direction_deg:

  Meteorological direction from which the wind is blowing, in degrees
  clockwise from north.
