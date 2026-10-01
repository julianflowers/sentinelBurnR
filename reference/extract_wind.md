# Extract wind conditions

Extracts ERA5 10 m wind components over a boundary and calculates wind
speed and direction.

## Usage

``` r
extract_wind(u_wind, v_wind, boundary, statistic = "daily_mean")
```

## Arguments

- u_wind:

  SpatRaster containing the 10 m u wind component.

- v_wind:

  SpatRaster containing the 10 m v wind component.

- boundary:

  Analysis boundary.

- statistic:

  ERA5 daily statistic used for the components.

## Value

An `sbr_wind` data frame.
