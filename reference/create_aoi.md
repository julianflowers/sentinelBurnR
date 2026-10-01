# Create an Area of Interest

Create an Area of Interest (AOI) from either a bounding box or a point
and radius.

## Usage

``` r
create_aoi(
  xmin = NULL,
  xmax = NULL,
  ymin = NULL,
  ymax = NULL,
  lon = NULL,
  lat = NULL,
  radius = NULL,
  crs = "EPSG:4326"
)
```

## Arguments

- xmin, xmax, ymin, ymax:

  Bounding box coordinates.

- lon, lat:

  Longitude and latitude of the centre point.

- radius:

  Radius (metres) around the centre point.

- crs:

  Coordinate reference system of the input coordinates.

## Value

An sbr_aoi object.

## Examples

``` r
if (FALSE) { # \dontrun{
aoi <- create_aoi(
    xmin = 1.61,
    ymin = 52.24,
    xmax = 1.64,
    ymax = 52.26
)
} # }
```
