# Read an area of interest

Reads an area of interest from a vector file, an `sf` object, an `sfc`
object, or a
[`terra::SpatVector`](https://rspatial.github.io/terra/reference/SpatVector-class.html).

## Usage

``` r
read_aoi(
  aoi,
  layer = NULL,
  dissolve = TRUE,
  make_valid = TRUE,
  target_crs = NULL
)
```

## Arguments

- aoi:

  A filename, `sf`, `sfc`, or
  [`terra::SpatVector`](https://rspatial.github.io/terra/reference/SpatVector-class.html)
  object.

- layer:

  Optional layer name when reading a multi-layer file such as a
  GeoPackage.

- dissolve:

  Logical. If `TRUE`, combine all features into one AOI.

- make_valid:

  Logical. If `TRUE`, attempt to repair invalid geometry.

- target_crs:

  Optional output coordinate reference system accepted by
  [`terra::project()`](https://rspatial.github.io/terra/reference/project.html),
  for example `"EPSG:27700"`.

## Value

A
[`terra::SpatVector`](https://rspatial.github.io/terra/reference/SpatVector-class.html).

## Details

Multiple features are optionally combined into a single geometry, and
invalid geometries are repaired where possible.

## Examples

``` r
if (FALSE) { # \dontrun{
aoi <- read_aoi("study_area.gpkg")

aoi_bng <- read_aoi(
  "study_area.gpkg",
  target_crs = "EPSG:27700"
)
} # }
```
