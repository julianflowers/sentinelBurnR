# Analyse landscape structure

Summarises categorical land cover and spatial relationships between
supplied landscape features, including interface proximity, target area
within distance bands, and the occurrence of mapped transport features
along interfaces.

## Usage

``` r
analyse_landscape(
  landcover,
  category = "cover",
  habitat = NULL,
  transport = NULL,
  boundary = NULL,
  interfaces = NULL,
  distances = c(0, 10, 25, 50, 100)
)
```

## Arguments

- landcover:

  An `sf` object containing categorical land-cover polygons.

- category:

  Character string giving the land-cover category column.

- habitat:

  Optional `sf` object containing habitat polygons.

- transport:

  Optional `sf` object containing transport polygons.

- boundary:

  Optional analysis boundary.

- interfaces:

  Optional named list of interfaces. Each element must contain `source`
  and `target` `sf` objects and may contain `source_label` and
  `target_label`.

- distances:

  Numeric vector of distances in metres used to describe source-target
  proximity.

## Value

An object of class `sbr_landscape`.

## Details

`analyse_landscape()` describes landscape structure rather than
calculating a wildfire risk score. Interpretation of features such as
roads and tracks depends on their physical characteristics and
management context.
