# Analyse burned area from pre- and post-fire Sentinel-2 collections

Builds pre- and post-fire composites, calculates NBR and dNBR, detects
burned pixels, classifies burn severity, and estimates burned area.

## Usage

``` r
analyse_burn(
  pre,
  post,
  threshold = 0.27,
  assets = s2_burn_assets,
  boundary = NULL
)
```

## Arguments

- pre:

  Pre-fire `sbr_collection`.

- post:

  Post-fire `sbr_collection`.

- threshold:

  Numeric dNBR threshold used to identify burned pixels. Default is
  0.27.

- assets:

  Sentinel-2 assets used to build the composites.

- boundary:

  Optional analysis boundary.

## Value

An object of class `sbr_burn`.
