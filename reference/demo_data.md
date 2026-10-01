# Load sentinelBurnR demo data

Loads the small Dunwich demonstration datasets supplied with
sentinelBurnR. Sentinel-2 collections contain cropped imagery and can be
passed directly to the package analysis functions.

## Usage

``` r
demo_data(
  name = c("aoi", "pre", "post", "visual_pre", "visual_post", "drought_historical",
    "drought_current", "habitat", "landcover", "transport", "rainfall")
)
```

## Arguments

- name:

  Demo dataset to load. Currently one of `"aoi"`, `"pre"`, or `"post"`.

## Value

The requested demo object.
