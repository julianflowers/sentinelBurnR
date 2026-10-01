# Plot a continuous raster index

Plot a continuous raster index

## Usage

``` r
plot_index(
  x,
  index,
  title = NULL,
  subtitle = NULL,
  boundary = NULL,
  caption = NULL,
  basemap = FALSE,
  basemap_type = "stamen_toner_background",
  basemap_zoom = NULL,
  index_alpha = 0.75
)
```

## Arguments

- x:

  A `SpatRaster` containing the index to plot.

- index:

  Character name of the spectral index.

- title:

  Optional plot title.

- subtitle:

  Optional plot subtitle.

- boundary:

  Optional spatial boundary to overlay on the plot.

- caption:

  Optional plot caption.

- basemap:

  Optional basemap default is FALSE

- basemap_type:

  haracter. Basemap type to use. Supported values include Stadia map
  styles such as `"stamen_toner_background"` and `"alidade_smooth"`, and
  `"satellite"` for satellite imagery.

- basemap_zoom:

  Integer or `NULL`. Basemap zoom level. If `NULL`, an appropriate zoom
  is selected for standard Stadia maps. Satellite imagery uses the
  default satellite zoom.

- index_alpha:

  Numeric between 0 and 1. Opacity of the index raster when plotted over
  a basemap. Default is `0.75`.

## Examples

``` r
if (FALSE) { # \dontrun{
plot_index(
    x,
    index = "ndvi",
    basemap = TRUE,
    basemap_type = "satellite",
    basemap_zoom = 14,
    index_alpha = 0.65
)
} # }
```
