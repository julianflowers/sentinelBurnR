# Plot an RGB composite

Plot an RGB composite

## Usage

``` r
plot_rgb(
  x,
  title = "Sentinel-2 RGB composite",
  subtitle = NULL,
  rgb_stretch = c("lin", "hist", "none"),
  boundary = NULL
)
```

## Arguments

- x:

  A composite containing red, green and blue bands.

- title:

  Plot title.

- subtitle:

  Optional subtitle.

- rgb_stretch:

  Apply stretch - linear, histogram or none

- boundary:

  Optional boundary to overlay on the plot.

## Value

A ggplot object.
