# Plot drought analysis

Plot vegetation moisture anomalies from an `sbr_drought` analysis.

## Usage

``` r
plot_drought(
  x,
  index = c("anomaly", "standardised"),
  boundary = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL
)
```

## Arguments

- x:

  An object of class `sbr_drought`.

- index:

  Drought product to plot. Either `"anomaly"` or `"standardised"`.

- boundary:

  Optional boundary to overlay.

- title:

  Optional plot title.

- subtitle:

  Optional plot subtitle.

- caption:

  Optional plot caption.

## Value

A ggplot object.
