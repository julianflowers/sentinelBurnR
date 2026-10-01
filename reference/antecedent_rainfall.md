# Calculate antecedent rainfall

Calculates total rainfall over a specified number of days ending on a
given date. This can be used to describe recent rainfall conditions
preceding an observation, event, or satellite acquisition.

## Usage

``` r
antecedent_rainfall(date, rainfall, days = 30)
```

## Arguments

- date:

  A `Date` giving the final day of the antecedent rainfall period.

- rainfall:

  A data frame containing `date` and `precipitation_mm` columns.

- days:

  Number of days over which rainfall is accumulated. Defaults to 30.

## Value

A data frame with one row containing:

- date:

  The final date of the accumulation period.

- rainfall_mm:

  Total precipitation over the period, in millimetres.

- n_days:

  Number of rainfall records included in the calculation.
