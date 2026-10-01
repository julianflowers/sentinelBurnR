# Select Sentinel-2 acquisitions for a time series

Selects approximately regularly spaced Sentinel-2 acquisitions,
preferring acquisitions with lower mean cloud cover.

## Usage

``` r
select_timeseries(search, interval = 10, max_cloud = 30)
```

## Arguments

- search:

  An `sbr_search`.

- interval:

  Minimum interval between selected observations, in days.

- max_cloud:

  Maximum mean cloud cover percentage for an acquisition.

## Value

An `sbr_search` containing the selected STAC items.

## Details

Only complete acquisitions are retained. An acquisition is defined by
acquisition date and satellite, so observations from different
Sentinel-2 platforms are not mixed.
