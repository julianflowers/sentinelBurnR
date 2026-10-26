# Remove old cached files

Deletes cached files older than a specified age.

## Usage

``` r
sbr_cache_prune(
  max_age = 30,
  downloads = TRUE,
  temp = TRUE,
  composites = FALSE,
  asset = NULL,
  year = NULL,
  dry_run = TRUE
)
```

## Arguments

- max_age:

  Maximum age (days) to retain.

- downloads:

  Prune download cache.

- temp:

  Prune temporary files.

- composites:

  Prune composites cache.

- asset:

  Optional character vector of Sentinel-2 asset names to prune, for
  example `"visual"` or `"red"`. Applies to the downloads cache only.

- year:

  Optional numeric or integer vector of acquisition years to prune, for
  example `2025` or `2020:2024`.

- dry_run:

  If TRUE, report what would be deleted.

## Value

Invisibly returns a data frame describing deleted files.
