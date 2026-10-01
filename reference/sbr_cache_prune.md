# Remove old cached files

Deletes cached files older than a specified age.

## Usage

``` r
sbr_cache_prune(
  max_age = 30,
  downloads = TRUE,
  temp = TRUE,
  composites = FALSE,
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

- dry_run:

  If TRUE, report what would be deleted.

## Value

Invisibly returns a data frame describing deleted files.
