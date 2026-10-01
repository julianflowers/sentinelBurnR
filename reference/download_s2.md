# Download Sentinel-2 assets

Download selected Sentinel-2 assets from an `sbr_search` object.
Existing cached files are skipped unless `overwrite = TRUE`.

## Usage

``` r
download_s2(
  x,
  assets = NULL,
  limit = NULL,
  max_cloud = NULL,
  project = NULL,
  output_dir = cache_downloads(),
  overwrite = FALSE,
  workers = NULL
)
```

## Arguments

- x:

  An `sbr_search` object.

- assets:

  Character vector of asset names.

- limit:

  Maximum number of scenes to download. If `NULL`, all scenes in the
  search are downloaded.

- max_cloud:

  Set numerical value for cloud cover cut off.

- project:

  Project name description

- output_dir:

  Directory used to cache downloaded files.

- overwrite:

  Logical. Overwrite existing files?

- workers:

  Number of parallel workers. If `NULL`, the number of workers is
  determined automatically using
  [`future::availableCores()`](https://parallelly.futureverse.org/reference/availableCores.html).
  Use `1` for sequential processing.#' @return An `sbr_collection`
  object.
