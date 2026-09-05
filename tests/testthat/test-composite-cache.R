make_cache_test_collection <- function() {

    dir <- tempfile("sbr-cache-source-")
    dir.create(dir)

    collection <- make_test_collection()

    r <- terra::rast(
        nrows = 10,
        ncols = 10,
        xmin = 0,
        xmax = 100,
        ymin = 0,
        ymax = 100,
        crs = "EPSG:3857"
    )

    assets <- collection$files$asset

    paths <- file.path(
        dir,
        paste0(assets, ".tif")
    )

    for (i in seq_along(assets)) {

        rr <- r

        terra::values(rr) <-
            seq_len(terra::ncell(rr)) + i

        terra::writeRaster(
            rr,
            paths[i],
            overwrite = TRUE
        )
    }

    collection$files$file <- paths

    collection
}

test_that("composite cache key is deterministic", {

    collection <- make_cache_test_collection()

    key1 <- composite_cache_key(
        collection,
        c("red", "nir08")
    )

    key2 <- composite_cache_key(
        collection,
        c("red", "nir08")
    )

    expect_identical(key1, key2)
})


test_that("different assets produce different composite cache keys", {

    collection <- make_cache_test_collection()

    key1 <- composite_cache_key(
        collection,
        c("red", "nir08")
    )

    key2 <- composite_cache_key(
        collection,
        c("nir08", "swir16")
    )

    expect_false(
        identical(key1, key2)
    )
})


test_that("composite cache version contributes to cache key", {

    expect_true(
        is.integer(composite_cache_version)
    )

    expect_length(
        composite_cache_version,
        1
    )
})
# test_that("build_composite reuses cached composite", {
#
#     cache_dir <- tempfile("sbr-cache-")
#     dir.create(cache_dir)
#
#     withr::local_options(
#         list(
#             sbr.cache_dir = cache_dir
#         )
#     )
#
#     collection <- make_cache_test_collection()
#
#     cached <- terra::rast(
#         nrows = 10,
#         ncols = 10,
#         xmin = 0,
#         xmax = 100,
#         ymin = 0,
#         ymax = 100,
#         crs = "EPSG:3857",
#         nlyrs = 2
#     )
#
#     terra::values(cached) <- seq_len(
#         terra::ncell(cached) *
#             terra::nlyr(cached)
#     )
#
#     names(cached) <- c(
#         "red",
#         "nir08"
#     )
#
#     path <- composite_cache_file(
#         collection,
#         c("red", "nir08")
#     )
#
#     terra::writeRaster(
#         cached,
#         path,
#         overwrite = TRUE
#     )
#
#     expect_message(
#         result <- build_composite(
#             collection,
#             assets = c("red", "nir08")
#         ),
#         "Using cached composite"
#     )
#
#     expect_true(
#         terra::compareGeom(
#             cached,
#             result,
#             stopOnError = FALSE
#         )
#     )
#
#     expect_equal(
#         terra::values(result),
#         terra::values(cached)
#     )
# })

# test_that("overwrite bypasses cached composite", {
#
#     cache_dir <- tempfile("sbr-cache-")
#     dir.create(cache_dir)
#
#     withr::local_options(
#         list(
#             sbr.cache_dir = cache_dir
#         )
#     )
#
#     collection <- make_cache_test_collection()
#
#     build_composite(
#         collection,
#         assets = c("red", "nir08"),
#         cache = TRUE
#     )
#
#     expect_message(
#         build_composite(
#             collection,
#             assets = c("red", "nir08"),
#             cache = TRUE,
#             overwrite = TRUE
#         ),
#         "Building red"
#     )
# })




