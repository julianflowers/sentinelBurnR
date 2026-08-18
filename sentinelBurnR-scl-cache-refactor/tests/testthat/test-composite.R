test_that("build_composite returns a raster", {

    skip_if_not(
        exists("pre_collection")
    )

    comp <- build_composite(
        pre_collection
    )

    expect_s4_class(
        comp,
        "SpatRaster"
    )

})

test_that("cached composite matches per-band SCL preparation", {

    test_dir <- testthat::local_tempdir()
    dates <- as.Date(c("2026-06-01", "2026-06-11"))
    assets <- c("red", "green", "blue", "nir08", "swir22", "scl")
    rows <- list()

    for (tile_index in 0:1) {
        tile <- paste0("T", tile_index + 1L)
        extent_10 <- terra::ext(tile_index * 40, tile_index * 40 + 40, 0, 40)

        for (date_index in seq_along(dates)) {
            for (asset in assets) {
                is_20_m <- asset %in% c("swir22", "scl")
                raster <- terra::rast(
                    nrows = if (is_20_m) 2 else 4,
                    ncols = if (is_20_m) 2 else 4,
                    extent = extent_10,
                    crs = "EPSG:32631"
                )

                values <- if (asset == "scl") {
                    c(4, 8, 5, 9)
                } else {
                    seq_len(terra::ncell(raster)) +
                        tile_index * 100 + date_index * 10
                }
                terra::values(raster) <- values

                path <- file.path(
                    test_dir,
                    paste(tile, asset, dates[[date_index]], "tif", sep = ".")
                )
                terra::writeRaster(raster, path)
                rows[[length(rows) + 1L]] <- data.frame(
                    scene = paste(tile, dates[[date_index]], sep = "_"),
                    tile = tile,
                    date = dates[[date_index]],
                    satellite = "sentinel-2c",
                    asset = asset,
                    file = path
                )
            }
        }
    }

    collection <- new_s2_collection(do.call(rbind, rows))
    requested <- c("red", "green", "blue", "nir08", "swir22")

    actual <- build_composite(collection, requested)

    scl_stacks <- read_band(collection, "scl")
    legacy_bands <- stats::setNames(vector("list", length(requested)), requested)
    for (asset in requested) {
        stacks <- read_band(collection, asset)
        per_band_masks <- lapply(
            names(stacks),
            function(tile) prepare_scl_mask(scl_stacks[[tile]], stacks[[tile]])
        )
        names(per_band_masks) <- names(stacks)
        legacy_bands[[asset]] <- build_band(
            collection,
            asset,
            stacks = stacks,
            scl_masks = per_band_masks
        )
    }
    legacy_bands <- align_bands(legacy_bands)
    expected <- do.call(c, unclass(legacy_bands))
    names(expected) <- requested

    expect_equal(terra::values(actual), terra::values(expected), tolerance = 0)
    expect_true(terra::compareGeom(actual, expected))
})
