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

    fixture_dir <- tempfile(
        pattern = "sentinelBurnR-composite-"
    )

    dir.create(
        fixture_dir,
        recursive = TRUE
    )

    on.exit(
        unlink(
            fixture_dir,
            recursive = TRUE,
            force = TRUE
        ),
        add = TRUE
    )

    dates <- as.Date(
        c(
            "2026-06-01",
            "2026-06-11"
        )
    )

    assets <- c(
        "red",
        "green",
        "blue",
        "nir08",
        "swir22",
        "scl"
    )

    rows <- list()

    for (tile_index in 0:1) {

        tile <- paste0(
            "T",
            tile_index + 1L
        )

        extent_10 <- terra::ext(
            tile_index * 40,
            tile_index * 40 + 40,
            0,
            40
        )

        for (date_index in seq_along(dates)) {

            scene_date <- dates[[date_index]]

            for (asset_index in seq_along(assets)) {

                asset_name <- assets[[asset_index]]

                is_20_m <- asset_name %in% c(
                    "swir22",
                    "scl"
                )

                raster <- terra::rast(
                    nrows = if (is_20_m) 2 else 4,
                    ncols = if (is_20_m) 2 else 4,
                    extent = extent_10,
                    crs = "EPSG:32631"
                )

                values <- if (asset_name == "scl") {

                    c(
                        4,
                        8,
                        5,
                        9
                    )

                } else {

                    seq_len(
                        terra::ncell(raster)
                    ) +
                        tile_index * 100 +
                        date_index * 10
                }

                terra::values(raster) <- values

                path <- file.path(
                    fixture_dir,
                    paste(
                        tile,
                        asset_name,
                        scene_date,
                        "tif",
                        sep = "."
                    )
                )

                terra::writeRaster(
                    raster,
                    path,
                    overwrite = TRUE
                )

                rows[[length(rows) + 1L]] <- data.frame(
                    scene = paste(
                        tile,
                        scene_date,
                        sep = "_"
                    ),
                    tile = tile,
                    date = scene_date,
                    satellite = "sentinel-2c",
                    asset = asset_name,
                    file = path,
                    stringsAsFactors = FALSE
                )
            }
        }
    }

})
