make_test_search <- function(
        date = "2026-06-01",
        cloud_cover = 10,
        satellite = "sentinel-2a"
) {

    feature <- list(
        id = paste0("test_scene_", gsub("-", "", date)),
        properties = list(
            datetime = paste0(date, "T10:30:00Z"),
            platform = satellite,
            `eo:cloud_cover` = cloud_cover,
            `s2:cloud_shadow_percentage` = 2,
            `s2:medium_proba_clouds_percentage` = 3,
            `s2:high_proba_clouds_percentage` = 1,
            `s2:vegetation_percentage` = 60,
            `s2:water_percentage` = 5
        )
    )

    structure(
        list(
            items = list(
                features = list(feature)
            ),
            aoi = NULL,
            start = as.Date(date),
            end = as.Date(date)
        ),
        class = "sbr_search"
    )
}


make_test_collection <- function(
        date = "2026-06-01",
        cloud_cover = 10,
        satellite = "sentinel-2a"
) {

    search <- make_test_search(
        date = date,
        cloud_cover = cloud_cover,
        satellite = satellite
    )

    downloads <- data.frame(
        scene = paste0(
            "test_scene_",
            gsub("-", "", date)
        ),
        asset = c(
            "red",
            "nir08",
            "swir16",
            "swir22"
        ),
        file = c(
            "red.tif",
            "nir08.tif",
            "swir16.tif",
            "swir22.tif"
        ),
        stringsAsFactors = FALSE
    )

    new_s2_collection(
        files = downloads,
        search = search
    )
}

make_test_disk_collection <- function() {

    dates <- as.Date(c(
        "2026-05-01",
        "2026-05-11"
    ))

    assets <- c(
        "red",
        "nir08",
        "swir16",
        "swir22"
    )

    rows <- list()

    k <- 1

    for (i in seq_along(dates)) {

        date <- dates[i]

        for (asset in assets) {

            r <- terra::rast(
                nrows = 4,
                ncols = 4,
                xmin = 0,
                xmax = 40,
                ymin = 0,
                ymax = 40,
                crs = "EPSG:27700"
            )

            value <- switch(
                asset,
                red = 0.2,
                nir08 = if (
                    date == as.Date("2026-05-01")
                ) {
                    0.8
                } else {
                    0.6
                },
                swir16 = if (
                    date == as.Date("2026-05-01")
                ) {
                    0.3
                } else {
                    0.4
                },
                swir22 = if (
                    date == as.Date("2026-05-01")
                ) {
                    0.1
                } else {
                    0.3
                }
            )

            terra::values(r) <- value

            path <- tempfile(
                fileext = ".tif"
            )

            terra::writeRaster(
                r,
                path,
                overwrite = TRUE
            )

            rows[[k]] <- data.frame(
                scene = paste0(
                    "scene_",
                    format(date, "%Y%m%d")
                ),
                tile = "31UCT",
                date = date,
                satellite = "sentinel-2a",
                asset = asset,
                file = path,
                status = "downloaded",
                attempts = 1L,
                stringsAsFactors = FALSE
            )

            k <- k + 1
        }
    }

    files <- do.call(
        rbind,
        rows
    )

    new_s2_collection(
        files = files
    )
}


# nake test timeseries search ---------------------------------------------

make_test_timeseries_search <- function() {

    dates <- as.Date(c(
        "2026-05-01",
        "2026-05-06",
        "2026-05-12",
        "2026-05-23",
        "2026-06-04"
    ))

    clouds <- c(
        20,
        5,
        10,
        2,
        8
    )

    tiles <- c(
        "31UCT",
        "31UDT"
    )

    items <- purrr::map2(
        dates,
        clouds,
        \(date, cloud) {

            purrr::map(
                tiles,
                \(tile) {

                    list(
                        id = paste(
                            "S2A",
                            tile,
                            format(date, "%Y%m%d"),
                            "0_L2A",
                            sep = "_"
                        ),
                        properties = list(
                            datetime = paste0(
                                date,
                                "T10:00:00Z"
                            ),
                            platform = "sentinel-2a",
                            `eo:cloud_cover` = cloud
                        )
                    )
                }
            )
        }
    )

    structure(
        list(
            items = items,
            start = min(dates),
            end = max(dates)
        ),
        class = "sbr_search"
    )
}

# make test drought -------------------------------------------------------

make_test_drought <- function() {

    r <- terra::rast(
        nrows = 4,
        ncols = 4,
        xmin = 0,
        xmax = 4,
        ymin = 0,
        ymax = 4,
        crs = "EPSG:27700"
    )

    current <- r
    baseline_median <- r
    baseline_mean <- r
    baseline_sd <- r
    anomaly <- r
    standardised <- r

    terra::values(current) <- 0.2
    terra::values(baseline_median) <- 0.5
    terra::values(baseline_mean) <- 0.5
    terra::values(baseline_sd) <- 0.1
    terra::values(anomaly) <- -0.3
    terra::values(standardised) <- -3

    names(anomaly) <- "ndmi_anomaly"
    names(standardised) <- "ndmi_standardised"

    structure(
        list(
            current = current,

            baseline = list(
                median = baseline_median,
                mean = baseline_mean,
                sd = baseline_sd,
                n_years = 3
            ),

            anomaly = anomaly,
            standardised = standardised,

            summary = data.frame(
                current_date =
                    as.Date("2026-08-13"),
                baseline_start = 2020,
                baseline_end = 2022,
                baseline_years = 3,
                window_days = 30,
                valid_coverage = 1,
                current_ndmi_median = 0.2,
                anomaly_q25 = -0.3,
                anomaly_median = -0.3,
                anomaly_q75 = -0.3,
                proportion_negative = 1,
                proportion_below_minus_2sd = 1
            )
        ),
        class = "sbr_drought"
    )
}

make_test_rainfall <- function(
        years = c(2020:2022, 2026),
        start = "07-01",
        end = "07-29",
        precipitation_mm = 1
    ) {

        purrr::map_dfr(
            years,
            \(year) {

                tibble::tibble(
                    date = seq.Date(
                        as.Date(sprintf("%d-%s", year, start)),
                        as.Date(sprintf("%d-%s", year, end)),
                        by = "day"
                    ),
                    precipitation_mm = precipitation_mm
                )
            }
        )
}


make_test_temperature <- function(
        years = c(2020:2022, 2026),
        start = "07-01",
        end = "07-29"
    ) {

        purrr::map_dfr(
            years,
            \(year) {

                offset <- year - min(years)

                tibble::tibble(
                    date = seq.Date(
                        as.Date(sprintf("%d-%s", year, start)),
                        as.Date(sprintf("%d-%s", year, end)),
                        by = "day"
                    ),
                    temperature_c =
                        20 + offset +
                        seq(0, 4, length.out = 29)
                )
            }
        )

}

make_test_vegetation_collection <- function() {

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

    values <- list(
        red    = 0.2,
        nir08  = 0.6,
        swir16 = 0.3,
        swir22 = 0.25
    )

    files <- purrr::imap_chr(
        values,
        \(value, asset) {

            x <- r
            terra::values(x) <- value

            file <- tempfile(
                pattern = paste0(asset, "_"),
                fileext = ".tif"
            )

            terra::writeRaster(
                x,
                file,
                overwrite = TRUE
            )

            file
        }
    )

    collection$files$file <- unname(files)

    collection
}

