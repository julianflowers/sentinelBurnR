test_that("index_timeseries returns one row per date", {

    composites <- list(
        "2026-05-01" = make_test_composite(),
        "2026-05-11" = make_test_composite(),
        "2026-05-21" = make_test_composite()
    )

    out <- index_timeseries(
        composites,
        index = "nbr"
    )

    expect_s3_class(
        out,
        "data.frame"
    )

    expect_equal(
        nrow(out),
        3
    )

    expect_named(
        out,
        c(
            "date",
            "median",
            "q25",
            "q75",
            "valid_pixels"
        )
    )

    expect_s3_class(
        out$date,
        "Date"
    )
})


test_that("index_timeseries calculates correct NBR median", {

    comp <- make_test_composite()

    nbr <- calc_nbr(comp)

    expected <- stats::median(
        terra::values(
            nbr,
            mat = FALSE
        )
    )

    out <- index_timeseries(
        list(
            "2026-05-01" = comp
        ),
        index = "nbr"
    )

    expect_equal(
        out$median,
        expected
    )
})


test_that("index_timeseries rejects invalid dates", {

    expect_error(
        index_timeseries(
            list(
                "not-a-date" =
                    make_test_composite()
            )
        ),
        "valid dates"
    )
})

test_that("index_timeseries rejects unnamed composites", {

    composites <- list(
        make_test_composite(),
        make_test_composite()
    )

    expect_error(
        index_timeseries(composites),
        "valid dates"
    )
})

test_that("sentinel_timeseries builds an NBR time series", {

    collection <- make_test_disk_collection()

    out <- sentinel_timeseries(
        collection,
        index = "nbr"
    )

    expect_s3_class(
        out,
        "data.frame"
    )

    expect_equal(
        nrow(out),
        2
    )

    expect_equal(
        out$date,
        as.Date(c(
            "2026-05-01",
            "2026-05-11"
        ))
    )

    expect_gt(
        out$median[1],
        out$median[2]
    )
})

test_that("build_timeseries_composites builds one composite per date", {

    collection <- make_test_disk_collection()

    out <- build_timeseries_composites(
        collection,
        assets = c(
            "red",
            "nir08",
            "swir22"
        )
    )

    expect_type(
        out,
        "list"
    )

    expect_length(
        out,
        2
    )

    expect_named(
        out,
        c(
            "2026-05-01",
            "2026-05-11"
        )
    )

    expect_true(
        all(
            vapply(
                out,
                inherits,
                logical(1),
                what = "SpatRaster"
            )
        )
    )

    expect_equal(
        names(out[[1]]),
        c(
            "red",
            "nir08",
            "swir22"
        )
    )
})

test_that("dated composites preserve temporal differences", {

    collection <- make_test_disk_collection()

    composites <- build_timeseries_composites(
        collection,
        assets = c(
            "red",
            "nir08",
            "swir22"
        )
    )

    ts <- index_timeseries(
        composites,
        index = "nbr"
    )

    expect_equal(
        nrow(ts),
        2
    )

    expect_false(
        isTRUE(
            all.equal(
                ts$median[1],
                ts$median[2]
            )
        )
    )

    expect_gt(
        ts$median[1],
        ts$median[2]
    )
})

test_that("align_bands works without red band", {

    r10 <- terra::rast(
        nrows = 4,
        ncols = 4,
        xmin = 0,
        xmax = 40,
        ymin = 0,
        ymax = 40,
        crs = "EPSG:27700"
    )

    r20 <- terra::rast(
        nrows = 2,
        ncols = 2,
        xmin = 0,
        xmax = 40,
        ymin = 0,
        ymax = 40,
        crs = "EPSG:27700"
    )

    rasters <- list(
        nir08 = r10,
        swir22 = r20
    )

    out <- align_bands(rasters)

    expect_true(
        terra::compareGeom(
            out$nir08,
            out$swir22,
            stopOnError = FALSE
        )
    )

    expect_equal(
        terra::res(out$nir08),
        c(10, 10)
    )
})

test_that("select_timeseries enforces minimum temporal spacing", {

    search <- make_test_timeseries_search()

    out <- select_timeseries(
        search,
        interval = 10,
        max_cloud = 100
    )

    dates <- as.Date(
        vapply(
            out$items$features,
            function(x) {
                substr(
                    x$properties$datetime,
                    1,
                    10
                )
            },
            character(1)
        )
    )

    dates <- sort(
        unique(dates)
    )

    expect_true(
        all(
            diff(dates) >= 10
        )
    )
})

test_that("select_timeseries prefers clearer acquisitions", {

    search <- make_test_timeseries_search()

    out <- select_timeseries(
        search,
        interval = 10,
        max_cloud = 100
    )

    dates <- unique(
        as.Date(
            vapply(
                out$items$features,
                function(x) {
                    substr(
                        x$properties$datetime,
                        1,
                        10
                    )
                },
                character(1)
            )
        )
    )

    # 6 May is clearer than 1 May and is within
    # the exclusion interval.
    expect_true(
        as.Date("2026-05-06") %in% dates
    )

    expect_false(
        as.Date("2026-05-01") %in% dates
    )
})


test_that("select_timeseries applies cloud threshold", {

    search <- make_test_timeseries_search()

    out <- select_timeseries(
        search,
        interval = 10,
        max_cloud = 5
    )

    clouds <- vapply(
        out$items$features,
        function(x) {
            x$properties$`eo:cloud_cover`
        },
        numeric(1)
    )

    expect_true(
        all(clouds <= 5)
    )
})

test_that("s2_item_tile uses MGRS metadata", {

    item <- list(
        id = "something-unparseable",

        properties = list(
            `mgrs:utm_zone` = 31,
            `mgrs:latitude_band` = "U",
            `mgrs:grid_square` = "CT"
        )
    )

    expect_equal(
        sentinelBurnR:::s2_item_tile(item),
        "31UCT"
    )
})


test_that("s2_item_tile falls back to item ID", {

    item <- list(
        id = "S2A_30UXC_20260825_0_L2A",
        properties = list()
    )

    expect_equal(
        sentinelBurnR:::s2_item_tile(item),
        "30UXC"
    )
})

test_that("s2_item_tile rejects items without tile information", {

    item <- list(
        id = "unknown-item",
        properties = list()
    )

    expect_error(
        sentinelBurnR:::s2_item_tile(item),
        "Could not determine MGRS tile"
    )
})
test_that("plot_timeseries returns a ggplot", {

    x <- data.frame(
        date = as.Date(c(
            "2026-06-01",
            "2026-06-15",
            "2026-07-01"
        )),
        median = c(
            0.4,
            0.3,
            0.2
        ),
        q25 = c(
            0.3,
            0.2,
            0.1
        ),
        q75 = c(
            0.5,
            0.4,
            0.3
        )
    )

    p <- plot_timeseries(
        x,
        index = "NBR"
    )

    expect_s3_class(
        p,
        "ggplot"
    )
})


test_that("plot_timeseries requires summary columns", {

    x <- data.frame(
        date = as.Date(
            "2026-06-01"
        ),
        median = 0.4
    )

    expect_error(
        plot_timeseries(x),
        "`x` must contain"
    )
})


test_that("plot_timeseries requires Date values", {

    x <- data.frame(
        date = "2026-06-01",
        median = 0.4,
        q25 = 0.3,
        q75 = 0.5
    )

    expect_error(
        plot_timeseries(x),
        "must be a Date vector"
    )
})

test_that("timeseries_change calculates consecutive changes", {

    x <- data.frame(
        date = as.Date(c(
            "2026-06-01",
            "2026-06-15",
            "2026-07-01"
        )),
        median = c(
            0.5,
            0.4,
            0.1
        )
    )

    out <- timeseries_change(x)

    expect_equal(
        out$change,
        c(
            -0.1,
            -0.3
        )
    )

    expect_equal(
        out$start_date,
        as.Date(c(
            "2026-06-01",
            "2026-06-15"
        ))
    )

    expect_equal(
        out$end_date,
        as.Date(c(
            "2026-06-15",
            "2026-07-01"
        ))
    )
})

test_that("timeseries_change sorts observations by date", {

    x <- data.frame(
        date = as.Date(c(
            "2026-07-01",
            "2026-06-01",
            "2026-06-15"
        )),
        median = c(
            0.1,
            0.5,
            0.4
        )
    )

    out <- timeseries_change(x)

    expect_equal(
        out$start_date,
        as.Date(c(
            "2026-06-01",
            "2026-06-15"
        ))
    )

    expect_equal(
        out$change,
        c(
            -0.1,
            -0.3
        )
    )
})

test_that("timeseries_change requires at least two observations", {

    x <- data.frame(
        date = as.Date(
            "2026-06-01"
        ),
        median = 0.4
    )

    expect_error(
        timeseries_change(x),
        "at least two observations"
    )
})


test_that("timeseries_change requires date and median columns", {

    x <- data.frame(
        date = as.Date(
            "2026-06-01"
        )
    )

    expect_error(
        timeseries_change(x),
        "must contain"
    )
})

test_that("detect_disturbance identifies large negative changes", {

    x <- data.frame(
        date = as.Date(c(
            "2026-06-01",
            "2026-06-15",
            "2026-07-01",
            "2026-07-15"
        )),
        median = c(
            0.5,
            0.46,
            0.10,
            0.08
        )
    )

    out <- detect_disturbance(
        x,
        threshold = -0.2
    )

    expect_equal(
        nrow(out),
        1
    )

    expect_equal(
        out$start_date,
        as.Date("2026-06-15")
    )

    expect_equal(
        out$end_date,
        as.Date("2026-07-01")
    )

    expect_equal(
        out$change,
        -0.36
    )
})


test_that("detect_disturbance can return no disturbances", {

    x <- data.frame(
        date = as.Date(c(
            "2026-06-01",
            "2026-06-15",
            "2026-07-01"
        )),
        median = c(
            0.5,
            0.45,
            0.42
        )
    )

    out <- detect_disturbance(
        x,
        threshold = -0.2
    )

    expect_equal(
        nrow(out),
        0
    )
})


test_that("detect_disturbance validates threshold", {

    x <- data.frame(
        date = as.Date(c(
            "2026-06-01",
            "2026-06-15"
        )),
        median = c(
            0.5,
            0.2
        )
    )

    expect_error(
        detect_disturbance(
            x,
            threshold = 0.2
        ),
        "must be a negative number"
    )
})

test_that("disturbance_dates selects strongest disturbance", {

    x <- data.frame(
        date = as.Date(c(
            "2026-06-01",
            "2026-06-15",
            "2026-07-01",
            "2026-07-15"
        )),
        median = c(
            0.5,
            0.2,
            0.18,
            -0.3
        )
    )

    out <- disturbance_dates(
        x,
        threshold = -0.2
    )

    expect_equal(
        out$pre,
        as.Date("2026-07-01")
    )

    expect_equal(
        out$post,
        as.Date("2026-07-15")
    )

    expect_equal(
        out$change,
        -0.48
    )
})


test_that("disturbance_dates rejects series without disturbance", {

    x <- data.frame(
        date = as.Date(c(
            "2026-06-01",
            "2026-06-15",
            "2026-07-01"
        )),
        median = c(
            0.5,
            0.45,
            0.42
        )
    )

    expect_error(
        disturbance_dates(x),
        "No candidate disturbance"
    )
})

test_that("index_timeseries calculates MSI", {

    r <- make_test_composite()

    composites <- list(
        "2026-06-01" = r,
        "2026-06-15" = r
    )

    out <- index_timeseries(
        composites,
        index = "msi"
    )

    expected <- calc_msi(r)

    expected_median <- stats::median(
        terra::values(
            expected,
            mat = FALSE
        ),
        na.rm = TRUE
    )

    expect_equal(
        out$median,
        rep(
            expected_median,
            2
        )
    )
})

test_that("antecedent_conditions summarises antecedent conditions", {

    dates <- as.Date(c(
        "2026-06-01",
        "2026-06-15",
        "2026-07-01"
    ))

    ndvi <- data.frame(
        date = dates,
        median = c(
            0.5,
            0.7,
            0.6
        )
    )

    ndmi <- data.frame(
        date = dates,
        median = c(
            0.1,
            0.2,
            0.05
        )
    )

    out <- antecedent_conditions(
        ndvi,
        ndmi,
        date = as.Date("2026-07-01"),
        window = 45
    )

    expect_equal(
        out$ndvi_current,
        0.6
    )

    expect_equal(
        out$ndvi_peak,
        0.7
    )

    expect_equal(
        out$ndvi_change,
        -0.1
    )

    expect_equal(
        out$ndmi_current,
        0.05
    )

    expect_equal(
        out$ndmi_peak,
        0.2
    )

    expect_equal(
        out$ndmi_change,
        -0.15
    )

    expect_true(
        is.finite(out$ndvi_trend)
    )

    expect_true(
        is.finite(out$ndmi_trend)
    )
})

test_that("antecedent_conditions respects look-back window", {

    dates <- as.Date(c(
        "2026-05-01",
        "2026-06-01",
        "2026-06-15",
        "2026-07-01"
    ))

    ndvi <- data.frame(
        date = dates,
        median = c(
            0.9,
            0.5,
            0.7,
            0.6
        )
    )

    ndmi <- data.frame(
        date = dates,
        median = c(
            0.4,
            0.1,
            0.2,
            0.05
        )
    )

    out <- antecedent_conditions(
        ndvi,
        ndmi,
        date = as.Date("2026-07-01"),
        window = 31
    )

    # The high May values lie outside the window.
    expect_equal(
        out$ndvi_peak,
        0.7
    )

    expect_equal(
        out$ndmi_peak,
        0.2
    )
})

test_that("antecedent_conditions requires sufficient observations", {

    dates <- as.Date(c(
        "2026-05-01",
        "2026-07-01"
    ))

    x <- data.frame(
        date = dates,
        median = c(
            0.5,
            0.4
        )
    )

    expect_error(
        antecedent_conditions(
            x,
            x,
            date = as.Date("2026-07-01"),
            window = 10
        ),
        "At least two observations"
    )
})

test_that("index_trend calculates per-cell trends", {

    make_composite <- function(
        nir,
        swir
    ) {

        nir_r <- terra::rast(
            nrows = 2,
            ncols = 2,
            xmin = 0,
            xmax = 2,
            ymin = 0,
            ymax = 2
        )

        swir_r <- nir_r

        terra::values(nir_r) <- nir
        terra::values(swir_r) <- swir

        x <- c(
            nir_r,
            swir_r
        )

        names(x) <- c(
            "nir08",
            "swir16"
        )

        x
    }

    composites <- list(
        "2026-06-01" = make_composite(
            c(0.8, 0.8, 0.8, 0.8),
            c(0.2, 0.2, 0.2, 0.2)
        ),

        "2026-06-11" = make_composite(
            c(0.8, 0.8, 0.8, 0.8),
            c(0.3, 0.2, 0.1, 0.2)
        ),

        "2026-06-21" = make_composite(
            c(0.8, 0.8, 0.8, 0.8),
            c(0.4, 0.2, 0.05, 0.2)
        )
    )

    out <- index_trend(
        composites,
        index = "ndmi"
    )

    expect_s4_class(
        out,
        "SpatRaster"
    )

    expect_equal(
        names(out),
        "ndmi_trend"
    )

    values <- terra::values(
        out,
        mat = FALSE
    )

    # Cell 1 becomes wetter in SWIR terms, so NDMI declines.
    expect_lt(
        values[1],
        0
    )

    # Cell 2 is unchanged.
    expect_equal(
        values[2],
        0,
        tolerance = 1e-10
    )

    # Cell 3 has falling SWIR and therefore increasing NDMI.
    expect_gt(
        values[3],
        0
    )

    # Cell 4 is unchanged.
    expect_equal(
        values[4],
        0,
        tolerance = 1e-10
    )
})

test_that("index_trend respects date range", {

    composites <- list(
        "2026-06-01" = make_test_composite(),
        "2026-06-15" = make_test_composite(),
        "2026-07-01" = make_test_composite()
    )

    expect_error(
        index_trend(
            composites,
            index = "ndmi",
            start = as.Date("2026-06-15"),
            min_obs = 3
        ),
        "Fewer than `min_obs` observations"
    )
})

test_that("index_timeseries projects boundary to raster CRS", {

    comps <- list(
        "2026-07-01" = make_test_composite(),
        "2026-07-11" = make_test_composite()
    )

    r <- comps[[1]]

    boundary <- terra::as.polygons(
        terra::ext(r)
    )
    terra::crs(boundary) <- terra::crs(r)

    # Deliberately put boundary into another CRS
    boundary <- terra::project(
        boundary,
        "EPSG:4326"
    )

    result <- index_timeseries(
        comps,
        index = "ndmi",
        boundary = boundary
    )

    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 2)
})

