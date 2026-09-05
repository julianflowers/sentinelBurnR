test_that("annual_index_composites gives equal weight to years", {

    make_raster <- function(value) {
        x <- terra::rast(
            nrows = 2,
            ncols = 2,
            xmin = 0,
            xmax = 2,
            ymin = 0,
            ymax = 2
        )

        terra::values(x) <- value

        x
    }

    x <- list(
        "2020-07-20" = make_raster(0.2),
        "2020-08-20" = make_raster(0.4),
        "2021-08-10" = make_raster(0.8)
    )

    result <- annual_index_composites(x)

    expect_equal(
        names(result),
        c("2020", "2021")
    )

    expect_equal(
        terra::values(result[["2020"]])[, 1],
        rep(0.3, 4)
    )

    expect_equal(
        terra::values(result[["2021"]])[, 1],
        rep(0.8, 4)
    )
})

test_that("build_drought_baseline calculates annual baseline", {

    make_raster <- function(value) {
        x <- terra::rast(
            nrows = 2,
            ncols = 2
        )

        terra::values(x) <- value
        x
    }

    annual <- list(
        "2020" = make_raster(0.2),
        "2021" = make_raster(0.4),
        "2022" = make_raster(0.6)
    )

    result <- build_drought_baseline(
        annual
    )

    expect_equal(
        terra::values(result$median)[, 1],
        rep(0.4, 4)
    )

    expect_equal(
        terra::values(result$mean)[, 1],
        rep(0.4, 4)
    )

    expect_equal(
        result$n_years,
        3
    )
})

test_that("build_drought_baseline calculates annual baseline", {

    make_raster <- function(value) {
        x <- terra::rast(
            nrows = 2,
            ncols = 2
        )

        terra::values(x) <- value
        x
    }

    annual <- list(
        "2020" = make_raster(0.2),
        "2021" = make_raster(0.4),
        "2022" = make_raster(0.6)
    )

    result <- build_drought_baseline(
        annual
    )

    expect_equal(
        terra::values(result$median)[, 1],
        rep(0.4, 4)
    )

    expect_equal(
        terra::values(result$mean)[, 1],
        rep(0.4, 4)
    )

    expect_equal(
        result$n_years,
        3
    )
})

test_that("raster_coverage measures valid pixels", {

    x1 <- terra::rast(
        nrows = 2,
        ncols = 2
    )

    x2 <- x1

    terra::values(x1) <- c(
        1, 1, 1, 1
    )

    terra::values(x2) <- c(
        1, 1, NA, NA
    )

    x <- list(
        "2020-08-01" = x1,
        "2020-08-15" = x2
    )

    result <- raster_coverage(x)

    expect_equal(
        result$valid_pixels,
        c(4, 2)
    )

    expect_equal(
        result$coverage,
        c(1, 0.5)
    )
})

test_that("filter_raster_coverage removes poor observations", {

    x1 <- terra::rast(
        nrows = 2,
        ncols = 2
    )

    x2 <- x1

    terra::values(x1) <- 1

    terra::values(x2) <- c(
        1, 1, NA, NA
    )

    x <- list(
        "2020-08-01" = x1,
        "2020-08-15" = x2
    )

    result <- filter_raster_coverage(
        x,
        min_coverage = 0.90
    )

    expect_equal(
        names(result$rasters),
        "2020-08-01"
    )

    expect_equal(
        result$reference_pixels,
        4
    )
})

test_that("raster_coverage uses supplied reference pixel count", {

    x <- terra::rast(
        nrows = 2,
        ncols = 2
    )

    terra::values(x) <- c(
        1, 1, 1, NA
    )

    result <- raster_coverage(
        list("2026-08-13" = x),
        reference_pixels = 4
    )

    expect_equal(
        result$coverage,
        0.75
    )
})

test_that("analyse_drought detects negative vegetation moisture anomaly", {

    make_raster <- function(value) {

        x <- terra::rast(
            nrows = 2,
            ncols = 2,
            xmin = 0,
            xmax = 2,
            ymin = 0,
            ymax = 2
        )

        terra::values(x) <- value

        x
    }

    historical_ndmi <- list(
        "2020-08-10" = make_raster(0.40),
        "2021-08-10" = make_raster(0.50),
        "2022-08-10" = make_raster(0.60)
    )

    current_ndmi <- list(
        "2026-08-13" = make_raster(0.20)
    )

    historical <- structure(
        list(),
        class = "sbr_collection"
    )

    current <- structure(
        list(),
        class = "sbr_collection"
    )

    call_number <- 0

    testthat::local_mocked_bindings(

        build_timeseries_composites = function(
        collection,
        assets
        ) {
            collection
        },

        index_rasters = function(
        composites,
        index,
        boundary = NULL
        ) {

            call_number <<- call_number + 1

            if (call_number == 1) {
                historical_ndmi
            } else {
                current_ndmi
            }
        }

    )

    result <- analyse_drought(
        historical = historical,
        current = current,
        current_date = "2026-08-13",
        window_days = 30,
        min_coverage = 0.90,
        min_years = 3
    )

    expect_s3_class(
        result,
        "sbr_drought"
    )

    expect_equal(
        names(result$annual),
        c("2020", "2021", "2022")
    )

    expect_equal(
        terra::values(
            result$baseline$median
        )[, 1],
        rep(0.50, 4)
    )

    expect_equal(
        terra::values(
            result$anomaly
        )[, 1],
        rep(-0.30, 4)
    )

    expect_true(
        result$summary$anomaly_median < 0
    )

    expect_equal(
        result$summary$baseline_years,
        3
    )
})

test_that("plot_drought returns ggplot", {

    x <- make_test_drought()

    p <- plot_drought(x)

    expect_s3_class(
        p,
        "ggplot"
    )
})

test_that("plot_drought plots standardised anomaly", {

    x <- make_test_drought()

    p <- plot_drought(
        x,
        index = "standardised"
    )

    expect_s3_class(
        p,
        "ggplot"
    )
})


