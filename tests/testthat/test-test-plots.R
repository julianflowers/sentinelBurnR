test_that("plot_rgb returns ggplot", {

    comp <- make_test_composite()

    p <- plot_rgb(comp)

    expect_s3_class(
        p,
        "ggplot"
    )

})

test_that("plot_severity returns ggplot", {

    x <- terra::rast(
        nrows = 2,
        ncols = 2,
        xmin = 0,
        xmax = 20,
        ymin = 0,
        ymax = 20,
        crs = "EPSG:27700"
    )

    terra::values(x) <- c(
        1, 3,
        5, 7
    )

    names(x) <- "severity"

    p <- plot_severity(x)

    expect_s3_class(
        p,
        "ggplot"
    )
})

test_that("plot_severity rejects unclassified raster", {

    x <- terra::rast(
        nrows = 2,
        ncols = 2
    )

    terra::values(x) <- c(
        0.05, 0.2,
        0.4, 0.7
    )

    expect_error(
        plot_severity(x),
        "classified burn-severity raster"
    )
})


test_climate <- list(
    date = as.Date("2026-08-25"),
    baseline_years = 1991:2020,

    summary = data.frame(
        window_days = c(30, 60, 90),
        current_mm = c(22.8, 30.6, 91.8),
        baseline_median_mm = c(61.8, 122.5, 183.6),
        anomaly_mm = c(-39.0, -92.0, -91.8),
        percent_of_normal = c(36.9, 24.9, 50.0),
        historical_percentile = c(6.7, 0, 0)
    ),

    temperature = data.frame(
        window_days = c(30, 60, 90),
        mean_max_anomaly_c = c(1.74, 1.88, 2.01)
    )
)

class(test_climate) <- "sbr_climate"


test_that("plot_climate plots rainfall", {

    p <- plot_climate(
        test_climate,
        metric = "rainfall"
    )

    expect_s3_class(
        p,
        "ggplot"
    )

    expect_equal(
        p$labels$y,
        "Rainfall (% of normal)"
    )
})


test_that("plot_climate plots temperature", {

    p <- plot_climate(
        test_climate,
        metric = "temperature"
    )

    expect_s3_class(
        p,
        "ggplot"
    )

    expect_equal(
        p$labels$y,
        "Mean maximum temperature anomaly (\u00b0C)"
    )
})


test_that("plot_climate rejects invalid metrics", {

    expect_error(
        plot_climate(
            test_climate,
            metric = "humidity"
        )
    )
})


test_that("plot_climate rejects invalid metrics", {

    expect_error(
        plot_climate(
            climate,
            metric = "humidity"
        )
    )
})

test_that("plot_severity handles missing severity classes", {

    x <- terra::rast(
        nrows = 2,
        ncols = 3,
        xmin = 0,
        xmax = 3,
        ymin = 0,
        ymax = 2
    )

    terra::values(x) <- 1:6

    p <- plot_severity(x)

    expect_s3_class(
        p,
        "ggplot"
    )
})

