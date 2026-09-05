test_that("seasonal_baseline calculates pixel median", {

    r1 <- terra::rast(
        nrows = 2,
        ncols = 2,
        xmin = 0,
        xmax = 2,
        ymin = 0,
        ymax = 2
    )

    r2 <- r1
    r3 <- r1

    terra::values(r1) <- c(1, 2, 3, 4)
    terra::values(r2) <- c(2, 3, 4, 5)
    terra::values(r3) <- c(3, 4, 5, 6)

    out <- seasonal_baseline(
        list(r1, r2, r3),
        min_years = 1
    )

    expect_equal(
        terra::values(out$baseline)[, 1],
        c(2, 3, 4, 5)
    )

    expect_equal(
        terra::values(out$n_years)[, 1],
        rep(3, 4)
    )
})


test_that("seasonal_baseline supports mean", {

    r1 <- terra::rast(nrows = 1, ncols = 2)
    r2 <- r1

    terra::values(r1) <- c(1, 4)
    terra::values(r2) <- c(3, 8)

    out <- seasonal_baseline(
        list(r1, r2),
        method = "mean",
        min_years = 1
    )

    expect_equal(
        terra::values(out$baseline)[, 1],
        c(2, 6)
    )
})


test_that("seasonal_baseline enforces min_years", {

    r1 <- terra::rast(nrows = 1, ncols = 2)
    r2 <- r1
    r3 <- r1

    terra::values(r1) <- c(1, 1)
    terra::values(r2) <- c(2, NA)
    terra::values(r3) <- c(3, NA)

    out <- seasonal_baseline(
        list(r1, r2, r3),
        min_years = 2
    )

    values <- terra::values(out$baseline)[, 1]

    expect_equal(values[1], 2)
    expect_true(is.na(values[2]))
})


test_that("seasonal_baseline validates input", {

    expect_error(
        seasonal_baseline(list()),
        "non-empty"
    )

    expect_error(
        seasonal_baseline(list(1, 2)),
        "SpatRaster"
    )

    r <- terra::rast(nrows = 1, ncols = 1)
    terra::values(r) <- 1

    expect_error(
        seasonal_baseline(
            list(r),
            min_years = 0
        ),
        "positive"
    )
})


test_that("index_anomaly calculates current minus baseline", {

    current <- terra::rast(nrows = 1, ncols = 3)
    baseline <- current

    terra::values(current) <- c(0.1, 0.2, 0.4)
    terra::values(baseline) <- c(0.2, 0.2, 0.3)

    out <- index_anomaly(
        current,
        baseline
    )

    expect_equal(
        terra::values(out)[, 1],
        c(-0.1, 0, 0.1),
        tolerance = 1e-10
    )

    expect_equal(
        names(out),
        "anomaly"
    )
})


test_that("index_anomaly accepts seasonal baseline result", {

    historical <- terra::rast(nrows = 1, ncols = 2)
    current <- historical

    terra::values(historical) <- c(0.3, 0.4)
    terra::values(current) <- c(0.2, 0.5)

    baseline <- seasonal_baseline(
        list(historical),
        min_years = 1
    )

    out <- index_anomaly(
        current,
        baseline
    )

    expect_equal(
        terra::values(out)[, 1],
        c(-0.1, 0.1),
        tolerance = 1e-10
    )
})


