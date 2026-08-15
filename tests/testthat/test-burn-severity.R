test_that("classify_burn_severity assigns correct classes", {

    x <- terra::rast(
        nrows = 1,
        ncols = 7
    )

    terra::values(x) <- c(
        -0.30,
        -0.20,
        0.00,
        0.20,
        0.35,
        0.55,
        0.75
    )

    severity <- classify_burn_severity(x)

    expect_equal(
        as.numeric(terra::values(severity)),
        1:7
    )

    expect_equal(
        names(severity),
        "severity"
    )
})

test_that("classify_burn_severity validates input", {

    expect_error(
        classify_burn_severity(1:10),
        "SpatRaster"
    )

    x <- terra::rast(
        nrows = 2,
        ncols = 2,
        nlyrs = 2
    )

    expect_error(
        classify_burn_severity(x),
        "one layer"
    )
})
