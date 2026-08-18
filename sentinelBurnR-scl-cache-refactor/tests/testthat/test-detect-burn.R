test_that("detect_burn classifies cells using threshold", {

    x <- terra::rast(
        nrows = 2,
        ncols = 2
    )

    terra::values(x) <- c(
        0.10,
        0.27,
        0.40,
        -0.10
    )

    burned <- detect_burn(
        x,
        threshold = 0.27
    )

    expect_equal(
        as.numeric(terra::values(burned)),
        c(0, 1, 1, 0)
    )

    expect_equal(
        names(burned),
        "burned"
    )
})

test_that("detect_burn validates its input", {

    expect_error(
        detect_burn(1:10),
        "SpatRaster"
    )

    x <- terra::rast(
        nrows = 2,
        ncols = 2,
        nlyrs = 2
    )

    expect_error(
        detect_burn(x),
        "one layer"
    )

    x <- terra::rast(
        nrows = 2,
        ncols = 2
    )

    expect_error(
        detect_burn(
            x,
            threshold = "high"
        ),
        "numeric"
    )
})
