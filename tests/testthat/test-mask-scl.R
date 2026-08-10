test_that("mask_scl preserves class", {

    r <- terra::rast(
        nrows = 4,
        ncols = 4,
        vals = 1:16
    )

    s <- terra::rast(
        nrows = 4,
        ncols = 4,
        vals = c(
            4,4,8,8,
            4,5,8,8,
            6,7,9,9,
            4,4,4,4
        )
    )

    out <- mask_scl(
        r,
        s
    )

    expect_s4_class(
        out,
        "SpatRaster"
    )

})

test_that("mask_scl masks cloud pixels", {

    r <- terra::rast(
        nrows = 2,
        ncols = 2,
        vals = 1:4
    )

    s <- terra::rast(
        nrows = 2,
        ncols = 2,
        vals = c(
            4,
            8,
            5,
            9
        )
    )

    out <- mask_scl(
        r,
        s
    )

    v <- terra::values(out)

    expect_equal(
        v[1],
        1
    )

    expect_true(
        is.na(v[2])
    )

    expect_equal(
        v[3],
        3
    )

    expect_true(
        is.na(v[4])
    )

    expect_equal(
        terra::values(out)[1, 1],
        terra::values(r)[1, 1]
    )

})
