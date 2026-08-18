test_that("classify_dnbr assigns expected classes", {

    r <- terra::rast(
        nrows = 1,
        ncols = 7,
        vals = c(
            -0.30,
            -0.20,
            0.00,
            0.20,
            0.35,
            0.55,
            0.80
        )
    )

    sev <- classify_dnbr(r)

    expect_equal(
        as.vector(
            terra::values(sev)
        ),
        1:7
    )

})

test_that("classify_dnbr accepts custom thresholds", {

    r <- terra::rast(
        nrows = 1,
        ncols = 3,
        vals = c(
            0,
            5,
            10
        )
    )

    sev <- classify_dnbr(

        r,

        thresholds = c(
            1,
            2,
            3,
            4,
            6,
            8
        )

    )

    expect_equal(

        as.vector(
            terra::values(sev)
        ),

        c(
            1,
            5,
            7
        )

    )

})

test_that("classify_dnbr rejects invalid thresholds", {

    r <- terra::rast(
        nrows = 1,
        ncols = 1,
        vals = 0
    )

    expect_error(

        classify_dnbr(
            r,
            thresholds = 1:5
        )

    )

    expect_error(

        classify_dnbr(
            r,
            thresholds = c(
                1,
                2,
                4,
                3,
                5,
                6
            )
        )

    )

})

