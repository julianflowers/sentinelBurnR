test_that("calc_dnbr subtracts correctly", {

    pre <- terra::rast(
        nrows = 2,
        ncols = 2,
        vals = c(0.8, 0.6, 0.4, 0.2)

    )

    post <- terra::rast(
        nrows = 2,
        ncols = 2,
        vals = c(0.3, 0.5, 0.4, 0.1)
    )

    dnbr <- calc_dnbr(
        pre,
        post
    )


    expect_equal(
        as.numeric(terra::values(dnbr)),
        c(0.5, 0.1, 0, 0.1)
    )

    expect_equal(
        names(dnbr),
        "dnbr"
    )
})
