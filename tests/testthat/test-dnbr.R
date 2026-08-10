test_that("calc_dnbr subtracts correctly", {

    pre <- terra::rast(
        nrows = 1,
        ncols = 1,
        vals = 0.7
    )

    post <- terra::rast(
        nrows = 1,
        ncols = 1,
        vals = 0.3
    )

    dnbr <- calc_dnbr(
        pre,
        post
    )

    expect_equal(
        unname(
        terra::values(dnbr)[1, 1]
        ),
        0.4,
        tolerance = 1e-6
    )

})
