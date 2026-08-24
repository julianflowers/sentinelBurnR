test_that("calc_nbr returns a raster", {

        comp <- make_test_composite()

        out <- calc_nbr(comp)

        expect_s4_class(
            out,
            "SpatRaster"
        )

        expect_equal(
            names(out),
            "nbr"
        )

})



test_that("calc_nbr produces expected values", {

    nir <- terra::rast(
        nrows = 1,
        ncols = 1,
        vals = 0.8
    )

    swir <- terra::rast(
        nrows = 1,
        ncols = 1,
        vals = 0.2
    )

    comp <- c(
        nir,
        swir
    )

    names(comp) <- c(
        "nir08",
        "swir22"
    )

    nbr <- calc_nbr(
        comp
    )

    expect_equal(
        unname(
        terra::values(nbr)[1, 1]
        ),
        0.6,
        tolerance = 1e-6
    )

})
