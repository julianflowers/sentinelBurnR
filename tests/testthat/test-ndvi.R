test_that("calc_ndvi returns a raster", {

    comp <- make_test_composite()
    out <- calc_ndvi(comp)

    expect_s4_class(
        out,
        "SpatRaster"
    )

    expect_equal(
        names(out),
        "ndvi"
    )

})

test_that("NDMI matches NBR calculation", {

    comp <- make_test_composite()

    nbr <- calc_nbr(comp)

    ndmi <- calc_ndmi(comp)

    expect_equal(
        names(nbr),
        "nbr"
    )

    expect_equal(
        names(ndmi),
        "ndmi"
    )

    expect_equal(
        as.vector(terra::values(nbr)),
        as.vector(terra::values(ndmi))
    )

})

test_that("NDVI uses different bands", {

    comp <- make_test_composite()

    ndvi <- calc_ndvi(comp)

    nbr <- calc_nbr(comp)

    expect_false(

        isTRUE(
            all.equal(
                terra::values(ndvi),
                terra::values(nbr)
            )
        )

    )

})


