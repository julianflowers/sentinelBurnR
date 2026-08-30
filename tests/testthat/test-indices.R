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

test_that("calc_ndmi returns a SpatRaster", {

    comp <- make_test_composite()
    ndmi <- calc_ndmi(comp)

    expect_s4_class(
        ndmi,
        "SpatRaster"
    )

    expect_equal(
        names(ndmi),
        "ndmi"
    )

})

test_that("calc_msi returns a SpatRaster", {

    comp <- make_test_composite()
    msi <- calc_msi(comp)

    expect_s4_class(
        msi,
        "SpatRaster"
    )

    expect_equal(
        names(msi),
        "msi"
    )

})

test_that("NDMI uses nir08 and swir16", {

    comp <- make_test_composite()

    ndmi <- calc_ndmi(comp)

    expected <- (
        comp[["nir08"]] - comp[["swir16"]]
    ) / (
        comp[["nir08"]] + comp[["swir16"]]
    )

    expect_equal(
        as.vector(terra::values(ndmi)),
        as.vector(terra::values(expected))
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


