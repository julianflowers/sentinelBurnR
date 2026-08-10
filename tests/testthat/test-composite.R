test_that("build_composite returns a raster", {

    skip_if_not(
        exists("pre_collection")
    )

    comp <- build_composite(
        pre_collection
    )

    expect_s4_class(
        comp,
        "SpatRaster"
    )

})
