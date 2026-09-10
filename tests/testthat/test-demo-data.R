test_that("drought demo data load correctly", {

    historical <- demo_data("drought_historical")
    current <- demo_data("drought_current")

    expect_length(historical, 8)
    expect_named(
        historical,
        as.character(2018:2025)
    )

    expect_length(current, 1)
    expect_named(current, "2026-07-29")

    expect_true(all(
        purrr::map_lgl(
            historical,
            \(x) inherits(x, "SpatRaster")
        )
    ))

    expect_s4_class(
        current[[1]],
        "SpatRaster"
    )
})

test_that("drought demo reproduces expected anomaly", {

    historical <- demo_data("drought_historical")
    current <- demo_data("drought_current")

    drought <- analyse_drought_rasters(
        annual = historical,
        current = current[[1]],
        current_date = "2026-07-29"
    )

    median_anomaly <- terra::global(
        drought$anomaly,
        fun = \(x, ...) c(median = median(x, ..., na.rm = TRUE))
    )[1,1]



    expect_equal(
        median_anomaly,
        -0.05038479,
        tolerance = 1e-6
    )

    expect_s3_class(
        drought,
        "sbr_drought"
    )
})

test_that("true-colour demo imagery loads", {

    pre <- demo_data("visual_pre")
    post <- demo_data("visual_post")

    expect_s4_class(pre, "SpatRaster")
    expect_s4_class(post, "SpatRaster")

    expect_equal(terra::nlyr(pre), 3)
    expect_equal(terra::nlyr(post), 3)
})
