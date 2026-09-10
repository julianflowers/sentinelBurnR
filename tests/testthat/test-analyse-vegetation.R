test_that("analyse_vegetation returns vegetation analysis", {

    collection <- make_test_disk_collection()

    veg <- analyse_vegetation(
        collection,
        assets = s2_vegetation_assets
    )

    expect_s3_class(
        veg,
        "sbr_vegetation"
    )

    expect_named(
        veg,
        c(
            "composite",
            "ndvi",
            "ndmi",
            "msi",
            "assets",
            "provenance"
        )
    )

    expect_s4_class(
        veg$composite,
        "SpatRaster"
    )

    expect_s4_class(
        veg$ndvi,
        "SpatRaster"
    )

    expect_s4_class(
        veg$ndmi,
        "SpatRaster"
    )

    expect_s4_class(
        veg$msi,
        "SpatRaster"
    )
})

test_that("analyse_vegetation requires an sbr_collection", {

    expect_error(
        analyse_vegetation(list()),
        "`collection` must be an sbr_collection"
    )
})

test_that(
    "analyse_vegetation calculates expected vegetation indices",
    {

        collection <- make_test_disk_collection()

        veg <- analyse_vegetation(
            collection
        )

        expect_equal(
            terra::global(
                veg$ndvi,
                "mean",
                na.rm = TRUE
            )[[1]],
            5 / 9,
            tolerance = 1e-6
        )

        expect_equal(
            terra::global(
                veg$ndmi,
                "mean",
                na.rm = TRUE
            )[[1]],
            1 / 3,
            tolerance = 1e-6
        )

        expect_equal(
            terra::global(
                veg$msi,
                "mean",
                na.rm = TRUE
            )[[1]],
            0.5,
            tolerance = 1e-6
        )
    }
)
