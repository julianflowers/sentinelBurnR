test_that("create_aoi creates a circular AOI", {

    aoi <- create_aoi(

        lon = 1,

        lat = 52,

        radius = 1000

    )

    expect_s3_class(

        aoi,

        "sbr_aoi"

    )

})

test_that("create_aoi creates a bounding box", {

    aoi <- create_aoi(

        xmin = 0,

        xmax = 1,

        ymin = 52,

        ymax = 53

    )

    expect_s3_class(

        aoi,

        "sbr_aoi"

    )

})
