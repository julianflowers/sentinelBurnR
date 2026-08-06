make_test_stac_items <- function() {

    list(
        type = "FeatureCollection",

        features = list(

            list(
                id = "S2_TEST_002",

                properties = list(
                    datetime = "2026-05-11T10:30:21.000Z",
                    `eo:cloud_cover` = 18.5,
                    `mgrs:tile` = "30UXC",
                    platform = "sentinel-2b"
                )
            ),

            list(
                id = "S2_TEST_001",

                properties = list(
                    datetime = "2026-05-01T10:40:11.000Z",
                    `eo:cloud_cover` = 4.2,
                    `mgrs:tile` = "30UXC",
                    platform = "sentinel-2a"
                )
            )
        )
    )
}


test_that("s2_item_metadata extracts scene metadata", {

    result <- s2_item_metadata(
        make_test_stac_items()
    )

    expect_s3_class(
        result,
        "data.frame"
    )

    expect_equal(
        nrow(result),
        2
    )

    expect_named(
        result,
        c(
            "id",
            "datetime",
            "date",
            "cloud_cover",
            "tile",
            "platform"
        )
    )

    expect_equal(
        result$id,
        c(
            "S2_TEST_001",
            "S2_TEST_002"
        )
    )

    expect_equal(
        result$cloud_cover,
        c(4.2, 18.5)
    )
})


test_that("s2_item_metadata handles no scenes", {

    result <- s2_item_metadata(
        list(
            type = "FeatureCollection",
            features = list()
        )
    )

    expect_s3_class(
        result,
        "data.frame"
    )

    expect_equal(
        nrow(result),
        0
    )
})


test_that("summary returns search metadata", {

    geom <- terra::vect(
        terra::ext(0, 1, 0, 1),
        crs = "EPSG:4326"
    )

    aoi <- new_aoi(geom)

    search <- new_s2_search(
        items = make_test_stac_items(),
        aoi = aoi,
        start = "2026-05-01",
        end = "2026-05-15"
    )

    result <- summary(search)

    expect_s3_class(
        result,
        "data.frame"
    )

    expect_equal(
        nrow(result),
        2
    )
})
