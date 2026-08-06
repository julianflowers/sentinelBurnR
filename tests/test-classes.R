test_that("new_s2_search returns an sbr_search object", {

    geom <- terra::vect(
        terra::ext(0, 1, 0, 1),
        crs = "EPSG:4326"
    )

    aoi <- new_aoi(geom)

    items <- list(
        type = "FeatureCollection",
        features = list()
    )

    result <- new_s2_search(
        items = items,
        aoi = aoi,
        start = "2026-05-01",
        end = "2026-06-15"
    )

    expect_s3_class(result, "sbr_search")
    expect_equal(result$start, as.Date("2026-05-01"))
    expect_equal(result$end, as.Date("2026-06-15"))
})
