test_that("new_s2_collection creates a collection", {

    downloads <- data.frame(

        scene = "scene1",

        asset = c(
            "red",
            "nir08",
            "swir22"
        ),

        file = c(
            "red.tif",
            "nir08.tif",
            "swir22.tif"
        ),

        stringsAsFactors = FALSE

    )

    collection <- new_s2_collection(
        downloads
    )

    expect_s3_class(
        collection,
        "sbr_collection"
    )

    expect_equal(
        nrow(
            files(collection)
        ),
        3
    )

})
