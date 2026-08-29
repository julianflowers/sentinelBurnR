test_that("collection contains provenance", {

    expect_true(
        "provenance" %in%
            names(pre_collection)
    )

})

test_that("provenance summary contains expected fields", {

    expect_named(

        pre_collection$provenance$summary,

        c(
            "start",
            "end",
            "n_acquisitions",
            "satellites",
            "mean_cloud",
            "max_cloud"
        )

    )

})

test_that("acquisition count matches summary", {


    expect_equal(

        pre_collection$provenance$summary$n_acquisitions,

        nrow(pre_collection$provenance$acquisitions)

    )

})


test_that("scene table contains cloud metadata", {


    expect_true(

        all(

            c(

                "cloud_cover",
                "cloud_shadow",
                "medium_cloud",
                "high_cloud",
                "vegetation",
                "water"

            ) %in%

                names(pre_collection$provenance$scenes)

        )

    )

})


test_that("cloud cover statistics are sensible", {

    # pre_collection <- download_s2(
    #     search_s2(
    #         aoi,
    #         start = "...",
    #         end = "..."
    #     ),
    #     limit = 1
    # )

    prov <- pre_collection$provenance

    expect_true(

        is.na(prov$summary$mean_cloud) ||

            (
                prov$summary$mean_cloud >= 0 &&
                    prov$summary$mean_cloud <= 100
            )

    )

    expect_true(

        is.na(prov$summary$max_cloud) ||

            (
                prov$summary$max_cloud >= 0 &&
                    prov$summary$max_cloud <= 100
            )

    )

})

test_that("burn caption returns a single string", {

    caption <- burn_caption(
        burn
    )

    expect_type(
        caption,
        "character"
    )

    expect_length(
        caption,
        1
    )

})

test_that("plot_dnbr returns ggplot", {

    expect_s3_class(
        plot_dnbr(burn$dnbr),
        "ggplot"
    )

})

test_that("plot_rgb returns ggplot", {

    expect_s3_class(
        plot_rgb(burn$pre_composite),
        "ggplot"
    )

})

expect_s3_class(
    rain,
    "sbr_rainfall"
)

expect_true(
    all(
        c("date","precipitation_mm") %in%
            names(rain)
    )
)
