test_that("analyse_burn stores provenance", {

    burn <- analyse_burn(
        pre_collection,
        post_collection
    )

    expect_true(

        "provenance" %in%

            names(burn)

    )

    expect_named(

        burn$provenance,

        c(
            "pre",
            "post",
            "processing"
        )

    )

})


test_that("burn caption returns a single character string", {

    burn <- analyse_burn(
        pre_collection,
        post_collection
    )

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

    expect_match(
        caption,
        "Pre-fire"
    )

    expect_match(
        caption,
        "Post-fire"
    )

})
