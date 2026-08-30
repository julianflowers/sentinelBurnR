test_that("analyse_burn stores provenance", {

    burn <- make_test_burn()

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

    burn <- make_test_burn()

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
