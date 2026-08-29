test_that("plot_dnbr returns ggplot", {

    p <- plot_dnbr(burn)

    expect_s3_class(
        p,
        "ggplot"
    )

})


test_that("plot_nbr returns ggplot", {

    p <- plot_nbr(
        burn$pre_nbr
    )

    expect_s3_class(
        p,
        "ggplot"
    )

})


test_that("plot_rgb returns ggplot", {

    p <- plot_rgb(
        burn$pre_composite
    )

    expect_s3_class(
        p,
        "ggplot"
    )

})


test_that("plot_severity returns ggplot", {

    p <- plot_severity(
        burn
    )

    expect_s3_class(
        p,
        "ggplot"
    )

})
