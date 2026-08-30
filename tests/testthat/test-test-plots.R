test_that("plot_rgb returns ggplot", {

    comp <- make_test_composite()

    p <- plot_rgb(comp)

    expect_s3_class(
        p,
        "ggplot"
    )

})


test_that("plot_severity returns ggplot", {

    x <- terra::rast(
        nrows = 2,
        ncols = 2,
        xmin = 0,
        xmax = 20,
        ymin = 0,
        ymax = 20,
        crs = "EPSG:27700"
    )

    terra::values(x) <- c(
        0.05, 0.2,
        0.4, 0.7
    )

    names(x) <- "severity"

    p <- plot_severity(x)

    expect_s3_class(
        p,
        "ggplot"
    )

})
