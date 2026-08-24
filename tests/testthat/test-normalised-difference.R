test_that("normalised difference is calculated correctly", {

    r <- terra::rast(
        nrows = 1,
        ncols = 1
    )

    nir <- r
    red <- r

    terra::values(nir) <- 0.8
    terra::values(red) <- 0.2

    x <- c(nir, red)

    names(x) <- c(
        "nir08",
        "red"
    )

    out <- .normalised_difference(
        x,
        "nir08",
        "red",
        "nd"
    )

    expect_equal(
        terra::values(out)[1],
        0.6
    )

})

test_that("missing band throws an error", {

    r <- terra::rast(
        nrows = 1,
        ncols = 1
    )

    names(r) <- "nir08"

    expect_error(

        .normalised_difference(
            r,
            "nir08",
            "red",
            "nd"
        ),

        "Band"

    )

})

# expect_equal(
#     names(out),
#     "nd"
# )
