make_test_boundary <- function() {

    terra::vect(

        matrix(

            c(
                0, 0,
                100, 0,
                100, 100,
                0, 100,
                0, 0
            ),

            ncol = 2,
            byrow = TRUE

        ),

        type = "polygons",
        crs = "EPSG:3857"

    )

}



test_that("dates are returned", {

    b <- make_test_boundary()

    x <- get_rainfall(

        boundary = b,

        start = "2024-01-01",

        end = "2024-01-05"

    )

    expect_equal(
        nrow(x),
        5
    )

})


test_that("start must precede end", {

    expect_error(

        get_rainfall(

            boundary = make_test_boundary(),

            start = "2024-02-01",

            end = "2024-01-01"

        )

    )

})

test_that("returns data frame", {

    x <- get_rainfall(

        boundary = make_test_boundary(),

        start = "2024-01-01",

        end = "2024-01-02"

    )

    expect_true(
        is.data.frame(x)
    )

})

test_that("rainfall object has expected structure", {

    expect_s3_class(
        rain,
        "sbr_rainfall"
    )

    expect_true(
        all(c("date", "precipitation_mm") %in% names(rain))
    )

    expect_true(
        inherits(rain$date, "Date")
    )

})

