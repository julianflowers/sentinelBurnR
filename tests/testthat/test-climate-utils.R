test_that("split_months handles one month", {

    x <- split_months(

        "2024-01-15",

        "2024-01-20"

    )

    expect_equal(
        nrow(x),
        1
    )

    expect_equal(
        x$month,
        1
    )

})


test_that("split_months spans months", {

    x <- split_months(

        "2024-01-15",

        "2024-04-10"

    )

    expect_equal(
        x$month,
        c(1,2,3,4)
    )

})

test_that("split_months spans months", {

    x <- split_months(

        "2024-01-15",

        "2024-04-10"

    )

    expect_equal(
        x$month,
        c(1,2,3,4)
    )

})

test_that("split_months checks date order", {

    expect_error(

        split_months(

            "2024-02-01",

            "2024-01-01"

        )

    )

})

test_that("climate_cache_file returns expected filename", {

    cache <- tempdir()

    f <- climate_cache_file(
        source = "era5",
        year = 2024,
        month = 7,
        variable = "total_precipitation",
        statistic = "daily_sum",
        cache = cache
    )

    expect_equal(
        basename(f),
        "2024_07_daily_sum.nc"
    )
})

test_that("climate_cache_file creates directories", {

    root <- file.path(
        tempdir(),
        "climate-test"
    )

    f <- climate_cache_file(
        "era5",
        2024,
        7,
        root
    )

    expect_true(
        dir.exists(
            dirname(f)
        )
    )

})

