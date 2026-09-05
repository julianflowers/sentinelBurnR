
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

    rain <- get_rainfall(

        boundary = make_test_boundary(),

        start = "2024-01-01",

        end = "2024-01-31"

    )

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

test_that("extract_temperature converts Kelvin to Celsius", {

    r <- terra::rast(
        nrows = 1,
        ncols = 1,
        xmin = 0,
        xmax = 1,
        ymin = 0,
        ymax = 1,
        crs = "EPSG:4326"
    )

    terra::values(r) <- 293.15
    terra::time(r) <- as.Date("2026-07-01")

    boundary <- terra::as.polygons(
        terra::ext(r),
        crs = terra::crs(r)
    )

    x <- extract_temperature(r, boundary)

    expect_equal(x$temperature_c, 20)
    expect_equal(x$date, as.Date("2026-07-01"))
    expect_s3_class(x, "sbr_temperature")
    expect_equal(attr(x, "units"), "degrees C")
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

    expect_true(
        grepl(
            "era5/total_precipitation/2024",
            f,
            fixed = TRUE
        )
    )

        f_temp <- climate_cache_file(
            source = "era5",
            year = 2024,
            month = 7,
            variable = "2m_temperature",
            statistic = "daily_mean",
            cache = cache
        )

        expect_equal(
            basename(f_temp),
            "2024_07_daily_mean.nc"
        )

        expect_true(
            grepl(
                "era5/2m_temperature/2024",
                f_temp,
                fixed = TRUE
            )


    )
})

test_that("relative_humidity behaves correctly", {

    expect_equal(
        relative_humidity(20, 20),
        100
    )

    expect_lt(
        relative_humidity(20, 10),
        100
    )

    expect_gt(
        relative_humidity(20, 10),
        0
    )
})

test_that("relative_humidity is vectorised and bounded", {

    rh <- relative_humidity(
        temperature_c = c(20, 25, 30),
        dewpoint_c = c(10, 15, 20)
    )

    expect_length(rh, 3)
    expect_true(all(rh >= 0))
    expect_true(all(rh <= 100))
})

test_that("vapour_pressure_deficit behaves correctly", {

    expect_equal(
        vapour_pressure_deficit(20, 20),
        0,
        tolerance = 1e-10
    )

    expect_gt(
        vapour_pressure_deficit(20, 10),
        0
    )

    expect_gt(
        vapour_pressure_deficit(30, 10),
        vapour_pressure_deficit(20, 10)
    )
})

test_that("vapour_pressure_deficit is vectorised and non-negative", {

    vpd <- vapour_pressure_deficit(
        temperature_c = c(20, 25, 30),
        dewpoint_c = c(10, 15, 20)
    )

    expect_length(vpd, 3)
    expect_true(all(vpd >= 0))
})


