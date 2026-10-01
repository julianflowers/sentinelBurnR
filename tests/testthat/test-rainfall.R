
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
    expect_equal(attr(x, "units"), "degC")
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

test_that("summarise_rainfall_window summarises rainfall correctly", {

    rainfall <- data.frame(
        date = as.Date("2026-07-01") + 0:29,
        precipitation_mm = rep(2, 30)
    )

    class(rainfall) <- c(
        "sbr_rainfall",
        "data.frame"
    )

    x <- summarise_rainfall_window(
        rainfall,
        date = "2026-07-30",
        window_days = 30
    )

    expect_equal(x$start_date, as.Date("2026-07-01"))
    expect_equal(x$end_date, as.Date("2026-07-30"))
    expect_equal(x$window_days, 30)
    expect_equal(x$rainfall_mm, 60)
    expect_equal(x$mean_daily_mm, 2)
    expect_equal(x$max_daily_mm, 2)
    expect_equal(x$days_observed, 30)
})

test_that("summarise_rainfall_window errors when window is incomplete", {

    rainfall <- data.frame(
        date = as.Date("2026-07-11") + 0:19,
        precipitation_mm = rep(2, 20)
    )

    class(rainfall) <- c(
        "sbr_rainfall",
        "data.frame"
    )

    expect_error(
        summarise_rainfall_window(
            rainfall,
            date = "2026-07-30",
            window_days = 30
        ),
        "incomplete"
    )
})

test_that("compare_rainfall_window compares current rainfall with baseline years", {

    dates <- seq(
        as.Date("2020-01-01"),
        as.Date("2023-12-31"),
        by = "day"
    )

    rainfall <- data.frame(
        date = dates,
        precipitation_mm = 1
    )

    rainfall$precipitation_mm[
        format(rainfall$date, "%Y") == "2023"
    ] <- 0.5

    class(rainfall) <- c(
        "sbr_rainfall",
        "data.frame"
    )

    x <- compare_rainfall_window(
        rainfall,
        date = "2023-07-29",
        baseline_years = 2020:2022,
        window_days = 30
    )

    expect_equal(x$current_mm, 15)
    expect_equal(x$baseline_median_mm, 30)
    expect_equal(x$anomaly_mm, -15)
    expect_equal(x$percent_of_normal, 50)
    expect_equal(x$historical_percentile, 0)
})

test_that("analyse_climate calculates rainfall anomalies", {

    dates <- seq(
        as.Date("2020-01-01"),
        as.Date("2023-12-31"),
        by = "day"
    )

    rainfall <- data.frame(
        date = dates,
        precipitation_mm = 1
    )

    rainfall$precipitation_mm[
        format(rainfall$date, "%Y") == "2023"
    ] <- 0.5

    class(rainfall) <- c(
        "sbr_rainfall",
        "data.frame"
    )

    attr(rainfall, "source") <- "test"

    x <- analyse_climate(
        rainfall = rainfall,
        date = "2023-07-29",
        baseline_years = 2020:2022,
        windows = c(30, 60, 90)
    )

    expect_s3_class(x, "sbr_climate")

    expect_equal(
        x$summary$window_days,
        c(30, 60, 90)
    )

    expect_equal(
        x$summary$current_mm,
        c(15, 30, 45)
    )

    expect_equal(
        x$summary$baseline_median_mm,
        c(30, 60, 90)
    )

    expect_equal(
        x$summary$percent_of_normal,
        rep(50, 3)
    )

    expect_equal(x$source, "test")
})

test_that("summarise_dry_spell identifies dry periods", {

    rainfall <- data.frame(
        date = as.Date("2026-07-01") + 0:29,
        precipitation_mm = 0
    )

    rainfall$precipitation_mm[c(5, 20)] <- 5

    class(rainfall) <- c(
        "sbr_rainfall",
        "data.frame"
    )

    x <- summarise_dry_spell(
        rainfall,
        date = "2026-07-30",
        window_days = 30
    )

    expect_equal(x$max_consecutive_dry_days, 14)
    expect_equal(x$days_since_rain, 10)
    expect_equal(x$dry_days, 28)
    expect_equal(x$dry_spell_start, as.Date("2026-07-06"))
    expect_equal(x$dry_spell_end, as.Date("2026-07-19"))
})

test_that("baseline dry spells summarise historical dry spells", {

    dates <- seq(
        as.Date("2020-05-01"),
        as.Date("2022-07-29"),
        by = "day"
    )

    rainfall <- data.frame(
        date = dates,
        precipitation_mm = 2
    )

    class(rainfall) <- c("sbr_rainfall", "data.frame")
    attr(rainfall, "source") <- "test"

    ## Give each baseline year a known dry spell.
    rainfall$precipitation_mm[
        rainfall$date >= as.Date("2020-06-01") &
            rainfall$date <= as.Date("2020-06-10")
    ] <- 0

    rainfall$precipitation_mm[
        rainfall$date >= as.Date("2021-06-01") &
            rainfall$date <= as.Date("2021-06-20")
    ] <- 0

    result <- baseline_dry_spells(
        rainfall = rainfall,
        date = "2022-07-29",
        baseline_years = 2020:2021,
        window_days = 90,
        threshold_mm = 1
    )

    expect_equal(nrow(result), 2)
    expect_equal(result$year, 2020:2021)
    expect_equal(result$max_dry_days, c(10, 20))
})

test_that("dry spell baseline compares current with historical spells", {

    dates <- seq(
        as.Date("2020-05-01"),
        as.Date("2022-07-29"),
        by = "day"
    )

    rainfall <- data.frame(
        date = dates,
        precipitation_mm = 2
    )

    class(rainfall) <- c("sbr_rainfall", "data.frame")
    attr(rainfall, "source") <- "test"

    rainfall$precipitation_mm[
        rainfall$date >= as.Date("2020-06-01") &
            rainfall$date <= as.Date("2020-06-10")
    ] <- 0

    rainfall$precipitation_mm[
        rainfall$date >= as.Date("2021-06-01") &
            rainfall$date <= as.Date("2021-06-20")
    ] <- 0

    current <- data.frame(
        max_consecutive_dry_days = 15
    )

    result <- summarise_dry_spell_baseline(
        rainfall = rainfall,
        date = "2022-07-29",
        baseline_years = 2020:2021,
        current_dry_spell = current,
        window_days = 90,
        threshold_mm = 1
    )

    expect_equal(result$median_days, 15)
    expect_equal(result$max_days, 20)

    ## One of the two baseline years had a shorter spell than 15 days.
    expect_equal(result$percentile, 50)

    expect_equal(nrow(result$baseline), 2)
})

test_that("summarise_temperature_window summarises temperature", {

    temperature <- tibble::tibble(
        date = seq.Date(
            as.Date("2026-07-01"),
            as.Date("2026-07-30"),
            by = "day"
        ),
        temperature_c = seq(20, 29, length.out = 30)
    )

    result <- summarise_temperature_window(
        temperature,
        date = "2026-07-30",
        window_days = 30
    )

    expect_equal(result$window_days, 30)
    expect_equal(result$mean_max_c, mean(temperature$temperature_c))
    expect_equal(result$maximum_c, 29)
    expect_equal(result$hot_days, sum(temperature$temperature_c >= 25))
    expect_equal(result$very_hot_days, 0)
    expect_true(result$complete)
})

test_that("summarise_temperature_window uses requested end date", {

    temperature <- tibble::tibble(
        date = seq.Date(
            as.Date("2026-07-01"),
            as.Date("2026-07-31"),
            by = "day"
        ),
        temperature_c = seq_len(31)
    )

    result <- summarise_temperature_window(
        temperature,
        date = "2026-07-29",
        window_days = 29
    )

    expect_equal(result$mean_max_c, mean(1:29))
    expect_equal(result$maximum_c, 29)
    expect_true(result$complete)
})

test_that("analyse_climate includes temperature analysis", {

    rainfall <- purrr::map_dfr(
        c(2020:2022, 2026),
        \(year) {
            tibble::tibble(
                date = seq.Date(
                    as.Date(sprintf("%d-05-01", year)),
                    as.Date(sprintf("%d-07-29", year)),
                    by = "day"
                ),
                precipitation_mm = 2
            )
        }
    )

    class(rainfall) <- c(
        "sbr_rainfall",
        class(rainfall)
    )

    attr(rainfall, "source") <- "era5"
    attr(rainfall, "units") <- "mm"

make_temperature <- function(year, offset) {
        tibble::tibble(
            date = as.Date(sprintf("%d-07-01", year)) + 0:28,
            temperature_c =
                20 + offset + seq(0, 4, length.out = 29)
        )
    }

    temperature <- dplyr::bind_rows(
        make_temperature(2020, 0),
        make_temperature(2021, 1),
        make_temperature(2022, 2),
        make_temperature(2026, 3)
    )

    result <- analyse_climate(
        rainfall = rainfall,
        temperature = temperature,
        date = "2026-07-29",
        baseline_years = 2020:2022,
        windows = 29
    )

    expect_s3_class(result, "sbr_climate")
    expect_s3_class(result$temperature, "data.frame")

    expect_equal(
        result$temperature$mean_max_anomaly_c,
        2
    )

    expect_equal(
        result$temperature$mean_max_percentile,
        100
    )
})

test_that("rainfall demo reproduces expected 90-day rainfall", {

    rainfall <- demo_data("rainfall")

    rain_90 <- compare_rainfall_window(
        rainfall = rainfall,
        date = as.Date("2026-07-29"),
        window_days = 90,
        baseline_years = 1991:2020
    )

    expect_equal(
        rain_90$current_mm,
        94.289,
        tolerance = 0.01
    )

        expect_equal(
            rain_90$baseline_median_mm,
            172.753,
            tolerance = 0.01
        )

        expect_equal(
            rain_90$percent_of_normal,
            54.58,
            tolerance = 0.1
        )

})

test_that("analyse_climate respects dry spell arguments", {

    dates <- seq(
        as.Date("2019-01-01"),
        as.Date("2021-12-31"),
        by = "day"
    )

    rainfall <- data.frame(
        date = dates,
        precipitation_mm = rep(0, length(dates))
    )

    class(rainfall) <- c(
        "sbr_rainfall",
        "data.frame"
    )

    result <- analyse_climate(
        rainfall = rainfall,
        date = as.Date("2021-08-25"),
        baseline_years = 2019:2020,
        dry_spell_window = 60,
        dry_threshold_mm = 2
    )

    expect_equal(
        result$dry_spell$window_days,
        60
    )

    expect_equal(
        result$dry_spell$threshold_mm,
        2
    )
})

test_that("prepare_rainfall_history calculates cumulative rainfall", {

    dates <- seq(
        as.Date("2020-05-01"),
        as.Date("2020-05-10"),
        by = "day"
    )

    rainfall <- data.frame(
        date = dates,
        precipitation_mm = rep(2, length(dates))
    )

    result <- prepare_rainfall_history(
        rainfall = rainfall,
        date = as.Date("2020-05-10"),
        baseline_years = integer(),
        period_days = 10,
        statistic = "cumulative"
    )

    expect_equal(
        result$value,
        seq(2, 20, by = 2)
    )
})

test_that("prepare_rainfall_history aligns baseline and current years", {

    rainfall <- data.frame(
        date = c(
            seq(
                as.Date("2019-05-01"),
                as.Date("2019-05-10"),
                by = "day"
            ),
            seq(
                as.Date("2020-05-01"),
                as.Date("2020-05-10"),
                by = "day"
            )
        ),
        precipitation_mm = 1
    )

    result <- prepare_rainfall_history(
        rainfall = rainfall,
        date = as.Date("2020-05-10"),
        baseline_years = 2019,
        period_days = 10
    )

    expect_equal(
        sort(unique(result$year)),
        c(2019, 2020)
    )

    expect_equal(
        unique(result$plot_date[result$year == 2019]),
        unique(result$plot_date[result$year == 2020])
    )

    expect_true(
        all(
            result$period[result$year == 2019] ==
                "baseline"
        )
    )

    expect_true(
        all(
            result$period[result$year == 2020] ==
                "current"
        )
    )
})

test_that("plot_climate_history returns a ggplot", {

    rainfall <- data.frame(
        date = c(
            seq(
                as.Date("2019-05-01"),
                as.Date("2019-05-10"),
                by = "day"
            ),
            seq(
                as.Date("2020-05-01"),
                as.Date("2020-05-10"),
                by = "day"
            )
        ),
        precipitation_mm = 1
    )

    p <- plot_climate_history(
        rainfall = rainfall,
        date = as.Date("2020-05-10"),
        baseline_years = 2019,
        period_days = 10
    )

    expect_s3_class(
        p,
        "ggplot"
    )
})

test_that(
    "calc_humidity gives saturation at the dew point",
    {

        x <- calc_humidity(
            temperature_c = 20,
            dewpoint_c = 20
        )

        expect_equal(
            x$relative_humidity,
            100,
            tolerance = 1e-8
        )

        expect_equal(
            x$vpd_kpa,
            0,
            tolerance = 1e-8
        )
    }
)

test_that(
    "calc_humidity returns sensible humidity values",
    {

        x <- calc_humidity(
            temperature_c = 20,
            dewpoint_c = 10
        )

        expect_gt(
            x$relative_humidity,
            50
        )

        expect_lt(
            x$relative_humidity,
            60
        )

        expect_gt(
            x$vpd_kpa,
            0
        )
    }
)

test_that(
    "compare_humidity_window compares RH and VPD",
    {

        dates <- seq(
            as.Date("2020-07-01"),
            as.Date("2022-07-30"),
            by = "day"
        )

        humidity <- data.frame(
            date = dates,
            relative_humidity = 80,
            vpd_kpa = 0.4
        )

        # Make the current year deliberately
        # drier than the baseline years
        current <- humidity$date >=
            as.Date("2022-07-01") &
            humidity$date <=
            as.Date("2022-07-30")

        humidity$relative_humidity[current] <- 70
        humidity$vpd_kpa[current] <- 0.6

        result <-
            sentinelBurnR:::compare_humidity_window(
                humidity = humidity,
                date = as.Date("2022-07-30"),
                baseline_years = 2020:2021,
                window_days = 30
            )

        expect_equal(
            result$relative_humidity,
            70
        )

        expect_equal(
            result$baseline_relative_humidity,
            80
        )

        expect_equal(
            result$rh_anomaly,
            -10
        )

        expect_equal(
            result$vpd_kpa,
            0.6
        )

        expect_equal(
            result$baseline_vpd_kpa,
            0.4
        )

        expect_equal(
            result$vpd_anomaly_kpa,
            0.2
        )

        expect_equal(
            result$rh_percentile,
            0
        )

        expect_equal(
            result$vpd_percentile,
            100
        )
    }
)

test_that(
    "calc_wind calculates wind speed and direction",
    {

        x <- calc_wind(
            u_ms = c(
                0,
                -5,
                0,
                5
            ),
            v_ms = c(
                -5,
                0,
                5,
                0
            )
        )

        expect_equal(
            x$wind_speed_ms,
            rep(5, 4)
        )

        expect_equal(
            x$wind_direction_deg,
            c(
                0,
                90,
                180,
                270
            )
        )
    }
)

test_that(
    "calc_wind checks component lengths",
    {

        expect_error(
            calc_wind(
                u_ms = 1:3,
                v_ms = 1:2
            ),
            "same length"
        )
    }
)

test_that(
    "extract_wind calculates wind speed",
    {

        u <- terra::rast(
            nrows = 1,
            ncols = 1,
            xmin = 0,
            xmax = 1,
            ymin = 0,
            ymax = 1,
            crs = "EPSG:4326"
        )

        v <- u

        terra::values(u) <- -3
        terra::values(v) <- -4

        names(u) <- "2026-07-01"
        names(v) <- "2026-07-01"

        boundary <- terra::as.polygons(
            terra::ext(u),
            crs = terra::crs(u)
        )

        result <- extract_wind(
            u_wind = u,
            v_wind = v,
            boundary = boundary
        )

        expect_s3_class(
            result,
            "sbr_wind"
        )

        expect_equal(
            result$wind_speed_ms,
            5
        )

        expect_equal(
            result$u_ms,
            -3
        )

        expect_equal(
            result$v_ms,
            -4
        )
    }
)

test_that(
    "era5_hourly_request creates hourly ERA5 request",
    {

        variables <- c(
            "2m_temperature",
            "2m_dewpoint_temperature",
            "10m_u_component_of_wind",
            "10m_v_component_of_wind"
        )

        request <-
            sentinelBurnR:::era5_hourly_request(
                variables
            )

        expect_equal(
            request$dataset_short_name,
            "reanalysis-era5-single-levels"
        )

        expect_equal(
            request$product_type,
            "reanalysis"
        )

        expect_equal(
            request$variable,
            variables
        )

        expect_equal(
            request$data_format,
            "netcdf"
        )

        expect_equal(
            request$download_format,
            "unarchived"
        )
    }
)

test_that(
    "download_era5_hourly uses cached file",
    {

        outfile <- tempfile(
            fileext = ".nc"
        )

        file.create(
            outfile
        )

        result <-
            sentinelBurnR:::download_era5_hourly(
                boundary = NULL,
                date = as.Date("2026-07-29"),
                outfile = outfile
            )

        expect_identical(
            result,
            outfile
        )
    }
)

test_that(
    "extract_fire_weather returns hourly fire weather",
    {

        r <- terra::rast(
            nrows = 1,
            ncols = 1,
            nlyrs = 8,
            xmin = 0,
            xmax = 1,
            ymin = 0,
            ymax = 1,
            crs = "EPSG:4326"
        )

        names(r) <- c(
            "d2m_1", "d2m_2",
            "t2m_1", "t2m_2",
            "u10_1", "u10_2",
            "v10_1", "v10_2"
        )

        terra::values(r) <- c(
            283.15, 284.15,
            293.15, 294.15,
            -3, -3,
            -4, -4
        )

        terra::time(r) <- rep(
            as.POSIXct(
                c(
                    "2026-07-29 00:00:00",
                    "2026-07-29 01:00:00"
                ),
                tz = "UTC"
            ),
            4
        )

        boundary <- terra::as.polygons(
            terra::ext(r),
            crs = terra::crs(r)
        )

        result <- extract_fire_weather(
            climate = r,
            boundary = boundary
        )

        expect_s3_class(
            result,
            "sbr_fire_weather"
        )

        expect_equal(
            nrow(result),
            2
        )

        expect_equal(
            result$temperature_c,
            c(20, 21)
        )

        expect_equal(
            result$dewpoint_c,
            c(10, 11)
        )

        expect_equal(
            result$wind_speed_ms,
            c(5, 5)
        )

        expect_true(
            all(
                result$relative_humidity > 0 &
                    result$relative_humidity <= 100
            )
        )

        expect_true(
            all(result$vpd_kpa > 0)
        )
    }
)

test_that(
    "wind_direction_label converts degrees to compass directions",
    {

        expect_equal(
            wind_direction_label(
                c(
                    0, 45, 90, 135,
                    180, 225, 270, 315
                )
            ),
            c(
                "N", "NE", "E", "SE",
                "S", "SW", "W", "NW"
            )
        )
    }
)

        expect_equal(
            wind_direction_label(
                c(350, 10)
            ),
            c("N", "N")
        )


test_that(
    "plot_fire_weather handles event datetime",
    {

        x <- data.frame(
            datetime = as.POSIXct(
                c(
                    "2026-07-29 09:00:00",
                    "2026-07-29 10:00:00",
                    "2026-07-29 11:00:00"
                ),
                tz = "UTC"
            ),
            temperature_c = c(20, 22, 24),
            relative_humidity = c(70, 65, 60),
            wind_speed_kmh = c(10, 15, 20),
            wind_direction_deg = c(180, 200, 220)
        )

        class(x) <- c(
            "sbr_fire_weather",
            "data.frame"
        )

        attr(x, "event_datetime") <-
            as.POSIXct(
                "2026-07-29 10:00:00",
                tz = "UTC"
            )

        p <- plot_fire_weather(x)

        expect_s3_class(
            p,
            "patchwork"
        )
    }
)
