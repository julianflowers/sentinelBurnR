#==========================================================
# Download climate data
#==========================================================

download_climate <- function(
        boundary,
        start,
        end,
        source = "era5",
        cache = cache_climate(),
        variable = "total_precipitation",
        statistic = "daily_sum",
        overwrite = FALSE
) {

    boundary <- read_boundary(boundary)

    months <- split_months(
        start,
        end
    )

    files <- character(
        nrow(months)
    )

    for (i in seq_len(nrow(months))) {

        year <- months$year[i]
        month <- months$month[i]

        file <- climate_cache_file(
            source = source,
            year = year,
            month = month,
            variable = variable,
            statistic = statistic,
            cache = cache
        )

        if (!file.exists(file) || overwrite) {

            message(
                sprintf(
                    "Downloading %04d-%02d...",
                    year,
                    month
                )
            )

            download_climate_month(
                boundary = boundary,
                year = year,
                month = month,
                source = source,
                variable = variable,
                statistic = statistic,
                outfile = file
            )
        } else {

            message(
                sprintf(
                    "Using cached %04d-%02d",
                    year,
                    month
                )
            )

        }

        files[i] <- file

    }

    files

}

download_climate_month <- function(
        boundary,
        year,
        month,
        source,
        variable = variable,
        statistic = statistic,
        outfile
) {

    if (source != "era5") {

        stop(
            "Unsupported climate source.",
            call. = FALSE
        )

    }

    download_era5_month(
        boundary,
        year,
        month,
        variable = variable,
        statistic = statistic,
        outfile
    )

}

era5_request <- function(variable = "total_precipitation",
                         daily_statistic = "daily_sum") {

    list(

        dataset_short_name =
            "derived-era5-single-levels-daily-statistics",

        product_type =
            "reanalysis",

        variable =
            variable,

        daily_statistic =
            daily_statistic,

        frequency =
            "1_hourly",

        time_zone =
            "utc+00:00"

    )

}


# download monthly era5 data ----------------------------------------------

download_era5_month <- function(
        boundary,
        year,
        month,
        outfile,
        variable = "total_precipitation",
        statistic = "daily_sum",
        max_tries = 5,
        bbox = NULL
) {

    request <- era5_request(
        variable = variable,
        daily_statistic = statistic
    )

    request$year <- sprintf(
        "%04d",
        year
    )

    request$month <- sprintf(
        "%02d",
        month
    )

    first <- as.Date(
        sprintf(
            "%04d-%02d-01",
            year,
            month
        )
    )

    last <- seq(
        first,
        by = "month",
        length.out = 2
    )[2] - 1

    request$day <- sprintf(
        "%02d",
        seq_len(
            as.integer(
                format(
                    last,
                    "%d"
                )
            )
        )
    )

    if (is.null(bbox)) {
        bbox <- era5_bbox(
            boundary
        )
    }

    request$area <- bbox
    request$target <- basename(outfile)

    message(
        "ERA5 request: ",
        sprintf(
            "%04d-%02d",
            year,
            month
        )
    )

    message(
        "CDS_API_KEY available: ",
        nzchar(
            Sys.getenv(
                "CDS_API_KEY"
            )
        )
    )

    ## Deliberately do NOT catch errors here.
    ## We want Connect to show the original ecmwfr error.

    ecmwfr::wf_request(
        request = request,
        user = "ecmwfr",
        transfer = TRUE,
        path = dirname(outfile),
        verbose = TRUE
    )

    if (!file.exists(outfile)) {
        stop(
            "ERA5 request completed but output file was not created.",
            call. = FALSE
        )
    }

    outfile
}


# climate baseline --------------------------------------------------------

get_climate_baseline <- function(
        boundary,
        date,
        baseline_years = 1991:2020,
        windows = c(30, 60, 90),
        dry_spell_window = 90,
        source = "era5"
) {

    date <- as.Date(date)

    if (length(date) != 1L || is.na(date)) {
        stop(
            "`date` must be a single valid date.",
            call. = FALSE
        )
    }

    if (length(baseline_years) < 2L) {
        stop(
            "At least two baseline years are required.",
            call. = FALSE
        )
    }

    max_window <- max(
        c(windows, dry_spell_window)
    )

    # Baseline years plus the current analysis year
    years <- unique(
        c(baseline_years, lubridate::year(date))
    )

    rainfall <- purrr::map(
        years,
        function(year) {

            end <- as.Date(sprintf(
                "%04d-%s",
                year,
                format(date, "%m-%d")
            ))

            start <- end - (max_window - 1)

            get_rainfall(
                boundary = boundary,
                start = start,
                end = end,
                source = source
            )
        }
    )

    temperature <- purrr::map(
        years,
        function(year) {

            end <- as.Date(sprintf(
                "%04d-%s",
                year,
                format(date, "%m-%d")
            ))

            start <- end - (max_window - 1)

            get_temperature(
                boundary = boundary,
                start = start,
                end = end,
                source = source
            )
        }
    )

    rainfall <- dplyr::bind_rows(rainfall)
    temperature <- dplyr::bind_rows(temperature)

    # Restore classes lost by bind_rows()
    class(rainfall) <- c(
        "sbr_rainfall",
        "data.frame"
    )

    structure(
        list(
            rainfall = rainfall,
            temperature = temperature,
            date = date,
            baseline_years = baseline_years,
            windows = windows,
            dry_spell_window = dry_spell_window
        ),
        class = "sbr_climate_baseline"
    )
}

