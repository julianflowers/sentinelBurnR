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
        max_tries = 5
) {
    request <- era5_request(
        variable = variable,
        daily_statistic = statistic
    )

    request$year <- sprintf("%04d", year)
    request$month <- sprintf("%02d", month)

    first <- as.Date(
        sprintf("%04d-%02d-01", year, month)
    )

    last <- seq(
        first,
        by = "month",
        length.out = 2
    )[2] - 1

    request$day <- sprintf(
        "%02d",
        seq_len(as.integer(format(last, "%d")))
    )

    request$area <- era5_bbox(boundary)
    request$target <- basename(outfile)

    for (attempt in seq_len(max_tries)) {

        message(
            sprintf(
                "ERA5 %04d-%02d, attempt %d/%d",
                year,
                month,
                attempt,
                max_tries
            )
        )

        result <- tryCatch(
            {
                ecmwfr::wf_request(
                    request = request,
                    transfer = TRUE,
                    path = dirname(outfile)
                )
                TRUE
            },
            error = function(e) {
                message(
                    "ERA5 request failed: ",
                    conditionMessage(e)
                )
                FALSE
            }
        )

        if (result && file.exists(outfile)) {
            return(outfile)
        }

        if (attempt < max_tries) {
            wait <- 10 * 2^(attempt - 1)

            message(
                "Retrying in ",
                wait,
                " seconds..."
            )

            Sys.sleep(wait)
        }
    }

    stop(
        sprintf(
            "ERA5 download failed after %d attempts: %04d-%02d",
            max_tries,
            year,
            month
        ),
        call. = FALSE
    )
}


