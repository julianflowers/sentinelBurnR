#==========================================================
# Download climate data
#==========================================================

download_climate <- function(
        boundary,
        start,
        end,
        source = "era5",
        cache = cache_climate(),
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
        outfile
    )

}

era5_request <- function() {

    list(

        dataset_short_name =
            "derived-era5-single-levels-daily-statistics",

        product_type =
            "reanalysis",

        variable =
            "total_precipitation",

        daily_statistic =
            "daily_sum",

        frequency =
            "1_hourly",

        time_zone =
            "utc+00:00"

    )

}

download_era5_month <- function(
        boundary,
        year,
        month,
        outfile
) {

    request <- era5_request()

    request$year <- sprintf(
        "%04d",
        year
    )

    request$month <- sprintf(
        "%02d",
        month
    )

    ##
    ## All days in month
    ##

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
                format(last, "%d")
            )
        )
    )

    request$area <- era5_bbox(
        boundary
    )

    request$target <- outfile

    message(
        "Submitting ERA5 request for ",
        year,
        "-",
        sprintf("%02d", month)
    )

    print(request$area)

    ecmwfr::wf_request(
        request = request,
        transfer = TRUE,
        path = dirname(outfile)
    )

    outfile

}

cds_download <- function(request) {

    ecmwfr::wf_request(
        request = request
    )

}


