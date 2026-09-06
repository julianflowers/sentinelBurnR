download_climate_months <- function(
        boundary,
        years,
        months,
        source = "era5",
        cache = cache_climate(),
        variable = "total_precipitation",
        statistic = "daily_sum",
        overwrite = FALSE,
        workers = 1
) {

    if (source != "era5") {
        stop(
            "Unsupported climate source.",
            call. = FALSE
        )
    }

    years <- as.integer(years)
    months <- as.integer(months)

    if (
        !length(years) ||
        !length(months) ||
        anyNA(years) ||
        anyNA(months)
    ) {
        stop(
            "`years` and `months` must contain valid values.",
            call. = FALSE
        )
    }

    if (any(months < 1L | months > 12L)) {
        stop(
            "`months` must be between 1 and 12.",
            call. = FALSE
        )
    }

    ## Reduce the spatial object to ordinary numeric values here.
    ## No spatial object needs to be passed to a worker later.
    bbox <- era5_bbox(
        read_boundary(boundary)
    )

    requests <- expand.grid(
        year = years,
        month = months,
        KEEP.OUT.ATTRS = FALSE,
        stringsAsFactors = FALSE
    )

    requests <- requests[
        order(requests$year, requests$month),
        ,
        drop = FALSE
    ]

    files <- character(nrow(requests))

    for (i in seq_len(nrow(requests))) {

        year <- requests$year[i]
        month <- requests$month[i]

        file <- climate_cache_file(
            source = source,
            year = year,
            month = month,
            variable = variable,
            statistic = statistic,
            cache = cache
        )

        if (file.exists(file) && !overwrite) {

            message(
                sprintf(
                    "Using cached %04d-%02d",
                    year,
                    month
                )
            )

        } else {

            message(
                sprintf(
                    "Downloading %04d-%02d...",
                    year,
                    month
                )
            )

            download_era5_month(
                bbox = bbox,
                year = year,
                month = month,
                variable = variable,
                statistic = statistic,
                outfile = file
            )
        }

        files[i] <- file
    }

    files
}
