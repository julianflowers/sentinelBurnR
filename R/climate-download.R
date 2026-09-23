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



# get climate baseline ----------------------------------------------------

#' Get climate data for baseline analysis
#'
#' Downloads and extracts rainfall and temperature data required for
#' climate analysis for the current year and a set of baseline years.
#'
#' @param boundary Spatial boundary.
#' @param date Analysis date.
#' @param baseline_years Years used for the historical baseline.
#' @param windows Rainfall and temperature comparison windows in days.
#' @param dry_spell_window Window used for dry-spell analysis.
#' @param source Climate data source.
#' @param workers Number of parallel download workers.
#'
#' @return A list containing `rainfall` and `temperature`.
#'
#' @export
get_climate_baseline <- function(
        boundary,
        date,
        baseline_years = 1991:2020,
        windows = c(30, 60, 90),
        dry_spell_window = 90,
        source = "era5",
        workers = 4
) {

    boundary <- read_boundary(boundary)
    date <- as.Date(date)

    analysis_year <- as.integer(
        format(date, "%Y")
    )

    years <- unique(
        c(
            baseline_years,
            analysis_year
        )
    )

    max_window <- max(
        c(
            windows,
            dry_spell_window
        )
    )

    year_month <- baseline_year_months(
        date = date,
        years = years,
        window_days = max_window
    )


    # Rainfall ------------------------------------------------------------

    rain_files <- download_climate_months(
        boundary = boundary,
        year_month = year_month,
        source = source,
        variable = "total_precipitation",
        statistic = "daily_sum",
        workers = workers
    )

    rain_climate <- read_climate(
        rain_files
    )

    rainfall <- extract_rainfall(
        rain_climate,
        boundary
    )

    attr(rainfall, "source") <- source
    attr(rainfall, "boundary") <- boundary


    # Temperature ---------------------------------------------------------

    temp_files <- download_climate_months(
        boundary = boundary,
        year_month = year_month,
        source = source,
        variable = "2m_temperature",
        statistic = "daily_mean",
        workers = workers
    )

    temp_climate <- read_climate(
        temp_files
    )

    temperature <- extract_temperature(
        temp_climate,
        boundary
    )

    attr(temperature, "source") <- source
    attr(temperature, "boundary") <- boundary
    attr(temperature, "statistic") <- "daily_mean"


    # Return --------------------------------------------------------------

    list(
        rainfall = rainfall,
        temperature = temperature
    )
}

