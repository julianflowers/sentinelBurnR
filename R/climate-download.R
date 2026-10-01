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


# dewpoint ----------------------------------------------------------------

    dewpoint_files <- download_climate_months(
        boundary = boundary,
        year_month = year_month,
        source = source,
        variable = "2m_dewpoint_temperature",
        statistic = "daily_mean",
        workers = workers
    )

    dewpoint_climate <- read_climate(
        dewpoint_files
    )

    humidity <- extract_humidity(
        temperature = temp_climate,
        dewpoint = dewpoint_climate,
        boundary = boundary,
        statistic = "daily_mean"
    )

    attr(humidity, "source") <- source
    attr(humidity, "boundary") <- boundary

    # Return --------------------------------------------------------------

    list(
        rainfall = rainfall,
        temperature = temperature,
        humidity = humidity
    )
}


# era5 hourly data --------------------------------------------------------


era5_hourly_request <- function(
        variables
) {

    list(
        dataset_short_name = "reanalysis-era5-single-levels",
        product_type = "reanalysis",
        variable = variables,
        data_format = "netcdf",
        download_format = "unarchived"
    )
}

download_era5_hourly <- function(
        boundary,
        date,
        outfile,
        variables = c(
            "2m_temperature",
            "2m_dewpoint_temperature",
            "10m_u_component_of_wind",
            "10m_v_component_of_wind"
        ),
        bbox = NULL
) {

    date <- as.Date(date)

    if (file.exists(outfile)) {
        return(outfile)
    }

    request <- era5_hourly_request(
        variables = variables
    )

    request$year <- format(
        date,
        "%Y"
    )

    request$month <- format(
        date,
        "%m"
    )

    request$day <- format(
        date,
        "%d"
    )

    request$time <- sprintf(
        "%02d:00",
        0:23
    )

    if (is.null(bbox)) {
        bbox <- era5_bbox(
            boundary
        )
    }

    request$area <- bbox
    request$target <- basename(
        outfile
    )

    dir.create(
        dirname(outfile),
        recursive = TRUE,
        showWarnings = FALSE
    )

    ecmwfr::wf_request(
        request = request,
        transfer = TRUE,
        path = dirname(outfile),
        verbose = TRUE
    )

    if (!file.exists(outfile)) {
        stop(
            "ERA5 hourly download failed: ",
            outfile,
            call. = FALSE
        )
    }

    outfile
}


# fire weather ------------------------------------------------------------


extract_fire_weather <- function(
        climate,
        boundary
) {

    if (!inherits(climate, "SpatRaster")) {
        stop(
            "`climate` must be a SpatRaster.",
            call. = FALSE
        )
    }

    vars <- c(
        "d2m",
        "t2m",
        "u10",
        "v10"
    )

    missing <- vars[
        !vapply(
            vars,
            \(v) any(
                startsWith(
                    names(climate),
                    paste0(v, "_")
                )
            ),
            logical(1)
        )
    ]

    if (length(missing) > 0) {
        stop(
            "Missing fire-weather variables: ",
            paste(
                missing,
                collapse = ", "
            ),
            call. = FALSE
        )
    }

    extract_variable <- function(
        prefix,
        name
    ) {

        i <- which(
            startsWith(
                names(climate),
                paste0(prefix, "_")
            )
        )

        r <- climate[[i]]

        values <- extract_hourly_climate_values(
            r,
            boundary
        )

        names(values)[
            names(values) == "value"
        ] <- name

        values
    }

    dew <- extract_variable(
        "d2m",
        "dewpoint_c"
    )

    temp <- extract_variable(
        "t2m",
        "temperature_c"
    )

    u <- extract_variable(
        "u10",
        "u_ms"
    )

    v <- extract_variable(
        "v10",
        "v_ms"
    )

    # ERA5 temperatures are Kelvin
    dew$dewpoint_c <-
        dew$dewpoint_c - 273.15

    temp$temperature_c <-
        temp$temperature_c - 273.15

    x <- Reduce(
        \(x, y) merge(
            x,
            y,
            by = "datetime",
            all = FALSE
        ),
        list(
            temp,
            dew,
            u,
            v
        )
    )

    humidity <- calc_humidity(
        temperature_c = x$temperature_c,
        dewpoint_c = x$dewpoint_c
    )

    wind <- calc_wind(
        u_ms = x$u_ms,
        v_ms = x$v_ms
    )

    x$relative_humidity <-
        humidity$relative_humidity

    x$vpd_kpa <-
        humidity$vpd_kpa

    x$wind_speed_ms <-
        wind$wind_speed_ms

    x$wind_speed_kmh <-
        x$wind_speed_ms * 3.6

    x$wind_direction_deg <-
        wind$wind_direction_deg

    x$wind_direction <-
        wind_direction_label(
            x$wind_direction_deg
        )

    class(x) <- c(
        "sbr_fire_weather",
        "data.frame"
    )

    attr(x, "source") <- "era5"

    x
}

extract_hourly_climate_values <- function(
        climate,
        boundary
) {

    boundary <- read_boundary(
        boundary
    )

    boundary <- terra::project(
        boundary,
        terra::crs(climate)
    )

    values <- terra::extract(
        climate,
        boundary,
        fun = mean,
        na.rm = TRUE
    )

    values <- values[
        1,
        -1,
        drop = TRUE
    ]

    datetime <- terra::time(
        climate
    )

    data.frame(
        datetime = as.POSIXct(
            datetime,
            tz = "UTC"
        ),
        value = as.numeric(
            values
        )
    )
}

#' Get hourly fire-weather conditions
#'
#' Downloads ERA5 hourly temperature, dewpoint temperature,
#' and 10 m wind components for a specified time window and
#' derives relative humidity, VPD, wind speed, and wind direction.
#'
#' @param boundary Spatial boundary.
#' @param start Start of the fire-weather window.
#' @param end End of the fire-weather window.
#' @param event_datetime Optional event time, such as the reported
#'   fire time.
#' @param tz Time zone used for `start`, `end`, and `event_datetime`.
#' @param cache Directory used for cached ERA5 downloads.
#'
#' @return An object of class `sbr_fire_weather`.
#'
#' @export
get_fire_weather <- function(
        boundary,
        start,
        end,
        event_datetime = NULL,
        tz = "UTC",
        cache = cache_climate()
) {

    # Interpret supplied times in the requested local timezone

    start_local <- as.POSIXct(
        start,
        tz = tz
    )

    end_local <- as.POSIXct(
        end,
        tz = tz
    )

    if (is.na(start_local) || is.na(end_local)) {
        stop(
            "`start` and `end` must be valid date-times.",
            call. = FALSE
        )
    }

    if (end_local <= start_local) {
        stop(
            "`end` must be later than `start`.",
            call. = FALSE
        )
    }

    if (!is.null(event_datetime)) {

        event_local <- as.POSIXct(
            event_datetime,
            tz = tz
        )

        if (is.na(event_local)) {
            stop(
                "`event_datetime` must be a valid date-time.",
                call. = FALSE
            )
        }

    } else {

        event_local <- NULL
    }

    # ERA5 timestamps are UTC. Changing tzone changes how the
    # same instant is represented; it does not change the instant.

    start_utc <- start_local
    end_utc <- end_local

    attr(start_utc, "tzone") <- "UTC"
    attr(end_utc, "tzone") <- "UTC"

    # Determine which UTC calendar days are required

    dates <- seq(
        as.Date(
            start_utc,
            tz = "UTC"
        ),
        as.Date(
            end_utc,
            tz = "UTC"
        ),
        by = "day"
    )

    # Calculate ERA5 bounding box once

    bbox <- era5_bbox(
        boundary
    )

    # Download/extract each required day

    weather <- lapply(
        dates,
        function(date) {

            outfile <- file.path(
                cache,
                paste0(
                    "era5_fire_weather_",
                    format(
                        date,
                        "%Y%m%d"
                    ),
                    ".nc"
                )
            )

            file <- download_era5_hourly(
                boundary = boundary,
                date = date,
                outfile = outfile,
                bbox = bbox
            )

            climate <- terra::rast(
                file
            )

            extract_fire_weather(
                climate = climate,
                boundary = boundary
            )
        }
    )

    weather <- do.call(
        rbind,
        weather
    )

    # Restrict ERA5 observations to requested interval

    weather <- weather[
        weather$datetime >= start_utc &
            weather$datetime <= end_utc,
        ,
        drop = FALSE
    ]

    rownames(weather) <- NULL

    # Display returned timestamps in requested timezone

    attr(
        weather$datetime,
        "tzone"
    ) <- tz

    # Store user-facing metadata in local time

    attr(weather, "start") <-
        start_local

    attr(weather, "end") <-
        end_local

    attr(weather, "event_datetime") <-
        event_local

    attr(weather, "timezone") <-
        tz

    attr(weather, "boundary") <-
        boundary

    class(weather) <- c(
        "sbr_fire_weather",
        "data.frame"
    )

    weather
}
