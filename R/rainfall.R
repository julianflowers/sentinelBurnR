#' Extract rainfall time series
#'
#' Extract the mean daily precipitation for a boundary from an ERA5
#' precipitation raster.
#'
#' @param climate A SpatRaster returned by \code{read_climate()}.
#' @param boundary Boundary polygon.
#' @param fun Summary function used when extracting rainfall.
#' @return An object of class \code{sbr_rainfall}.
#'
#' @export

extract_rainfall <- function(
        climate,
        boundary,
        fun = mean) {

    stopifnot(
        inherits(
            climate,
            "SpatRaster"
        )
    )

    boundary <- prepare_boundary(
        boundary,
        climate
    )

    vals <- terra::extract(
        climate,
        boundary,
        fun = fun,
        na.rm = TRUE
    )

    vals <- vals[, -1, drop = FALSE]

    out <- data.frame(

        date = as.Date(
            terra::time(climate)
        ),

        precipitation_mm =
            as.numeric(vals[1, ]) * 1000,

        stringsAsFactors = FALSE

    )

    class(out) <- c(
        "sbr_rainfall",
        "data.frame"
    )

    attr(out, "units") <- "mm"

    out

}



get_rainfall <- function(
        boundary,
        start,
        end,
        source = "era5"
        #workers = 1
) {
    boundary <- read_boundary(boundary)

    files <- download_climate(
        boundary = boundary,
        start = start,
        end = end,
        source = source
        #workers = workers
    )

    climate <- read_climate(files)


    out <- extract_rainfall(
        climate,
        boundary
    )

    out <- out[
        out$date >= as.Date(start) &
            out$date <= as.Date(end),
    ]

    attr(out, "source") <- source
    attr(out, "boundary") <- boundary

    out

}


# extract temperature -----------------------------------------------------
extract_temperature <- function(
        climate,
        boundary,
        statistic = c(
            "daily_mean",
            "daily_max",
            "daily_min"
        )
) {

    statistic <- match.arg(statistic)

    x <- extract_climate_values(
        climate,
        boundary
    )

    x$temperature_c <- x$value - 273.15
    x$value <- NULL

    class(x) <- c(
        "sbr_temperature",
        "data.frame"
    )

    attr(x, "source") <- "era5"
    attr(x, "statistic") <- statistic
    attr(x, "units") <- "degC"

    x
}



#     boundary <- prepare_boundary(boundary, climate)
#
#     vals <- terra::extract(
#         climate,
#         boundary,
#         fun = fun,
#         na.rm = TRUE
#     )
#
#     vals <- vals[, -1, drop = FALSE]
#
#     out <- data.frame(
#         date = as.Date(terra::time(climate)),
#         temperature_c =
#             as.numeric(vals[1, ]) - 273.15,
#         stringsAsFactors = FALSE
#     )
#
#     class(out) <- c(
#         "sbr_temperature",
#         "data.frame"
#     )
#
#     attr(out, "units") <- "degrees C"
#
#     out
# }

get_temperature <- function(
        boundary,
        start,
        end,
        statistic = "daily_mean",
        source = "era5"
) {
    boundary <- read_boundary(boundary)

    files <- download_climate(
        boundary = boundary,
        start = start,
        end = end,
        source = source,
        variable = "2m_temperature",
        statistic = statistic
    )

    climate <- read_climate(files)

    out <- extract_temperature(
        climate,
        boundary
    )

    out <- out[
        out$date >= as.Date(start) &
            out$date <= as.Date(end),
        ,
        drop = FALSE
    ]

    attr(out, "source") <- source
    attr(out, "boundary") <- boundary
    attr(out, "statistic") <- statistic

    out
}


#' Read climate data
#'
#' Read a climate raster from a NetCDF file.
#'
#' @param files Path to one or more NetCDF files.
#'
#' @return A SpatRaster.
#'
#' @export
read_climate <- function(files) {

    terra::rast(files)

}

#' @export
print.sbr_rainfall <- function(x, ...) {

    cat("\n")
    cat("sentinelBurnR rainfall\n")
    cat("----------------------\n")
    cat(sprintf("Days        : %d\n", nrow(x)))
    cat(sprintf("Total rain  : %.1f mm\n", sum(x$precipitation_mm, na.rm = TRUE)))
    cat(sprintf("Mean/day    : %.2f mm\n", mean(x$precipitation_mm, na.rm = TRUE)))
    cat(sprintf("Maximum day : %.1f mm\n", max(x$precipitation_mm, na.rm = TRUE)))
    cat("\n")

    print.data.frame(
        utils::head(x),
        row.names = FALSE
    )

    invisible(x)
}

relative_humidity <- function(
        temperature_c,
        dewpoint_c
) {
    100 *
        exp(
            (17.625 * dewpoint_c) /
                (243.04 + dewpoint_c) -
                (17.625 * temperature_c) /
                (243.04 + temperature_c)
        )
}

relative_humidity <- function(
        temperature_c,
        dewpoint_c
) {
    rh <- 100 *
        exp(
            (17.625 * dewpoint_c) /
                (243.04 + dewpoint_c) -
                (17.625 * temperature_c) /
                (243.04 + temperature_c)
        )

    pmin(
        100,
        pmax(0, rh)
    )
}

extract_temperature <- function(
        climate,
        boundary,
        statistic = c(
            "daily_mean",
            "daily_max",
            "daily_min"
        )
) {

    statistic <- match.arg(statistic)

    x <- extract_climate_values(
        climate,
        boundary
    )

    x$temperature_c <- x$value - 273.15
    x$value <- NULL

    class(x) <- c(
        "sbr_temperature",
        "data.frame"
    )

    attr(x, "source") <- "era5"
    attr(x, "statistic") <- statistic
    attr(x, "units") <- "degC"

    x
}

get_humidity <- function(
        boundary,
        start,
        end,
        source = "era5"
) {
    boundary <- read_boundary(boundary)

    temp_files <- download_climate(
        boundary = boundary,
        start = start,
        end = end,
        source = source,
        variable = "2m_temperature",
        statistic = "daily_mean"
    )

    dew_files <- download_climate(
        boundary = boundary,
        start = start,
        end = end,
        source = source,
        variable = "2m_dewpoint_temperature",
        statistic = "daily_mean"
    )

    temp <- extract_temperature(
        read_climate(temp_files),
        boundary,
        name = "temperature_c"
    )

    dew <- extract_temperature(
        read_climate(dew_files),
        boundary,
        name = "dewpoint_c"
    )

    out <- merge(
        temp,
        dew,
        by = "date",
        all = FALSE
    )

    out$relative_humidity <- relative_humidity(
        out$temperature_c,
        out$dewpoint_c
    )

    out$vpd_kpa <- vapour_pressure_deficit(
        out$temperature_c,
        out$dewpoint_c
    )

    out <- out[
        out$date >= as.Date(start) &
            out$date <= as.Date(end),
        ,
        drop = FALSE
    ]

    class(out) <- c(
        "sbr_humidity",
        "data.frame"
    )

    attr(out, "source") <- source
    attr(out, "humidity_method") <-
        "derived from daily-mean 2 m temperature and dewpoint"

    out
}

vapour_pressure_deficit <- function(
        temperature_c,
        dewpoint_c
) {
    saturation_vapour_pressure <- function(x) {
        0.6108 * exp(
            (17.27 * x) /
                (x + 237.3)
        )
    }

    vpd <-
        saturation_vapour_pressure(temperature_c) -
        saturation_vapour_pressure(dewpoint_c)

    pmax(0, vpd)
}


# climate values ----------------------------------------------------------

extract_climate_values <- function(climate, boundary) {

    if (!inherits(climate, "SpatRaster")) {
        stop(
            "`climate` must be a SpatRaster.",
            call. = FALSE
        )
    }

    boundary <- prepare_boundary(
        boundary,
        climate
    )

    values <- terra::extract(
        climate,
        boundary,
        fun = mean,
        na.rm = TRUE
    )

    dates <- as.Date(terra::time(climate))

    if (length(dates) != terra::nlyr(climate)) {
        stop(
            "Climate raster must have one date per layer.",
            call. = FALSE
        )
    }

    data.frame(
        date = dates,
        value = as.numeric(values[1, -1])
    )
}

summarise_temperature_window <- function(
        temperature,
        date,
        window_days
) {

    date <- as.Date(date)
    start <- date - window_days + 1

    x <- temperature[
        temperature$date >= start &
            temperature$date <= date,
        ,
        drop = FALSE
    ]

    data.frame(
        window_days = window_days,
        mean_max_c = mean(
            x$temperature_c,
            na.rm = TRUE
        ),
        max_c = max(
            x$temperature_c,
            na.rm = TRUE
        ),
        days_ge_25 = sum(
            x$temperature_c >= 25,
            na.rm = TRUE
        ),
        days_ge_30 = sum(
            x$temperature_c >= 30,
            na.rm = TRUE
        )
    )
}


# summarise temperature window --------------------------------------------


summarise_temperature_window <- function(
        temperature,
        date,
        window_days,
        hot_threshold = 25,
        very_hot_threshold = 30
) {

    date <- as.Date(date)

    start <- date - window_days + 1L

    x <- temperature |>
        dplyr::filter(
            .data$date >= .env$start,
            .data$date <= .env$date
        )

    expected_dates <- seq.Date(
        start,
        date,
        by = "day"
    )

    complete <- (
        nrow(x) == length(expected_dates) &&
            dplyr::n_distinct(x$date) == nrow(x) &&
            setequal(x$date, expected_dates)
    )

    if (!complete) {
        warning(
            sprintf(
                "Temperature window ending %s is incomplete.",
                date
            ),
            call. = FALSE
        )
    }

    tibble::tibble(
        window_days = window_days,
        mean_max_c = mean(
            x$temperature_c,
            na.rm = TRUE
        ),
        maximum_c = max(
            x$temperature_c,
            na.rm = TRUE
        ),
        hot_days = sum(
            x$temperature_c >= hot_threshold,
            na.rm = TRUE
        ),
        very_hot_days = sum(
            x$temperature_c >= very_hot_threshold,
            na.rm = TRUE
        ),
        complete = complete
    )
}

