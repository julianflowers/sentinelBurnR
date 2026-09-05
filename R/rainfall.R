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
) {
    boundary <- read_boundary(boundary)

    files <- download_climate(
        boundary = boundary,
        start = start,
        end = end,
        source = source
    )
    boundary <- read_boundary(boundary)

    files <- download_climate(
        boundary = boundary,
        start = start,
        end = end,
        source = source
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

extract_temperature <- function(
        climate,
        boundary,
        fun = mean
) {
    stopifnot(inherits(climate, "SpatRaster"))

    boundary <- prepare_boundary(boundary, climate)

    vals <- terra::extract(
        climate,
        boundary,
        fun = fun,
        na.rm = TRUE
    )

    vals <- vals[, -1, drop = FALSE]

    out <- data.frame(
        date = as.Date(terra::time(climate)),
        temperature_c =
            as.numeric(vals[1, ]) - 273.15,
        stringsAsFactors = FALSE
    )

    class(out) <- c(
        "sbr_temperature",
        "data.frame"
    )

    attr(out, "units") <- "degrees C"

    out
}

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
        name = "temperature_c",
        fun = mean
) {
    stopifnot(inherits(climate, "SpatRaster"))

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
        date = as.Date(terra::time(climate)),
        value = as.numeric(vals[1, ]) - 273.15,
        stringsAsFactors = FALSE
    )

    names(out)[2] <- name

    class(out) <- c(
        "sbr_temperature",
        "data.frame"
    )

    attr(out, "units") <- "degrees C"

    out
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


