#==========================================================
# Split a date range into year/month combinations
#==========================================================

split_months <- function(
        start,
        end
) {

    start <- as.Date(start)
    end <- as.Date(end)

    if (start > end) {

        stop(
            "`start` must be before `end`.",
            call. = FALSE
        )

    }

    months <- seq(

        as.Date(format(start, "%Y-%m-01")),

        as.Date(format(end, "%Y-%m-01")),

        by = "month"

    )

    data.frame(

        year = as.integer(
            format(months, "%Y")
        ),

        month = as.integer(
            format(months, "%m")
        ),

        stringsAsFactors = FALSE

    )

}


#==========================================================
# Climate cache filename
#==========================================================

climate_cache_file <- function(
        source,
        year,
        month,
        variable = "total_precipitation",
        statistic = "daily_sum",
        cache = cache_climate()
) {

    stopifnot(
        length(source) == 1,
        length(year) == 1,
        length(month) == 1
    )

    dir <- file.path(
        cache,
        source,
        variable,
        sprintf("%04d", year)
    )

    dir.create(
        dir,
        recursive = TRUE,
        showWarnings = FALSE
    )

    file.path(
        dir,
        sprintf(
            "%04d_%02d_%s.nc",
            year,
            month,
            statistic
        )
    )

}

#==========================================================
# Expand a bounding box to a minimum size
#==========================================================

expand_bbox <- function(
        bbox,
        min_width = 0.5,
        min_height = 0.5
) {

    stopifnot(length(bbox) == 4)

    north <- bbox[1]
    west  <- bbox[2]
    south <- bbox[3]
    east  <- bbox[4]

    width <- east - west
    height <- north - south

    if (width < min_width) {

        pad <- (min_width - width) / 2

        west <- west - pad
        east <- east + pad

    }

    if (height < min_height) {

        pad <- (min_height - height) / 2

        south <- south - pad
        north <- north + pad

    }

    c(
        north,
        west,
        south,
        east
    )

}


# prepare rainfall history ------------------------------------------------
prepare_rainfall_history <- function(
        rainfall,
        date,
        baseline_years,
        period_days = 90,
        statistic = c(
            "cumulative",
            "rolling"
        ),
        window_days = 30
) {

    statistic <- match.arg(statistic)
    date <- as.Date(date)

    required <- c(
        "date",
        "precipitation_mm"
    )

    missing <- setdiff(
        required,
        names(rainfall)
    )

    if (length(missing) > 0L) {
        stop(
            "`rainfall` must contain: ",
            paste(required, collapse = ", "),
            ".",
            call. = FALSE
        )
    }

    analysis_year <- as.integer(
        format(date, "%Y")
    )

    years <- unique(
        c(
            baseline_years,
            analysis_year
        )
    )

    month_day <- format(
        date,
        "%m-%d"
    )

    out <- lapply(
        years,
        function(year) {

            target <- as.Date(
                sprintf(
                    "%04d-%s",
                    year,
                    month_day
                )
            )

            display_start <-
                target - (period_days - 1)

            if (statistic == "rolling") {
                data_start <-
                    display_start -
                    (window_days - 1)
            } else {
                data_start <- display_start
            }

            x <- rainfall[
                rainfall$date >= data_start &
                    rainfall$date <= target,
                ,
                drop = FALSE
            ]

            if (nrow(x) == 0L) {
                return(NULL)
            }

            x <- x[
                order(x$date),
                ,
                drop = FALSE
            ]

            if (statistic == "cumulative") {

                keep <- (
                    x$date >= display_start &
                        x$date <= target
                )

                x <- x[keep, , drop = FALSE]

                value <- cumsum(
                    x$precipitation_mm
                )

            } else {

                value <- vapply(
                    x$date,
                    function(d) {

                        start <-
                            d - (window_days - 1)

                        keep <- (
                            x$date >= start &
                                x$date <= d
                        )

                        dates <- x$date[keep]

                        if (
                            length(unique(dates)) <
                            window_days
                        ) {
                            return(NA_real_)
                        }

                        sum(
                            x$precipitation_mm[keep],
                            na.rm = TRUE
                        )
                    },
                    numeric(1)
                )

                keep <- (
                    x$date >= display_start &
                        x$date <= target
                )

                x <- x[keep, , drop = FALSE]
                value <- value[keep]
            }

            data.frame(
                date = x$date,
                year = year,
                value = value
            )
        }
    )

    out <- do.call(
        rbind,
        out
    )

    out$plot_date <- as.Date(
        paste0(
            "2000-",
            format(out$date, "%m-%d")
        )
    )

    out$period <- ifelse(
        out$year == analysis_year,
        "current",
        "baseline"
    )

    rownames(out) <- NULL

    out
}


# wind direction ----------------------------------------------------------

#' Calculate wind speed and direction
#'
#' Calculates wind speed and meteorological wind direction
#' from eastward (u) and northward (v) wind components.
#'
#' @param u_ms Numeric vector of eastward wind components in m/s.
#' @param v_ms Numeric vector of northward wind components in m/s.
#'
#' @return A data frame containing:
#' \describe{
#'   \item{wind_speed_ms}{Wind speed in m/s.}
#'   \item{wind_direction_deg}{Meteorological direction from which
#'   the wind is blowing, in degrees clockwise from north.}
#' }
#'
#' @export
calc_wind <- function(
        u_ms,
        v_ms
) {

    if (length(u_ms) != length(v_ms)) {
        stop(
            "`u_ms` and `v_ms` must have the same length.",
            call. = FALSE
        )
    }

    wind_speed_ms <- sqrt(
        u_ms^2 + v_ms^2
    )

    wind_direction_deg <- (
        atan2(
            -u_ms,
            -v_ms
        ) *
            180 / pi
    ) %% 360

    data.frame(
        wind_speed_ms = wind_speed_ms,
        wind_direction_deg = wind_direction_deg
    )
}

wind_direction_label <- function(x) {

    labels <- c(
        "N", "NE", "E", "SE",
        "S", "SW", "W", "NW"
    )

    i <- floor(
        ((x + 22.5) %% 360) / 45
    ) + 1

    labels[i]
}

