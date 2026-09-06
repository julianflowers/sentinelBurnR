summarise_rainfall_window <- function(
        rainfall,
        date,
        window_days = 30
) {

    if (!inherits(rainfall, "sbr_rainfall")) {
        stop("`rainfall` must be an sbr_rainfall object.")
    }

    date <- as.Date(date)

    if (length(date) != 1L || is.na(date)) {
        stop("`date` must be a single valid date.")
    }

    if (length(window_days) != 1L ||
        !is.numeric(window_days) ||
        is.na(window_days) ||
        window_days < 1) {
        stop("`window_days` must be a positive number.")
    }

    start_date <- date - window_days + 1

    x <- rainfall[
        rainfall$date >= start_date &
            rainfall$date <= date,
        ,
        drop = FALSE
    ]

    expected_dates <- seq(
        start_date,
        date,
        by = "day"
    )

    if (!setequal(x$date, expected_dates) ||
        anyDuplicated(x$date)) {

        stop(
            sprintf(
                "Rainfall data incomplete: expected %d unique daily observations.",
                window_days
            )
        )
    }

    data.frame(
        start_date = start_date,
        end_date = date,
        window_days = window_days,
        rainfall_mm = sum(
            x$precipitation_mm,
            na.rm = TRUE
        ),
        mean_daily_mm = mean(
            x$precipitation_mm,
            na.rm = TRUE
        ),
        max_daily_mm = max(
            x$precipitation_mm,
            na.rm = TRUE
        ),
        days_observed = nrow(x)
    )
}


compare_rainfall_window <- function(
        rainfall,
        date,
        baseline_years,
        window_days = 30
) {

    date <- as.Date(date)

    current <- summarise_rainfall_window(
        rainfall,
        date = date,
        window_days = window_days
    )

    baseline <- lapply(
        baseline_years,
        function(year) {

            baseline_date <- as.Date(
                sprintf(
                    "%04d-%s",
                    year,
                    format(date, "%m-%d")
                )
            )

            x <- summarise_rainfall_window(
                rainfall,
                date = baseline_date,
                window_days = window_days
            )

            x$year <- year

            x
        }
    )

    baseline <- do.call(
        rbind,
        baseline
    )

    baseline_median <- median(
        baseline$rainfall_mm,
        na.rm = TRUE
    )

    current_mm <- current$rainfall_mm

    data.frame(
        window_days = window_days,
        current_mm = current_mm,
        baseline_median_mm = baseline_median,
        anomaly_mm = current_mm - baseline_median,
        percent_of_normal =
            100 * current_mm / baseline_median,
        historical_percentile =
            100 * mean(
                baseline$rainfall_mm <= current_mm,
                na.rm = TRUE
            )
    )

}

analyse_climate <- function(
        rainfall,
        date,
        baseline_years,
        windows = c(30, 60, 90)
    ) {

        if (!inherits(rainfall, "sbr_rainfall")) {
            stop("`rainfall` must be an sbr_rainfall object.")
        }

        date <- as.Date(date)

        if (length(date) != 1L || is.na(date)) {
            stop("`date` must be a single valid date.")
        }

        if (length(baseline_years) < 2L) {
            stop("At least two baseline years are required.")
        }

        if (any(windows < 1)) {
            stop("`windows` must contain positive values.")
        }

        summary <- lapply(
            windows,
            function(window_days) {
                compare_rainfall_window(
                    rainfall = rainfall,
                    date = date,
                    baseline_years = baseline_years,
                    window_days = window_days
                )
            }
        )

        summary <- do.call(
            rbind,
            summary
        )

        rownames(summary) <- NULL

        dry_spell <- summarise_dry_spell(
            rainfall = rainfall,
            date = date,
            window_days = max(windows),
            threshold_mm = 1
        )

        out <- list(
            date = date,
            baseline_years = baseline_years,
            windows = windows,
            summary = summary,
            dry_spell = dry_spell,
            source = attr(rainfall, "source")
        )

        class(out) <- "sbr_climate"

        out
}


# print climate -----------------------------------------------------------


#' Print climate analysis
#'
#' @param x An `sbr_climate` object.
#' @param ... Additional arguments, currently unused.
#'
#' @return `x`, invisibly.
#' @export
print.sbr_climate <- function(x, ...) {

    cat("\n")
    cat("<sbr_climate>\n")

    cat(sprintf(
        "Assessment date:  %s\n",
        x$date
    ))

    cat(sprintf(
        "Baseline:         %d-%d\n",
        min(x$baseline_years),
        max(x$baseline_years)
    ))

    if (!is.null(x$source)) {
        cat(sprintf(
            "Source:           %s\n",
            x$source
        ))
    }

    cat("\nRainfall:\n")

    print(
        x$summary,
        row.names = FALSE
    )

    # Dry-spell summary
    if (!is.null(x$dry_spell)) {

        d <- x$dry_spell

        cat("\nDry conditions:\n")

        cat(sprintf(
            "Dry days (<%.1f mm): %d of %d\n",
            d$threshold_mm,
            d$dry_days,
            d$window_days
        ))

        cat(sprintf(
            "Longest dry spell:   %d days\n",
            d$max_consecutive_dry_days
        ))

        cat(sprintf(
            "Dry-spell dates:      %s to %s\n",
            d$dry_spell_start,
            d$dry_spell_end
        ))

        cat(sprintf(
            "Days since rain:      %d\n",
            d$days_since_rain
        ))
    }

    invisible(x)
}

# dry speill --------------------------------------------------------------

summarise_dry_spell <- function(
        rainfall,
        date,
        window_days = 90,
        threshold_mm = 1
) {

    if (!inherits(rainfall, "sbr_rainfall")) {
        stop("`rainfall` must be an sbr_rainfall object.")
    }

    date <- as.Date(date)
    start_date <- date - window_days + 1

    x <- rainfall[
        rainfall$date >= start_date &
            rainfall$date <= date,
        ,
        drop = FALSE
    ]

    expected_dates <- seq(start_date, date, by = "day")

    if (!setequal(x$date, expected_dates) ||
        anyDuplicated(x$date)) {
        stop(
            sprintf(
                "Rainfall data incomplete: expected %d unique daily observations.",
                window_days
            )
        )
    }

    x <- x[order(x$date), ]

    dry <- x$precipitation_mm < threshold_mm

    runs <- rle(dry)

    if (any(runs$values)) {

        ends <- cumsum(runs$lengths)
        starts <- ends - runs$lengths + 1

        dry_runs <- which(runs$values)

        longest <- dry_runs[
            which.max(runs$lengths[dry_runs])
        ]

        max_dry_days <- runs$lengths[longest]

        dry_spell_start <- x$date[starts[longest]]
        dry_spell_end <- x$date[ends[longest]]

    } else {

        max_dry_days <- 0L
        dry_spell_start <- as.Date(NA)
        dry_spell_end <- as.Date(NA)
    }


    wet_dates <- x$date[x$precipitation_mm >= threshold_mm]

    days_since_rain <- if (length(wet_dates)) {
        as.integer(date - max(wet_dates))
    } else {
        window_days
    }

    data.frame(
        window_days = window_days,
        threshold_mm = threshold_mm,
        max_consecutive_dry_days = max_dry_days,
        days_since_rain = days_since_rain,
        dry_days = sum(dry),
        dry_spell_start = dry_spell_start,
        dry_spell_end = dry_spell_end
    )
}


