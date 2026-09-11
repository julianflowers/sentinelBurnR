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


# analyse climate ---------------------------------------------------------

#' Analyse climate conditions
#'
#' Summarises recent rainfall and, optionally, temperature conditions
#' relative to a historical baseline.
#'
#' @param rainfall Rainfall data used to calculate recent rainfall,
#'   rainfall anomalies, and dry-spell statistics.
#' @param temperature Optional temperature data used to calculate
#'   temperature summaries. If `NULL`, temperature analysis is omitted.
#' @param date Date for which climate conditions are assessed.
#' @param baseline_years Years used to define the historical climate
#'   baseline.
#' @param windows Numeric vector giving the time windows, in days, over
#'   which recent climate conditions are summarised.
#' @param dry_spell_window Number of days preceding `date` used to
#'   assess dry-spell conditions.
#' @param dry_threshold_mm Daily rainfall threshold, in millimetres,
#'   below which a day is considered dry.
#' @param hot_threshold Temperature threshold, in degrees Celsius, used
#'   to identify hot days.
#' @param very_hot_threshold Temperature threshold, in degrees Celsius,
#'   used to identify very hot days.
#'
#' @return An object containing climate summaries for the requested
#'   date and historical baseline.
#'
#' @importFrom stats median quantile sd setNames
#' @export

analyse_climate <- function(
        rainfall,
        temperature = NULL,
        date,
        baseline_years,
        windows = c(30, 60, 90),
        dry_spell_window = 90,
        dry_threshold_mm = 1,
        hot_threshold = 25,
        very_hot_threshold = 30
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

        temperature_summary <- NULL

        if (!is.null(temperature)) {

            temperature_summary <- purrr::map_dfr(
                windows,
                \(window) {
                    compare_temperature_window(
                        temperature = temperature,
                        date = date,
                        baseline_years = baseline_years,
                        window_days = window,
                        hot_threshold = hot_threshold,
                        very_hot_threshold = very_hot_threshold
                    )
                }
            )
        }

        dry_spell <- summarise_dry_spell(
            rainfall = rainfall,
            date = date,
            window_days = max(windows),
            threshold_mm = 1
        )

        dry_spell_baseline <- summarise_dry_spell_baseline(
        rainfall = rainfall,
        date = date,
        baseline_years = baseline_years,
        current_dry_spell = dry_spell,
        window_days = 90,
        threshold_mm = 1
        )

        out <- list(
            date = date,
            baseline_years = baseline_years,
            windows = windows,
            summary = summary,
            dry_spell = dry_spell,
            dry_spell_baseline = dry_spell_baseline,
            temperature = temperature_summary,
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

    if (!is.null(x$dry_spell_baseline)) {

        dsb <- x$dry_spell_baseline

        cat(
            sprintf(
                "Baseline median spell: %.1f days\n",
                dsb$median_days
            )
        )

        cat(
            sprintf(
                "Dry-spell percentile: %.1f%%\n",
                dsb$percentile
            )
        )

        if (!is.null(x$temperature)) {

            cat("\nTemperature:\n")

            print(
                x$temperature |>
                    dplyr::select(
                        .data$window_days,
                        .data$mean_max_c,
                        .data$baseline_median_mean_max_c,
                        .data$mean_max_anomaly_c,
                        .data$mean_max_percentile,
                        .data$maximum_c,
                        .data$hot_days,
                        .data$very_hot_days
                    ),
                row.names = FALSE
            )
        }

    }

    invisible(x)
}

# dry spell --------------------------------------------------------------

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


# baseline_dry_spell  -----------------------------------------------------
baseline_dry_spells <- function(
        rainfall,
        date,
        baseline_years,
        window_days = 90,
        threshold_mm = 1
) {

    date <- as.Date(date)
    md <- format(date, "%m-%d")

    out <- lapply(baseline_years, function(year) {

        assessment_date <- as.Date(
            sprintf("%04d-%s", year, md)
        )

        x <- summarise_dry_spell(
            rainfall = rainfall,
            date = assessment_date,
            window_days = window_days,
            threshold_mm = threshold_mm
        )

        data.frame(
            year = year,
            max_dry_days = x$max_consecutive_dry_days,
            dry_days = x$dry_days,
            dry_spell_start = x$dry_spell_start,
            dry_spell_end = x$dry_spell_end
        )
    })

    do.call(rbind, out)
}


# summarise baseline dry spell --------------------------------------------


summarise_dry_spell_baseline <- function(
        rainfall,
        date,
        baseline_years,
        current_dry_spell,
        window_days = 90,
        threshold_mm = 1
) {

    baseline <- baseline_dry_spells(
        rainfall = rainfall,
        date = date,
        baseline_years = baseline_years,
        window_days = window_days,
        threshold_mm = threshold_mm
    )

    current <- current_dry_spell$max_consecutive_dry_days

    list(
        baseline = baseline,
        median_days = median(
            baseline$max_dry_days,
            na.rm = TRUE
        ),
        max_days = max(
            baseline$max_dry_days,
            na.rm = TRUE
        ),
        percentile = 100 * mean(
            baseline$max_dry_days < current,
            na.rm = TRUE
        )
    )
}


# compare temperature window ----------------------------------------------
compare_temperature_window <- function(
        temperature,
        date,
        baseline_years,
        window_days,
        hot_threshold = 25,
        very_hot_threshold = 30
) {

    date <- as.Date(date)

    current <- summarise_temperature_window(
        temperature = temperature,
        date = date,
        window_days = window_days,
        hot_threshold = hot_threshold,
        very_hot_threshold = very_hot_threshold
    )

    baseline <- purrr::map_dfr(
        baseline_years,
        \(year) {

            baseline_date <- as.Date(sprintf(
                "%04d-%02d-%02d",
                year,
                lubridate::month(date),
                lubridate::day(date)
            ))

            summarise_temperature_window(
                temperature = temperature,
                date = baseline_date,
                window_days = window_days,
                hot_threshold = hot_threshold,
                very_hot_threshold = very_hot_threshold
            ) |>
                dplyr::mutate(
                    year = year,
                    .before = 1
                )
        }
    )

    baseline_complete <- baseline |>
        dplyr::filter(.data$complete)

    if (nrow(baseline_complete) == 0L) {
        stop(
            "No complete baseline temperature windows available.",
            call. = FALSE
        )
    }

    tibble::tibble(
        window_days = window_days,

        mean_max_c = current$mean_max_c,
        baseline_median_mean_max_c = median(
            baseline_complete$mean_max_c,
            na.rm = TRUE
        ),
        mean_max_anomaly_c =
            current$mean_max_c -
            median(
                baseline_complete$mean_max_c,
                na.rm = TRUE
            ),
        mean_max_percentile =
            100 * mean(
                baseline_complete$mean_max_c <
                    current$mean_max_c
            ),
        hot_days_percentile =
            100 * mean(
                baseline_complete$hot_days <
                    current$hot_days
            ),

        very_hot_days_percentile =
            100 * mean(
                baseline_complete$very_hot_days <
                    current$very_hot_days
            ),

        maximum_c = current$maximum_c,
        baseline_median_maximum_c = median(
            baseline_complete$maximum_c,
            na.rm = TRUE
        ),
        maximum_percentile =
            100 * mean(
                baseline_complete$maximum_c <
                    current$maximum_c
            ),

        hot_days = current$hot_days,
        baseline_median_hot_days = median(
            baseline_complete$hot_days
        ),

        very_hot_days = current$very_hot_days,
        baseline_median_very_hot_days = median(
            baseline_complete$very_hot_days
        ),

        n_baseline_years = nrow(baseline_complete)
    )
}

