#' Analyse vegetation moisture anomaly
#'
#' Compares current NDMI with a seasonally matched historical
#' Sentinel-2 baseline. Historical observations are combined within
#' years before the baseline is calculated, so each year receives
#' equal weight.
#'
#' @param historical Historical `sbr_collection`.
#' @param current Current `sbr_collection`.
#' @param current_date Date to analyse.
#' @param window_days Seasonal window around `current_date`.
#' @param min_coverage Minimum proportion of valid pixels required.
#' @param min_years Minimum number of historical years required.
#' @param boundary Optional analysis boundary.
#' @param min_sd Minimum historical SD used for standardisation.
#'
#' @return An object of class `sbr_drought`.
#'
#' @export
analyse_drought <- function(
        historical,
        current,
        current_date,
        window_days = 30,
        min_coverage = 0.90,
        min_years = 3,
        boundary = NULL,
        min_sd = 0.01
) {

    if (!inherits(historical, "sbr_collection")) {
        stop("`historical` must be an sbr_collection.")
    }

    if (!inherits(current, "sbr_collection")) {
        stop("`current` must be an sbr_collection.")
    }

    current_date <- as.Date(current_date)

    if (is.na(current_date)) {
        stop("`current_date` must be a valid date.")
    }

    assets <- c("nir08", "swir16")

    # Historical NDMI observations
    historical_composites <- build_timeseries_composites(
        historical,
        assets = assets
    )

    historical_ndmi <- index_rasters(
        historical_composites,
        index = "ndmi",
        boundary = boundary
    )

    # Restrict baseline to observations from the same season
    historical_ndmi <- select_seasonal_baseline(
        historical_ndmi,
        current = current_date,
        window_days = window_days
    )

    if (length(historical_ndmi) == 0) {
        stop("No historical observations in seasonal window.")
    }

    # Historical coverage QC
    historical_qc <- filter_raster_coverage(
        historical_ndmi,
        min_coverage = min_coverage
    )

    historical_ndmi <- historical_qc$rasters

    if (length(historical_ndmi) == 0) {
        stop("No historical observations pass coverage threshold.")
    }

    # One raster per historical year
    annual <- annual_index_composites(
        historical_ndmi
    )

    if (length(annual) < min_years) {
        stop(
            "Fewer than ",
            min_years,
            " historical years remain after filtering."
        )
    }

    baseline <- build_drought_baseline(
        annual
    )

    # Current NDMI observations
    current_composites <- build_timeseries_composites(
        current,
        assets = assets
    )

    current_ndmi <- index_rasters(
        current_composites,
        index = "ndmi",
        boundary = boundary
    )

    if (!as.character(current_date) %in% names(current_ndmi)) {
        stop(
            "No current observation found for ",
            current_date,
            "."
        )
    }

    current_raster <- current_ndmi[[
        as.character(current_date)]]

    current_list <- setNames(
        list(current_raster),
        as.character(current_date)
    )

    current_qc <- raster_coverage(
        current_list,
        reference_pixels =
            historical_qc$reference_pixels
    )

    if (current_qc$coverage[1] < min_coverage) {
        stop(
            "Current observation does not meet coverage threshold."
        )
    }

    anomaly <- calculate_drought_anomaly(
        current = current_raster,
        baseline = baseline,
        min_sd = min_sd
    )

    baseline_years <- as.integer(
        names(annual)
    )

    summary <- summarise_drought(
        current = current_raster,
        anomaly = anomaly$anomaly,
        standardised = anomaly$standardised,
        current_date = current_date,
        baseline_years = baseline_years,
        window_days = window_days,
        coverage = current_qc$coverage[1]
    )

    out <- list(
        current = current_raster,
        baseline = baseline,
        anomaly = anomaly$anomaly,
        standardised = anomaly$standardised,
        annual = annual,
        summary = summary,
        coverage = list(
            historical = historical_qc$coverage,
            current = current_qc
        ),
        assets = assets
    )

    class(out) <- "sbr_drought"

    out
}








# annual composite --------------------------------------------------------


annual_index_composites <- function(x) {

    if (!is.list(x) || length(x) == 0) {
        stop("`x` must be a non-empty list of SpatRaster objects.")
    }

    dates <- as.Date(names(x))

    if (anyNA(dates)) {
        stop("`x` must have names that can be converted to dates.")
    }

    check_raster_geometry(x)

    years <- format(dates, "%Y")

    out <- lapply(
        unique(years),
        function(year) {

            xx <- x[years == year]

            terra::median(
                terra::rast(xx),
                na.rm = TRUE
            )
        }
    )

    names(out) <- unique(years)

    out
}


# baseline ----------------------------------------------------------------


build_drought_baseline <- function(x) {

    if (!is.list(x) || length(x) < 2) {
        stop("At least two annual rasters are required.")
    }

    check_raster_geometry(x)

    s <- terra::rast(x)

    list(
        median = terra::median(
            s,
            na.rm = TRUE
        ),

        mean = terra::mean(
            s,
            na.rm = TRUE
        ),

        sd = terra::app(
            s,
            sd,
            na.rm = TRUE
        ),

        n_years = length(x)
    )
}


# calculate anomaly -------------------------------------------------------

calculate_drought_anomaly <- function(
        current,
        baseline,
        min_sd = 0.01
) {

    terra::compareGeom(
        current,
        baseline$median,
        stopOnError = TRUE
    )

    anomaly <- current - baseline$median

    standardised <-
        (current - baseline$mean) /
        baseline$sd

    standardised[
        baseline$sd < min_sd
    ] <- NA

    names(anomaly) <- "ndmi_anomaly"
    names(standardised) <- "ndmi_standardised"

    list(
        anomaly = anomaly,
        standardised = standardised
    )
}


# summary -----------------------------------------------------------------

summarise_drought <- function(
        current,
        anomaly,
        standardised,
        current_date,
        baseline_years,
        window_days,
        coverage
) {

    current_q <- terra::global(
        current,
        fun = quantile,
        probs = c(0.25, 0.5, 0.75),
        na.rm = TRUE
    )

    anomaly_q <- terra::global(
        anomaly,
        fun = quantile,
        probs = c(0.25, 0.5, 0.75),
        na.rm = TRUE
    )

    proportion_negative <- terra::global(
        anomaly < 0,
        "mean",
        na.rm = TRUE
    )[1, 1]

    proportion_below_minus_2sd <- terra::global(
        standardised < -2,
        "mean",
        na.rm = TRUE
    )[1, 1]

    data.frame(
        current_date = as.Date(current_date),

        baseline_start = min(baseline_years),
        baseline_end = max(baseline_years),
        baseline_years = length(baseline_years),

        window_days = window_days,
        valid_coverage = coverage,

        current_ndmi_median = current_q[1, 2],

        anomaly_q25 = anomaly_q[1, 1],
        anomaly_median = anomaly_q[1, 2],
        anomaly_q75 = anomaly_q[1, 3],

        proportion_negative =
            proportion_negative,

        proportion_below_minus_2sd =
            proportion_below_minus_2sd
    )
}


# raster coverage ---------------------------------------------------------

raster_coverage <- function(
        x,
        reference_pixels = NULL
) {

    if (!is.list(x) || length(x) == 0) {
        stop("`x` must be a non-empty list of SpatRaster objects.")
    }

    valid_pixels <- vapply(
        x,
        function(r) {
            sum(is.finite(
                terra::values(
                    r,
                    mat = FALSE
                )
            ))
        },
        numeric(1)
    )

    if (is.null(reference_pixels)) {
        reference_pixels <- max(valid_pixels)
    }

    data.frame(
        date = as.Date(names(x)),
        valid_pixels = valid_pixels,
        coverage = valid_pixels / reference_pixels,
        row.names = NULL
    )
}


# filter ------------------------------------------------------------------

filter_raster_coverage <- function(
        x,
        min_coverage = 0.90,
        reference_pixels = NULL
) {

    coverage <- raster_coverage(
        x,
        reference_pixels = reference_pixels
    )

    keep <- coverage$coverage >= min_coverage

    list(
        rasters = x[
            as.character(coverage$date[keep])
        ],
        coverage = coverage,
        reference_pixels = if (is.null(reference_pixels)) {
            max(coverage$valid_pixels)
        } else {
            reference_pixels
        }
    )
}



