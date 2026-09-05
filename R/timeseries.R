#' Summarise a spectral index through time
#'
#' Calculates a spatial summary of a spectral index for a sequence
#' of dated Sentinel-2 composites.
#'
#' @param composites A named list of `SpatRaster` composites. Names
#'   must be valid dates.
#' @param index Spectral index to calculate. One of `"nbr"`, `"ndvi"`
#'  `"msi"` or `"ndmi"`.
#' @param boundary Optional spatial boundary used to crop and mask
#'   each index raster.
#'
#' @return A data frame containing one row per date, with the median,
#'   interquartile range and number of valid pixels.
#'
#' @export
#'
#

subset_collection_date <- function(
        collection,
        date
) {

    if (!inherits(collection, "sbr_collection")) {
        stop(
            "`collection` must be an sbr_collection.",
            call. = FALSE
        )
    }

    date <- as.Date(date)

    keep <- collection$files$date == date

    if (!any(keep)) {
        stop(
            "No files found for date ",
            date,
            ".",
            call. = FALSE
        )
    }

    out <- collection

    out$files <- collection$files[
        keep,
        ,
        drop = FALSE
    ]

    out
}


index_timeseries <- function(
        composites,
        index = c(
            "nbr",
            "ndvi",
            "ndmi",
            "msi"
        ),
        boundary = NULL
) {

    index <- match.arg(index)

    if (!is.list(composites) ||
        length(composites) == 0) {

        stop(
            "`composites` must be a non-empty list.",
            call. = FALSE
        )
    }

    if (is.null(names(composites)) ||
        any(names(composites) == "")) {

        stop(
            "Names of `composites` must be valid dates.",
            call. = FALSE
        )
    }

    dates <- tryCatch(
        as.Date(
            names(composites)
        ),
        error = function(e) {
            stop(
                "Names of `composites` must be valid dates.",
                call. = FALSE
            )
        }
    )

    if (anyNA(dates)) {
        stop(
            "Names of `composites` must be valid dates.",
            call. = FALSE
        )
    }

    out <- lapply(
        seq_along(composites),
        function(i) {

            x <- composites[[i]]


            if (!inherits(x, "SpatRaster")) {
                stop(
                    "All composites must be SpatRaster objects.",
                    call. = FALSE
                )
            }

            idx <- calculate_index(
                x,
                index
            )

            if (!is.null(boundary)) {

                b <- aoi_to_spatvector(boundary)

                if (!terra::same.crs(b, idx)) {
                    b <- terra::project(
                        b,
                        terra::crs(idx)
                    )
                }

                idx <- terra::crop(
                    idx,
                    b
                )

                idx <- terra::mask(
                    idx,
                    b
                )
            }

            values <- terra::values(
                idx,
                mat = FALSE
            )

            values <- values[
                is.finite(values)
            ]

            data.frame(
                date = dates[i],

                median = if (length(values)) {
                    stats::median(values)
                } else {
                    NA_real_
                },

                q25 = if (length(values)) {
                    stats::quantile(
                        values,
                        0.25,
                        names = FALSE
                    )
                } else {
                    NA_real_
                },

                q75 = if (length(values)) {
                    stats::quantile(
                        values,
                        0.75,
                        names = FALSE
                    )
                } else {
                    NA_real_
                },

                valid_pixels = length(values)
            )
        }
    )

    out <- do.call(
        rbind,
        out
    )

    rownames(out) <- NULL

    out
}

#' Build dated Sentinel-2 composites
#'
#' Builds one Sentinel-2 composite for each acquisition date in a
#' downloaded collection.
#'
#' Each date is processed independently using the standard
#' `build_composite()` pipeline, including SCL masking, tile
#' mosaicking, band alignment and AOI masking.
#'
#' @param collection An `sbr_collection`.
#' @param assets Character vector of Sentinel-2 assets to include.
#'
#' @return A named list of `SpatRaster` composites, with names
#'   corresponding to acquisition dates.
#'
#' @export
build_timeseries_composites <- function(
        collection,
        assets = s2_burn_assets,
        cache = TRUE,
        overwrite = FALSE
) {

    if (!inherits(collection, "sbr_collection")) {
        stop(
            "`collection` must be an sbr_collection.",
            call. = FALSE
        )
    }

    asset_table <- files(collection)

    if (!"date" %in% names(asset_table)) {
        stop(
            "Collection does not contain acquisition dates.",
            call. = FALSE
        )
    }

    dates <- sort(
        unique(asset_table$date)
    )

    composites <- lapply(
        dates,
        function(date) {

            message(
                "Building composite for ",
                date
            )

            dated_collection <- subset_collection_date(
                collection,
                date
            )

            dated_collection <- subset_collection_date(
                collection,
                date
            )

            message(
                "Cache key: ",
                composite_cache_key(
                    dated_collection,
                    assets
                )
            )

            message(
                "Cache exists: ",
                file.exists(
                    composite_cache_file(
                        dated_collection,
                        assets
                    )
                )
            )

            build_composite(
                dated_collection,
                assets = assets,
                cache = cache,
                overwrite = overwrite
            )
        }
    )

    names(composites) <- as.character(
        dates
    )

    composites
}

#' Build a Sentinel-2 spectral index time series
#'
#' Builds one composite for each acquisition date in a Sentinel-2
#' collection and calculates a spatial summary of the requested
#' spectral index.
#'
#' @param collection An `sbr_collection`.
#' @param index Spectral index to calculate. One of `"nbr"`, `"ndvi"`
#'  `"msi"` or `"ndmi"`.
#' @param boundary Optional spatial boundary used to crop and mask
#'   each index raster.
#'
#' @return A data frame containing one row per acquisition date,
#'   with the median, interquartile range and number of valid pixels.
#'
#' @export
sentinel_timeseries <- function(
        collection,
        index = c(
            "nbr",
            "ndvi",
            "ndmi",
            "msi"
        ),
        boundary = NULL
) {

    index <- match.arg(index)

    assets <- switch(
        index,
        nbr = c(
            "nir08",
            "swir22"
        ),
        ndvi = c(
            "red",
            "nir08"
        ),
        ndmi = c(
            "nir08",
            "swir16"
        ),
        msi = c(
            "nir08",
            "swir16"
        )
    )

    composites <- build_timeseries_composites(
        collection = collection,
        assets = assets
    )

    index_timeseries(
        composites = composites,
        index = index,
        boundary = boundary
    )
}

# # tile extract ----------------------------------------------------------

s2_item_tile <- function(item) {

    properties <- item$properties

    zone <- properties$`mgrs:utm_zone`
    band <- properties$`mgrs:latitude_band`
    square <- properties$`mgrs:grid_square`

    # Prefer explicit MGRS metadata when available.
    if (!is.null(zone) &&
        length(zone) == 1 &&
        !is.null(band) &&
        length(band) == 1 &&
        !is.null(square) &&
        length(square) == 1) {

        return(
            paste0(
                zone,
                band,
                square
            )
        )
    }

    # Fall back to extracting the MGRS tile from the
    # Sentinel-2 item ID.
    tile <- sub(
        "^.*_([0-9]{2}[A-Z]{3})_.*$",
        "\\1",
        item$id
    )

    if (identical(tile, item$id)) {
        stop(
            "Could not determine MGRS tile for Sentinel-2 item `",
            item$id,
            "`.",
            call. = FALSE
        )
    }

    tile
}

# select timeseries -------------------------------------------------------


#' Select Sentinel-2 acquisitions for a time series
#'
#' Selects approximately regularly spaced Sentinel-2 acquisitions,
#' preferring acquisitions with lower mean cloud cover.
#'
#' Only complete acquisitions are retained. An acquisition is defined
#' by acquisition date and satellite, so observations from different
#' Sentinel-2 platforms are not mixed.
#'
#' @param search An `sbr_search`.
#' @param interval Minimum interval between selected observations,
#'   in days.
#' @param max_cloud Maximum mean cloud cover percentage for an
#'   acquisition.
#'
#' @return An `sbr_search` containing the selected STAC items.
#'
#' @export
select_timeseries <- function(
        search,
        interval = 10,
        max_cloud = 30
) {

    if (!inherits(search, "sbr_search")) {
        stop(
            "`search` must be an sbr_search.",
            call. = FALSE
        )
    }

    if (!is.numeric(interval) ||
        length(interval) != 1 ||
        is.na(interval) ||
        interval <= 0) {

        stop(
            "`interval` must be a positive number.",
            call. = FALSE
        )
    }

    if (!is.numeric(max_cloud) ||
        length(max_cloud) != 1 ||
        is.na(max_cloud) ||
        max_cloud < 0 ||
        max_cloud > 100) {

        stop(
            "`max_cloud` must be between 0 and 100.",
            call. = FALSE
        )
    }

    items <- search$items$features

    if (length(items) == 0) {
        stop(
            "Search contains no Sentinel-2 items.",
            call. = FALSE
        )
    }

    # Extract information required to identify acquisitions.
    info <- data.frame(
        item = seq_along(items),

        date = as.Date(
            vapply(
                items,
                function(x) {
                    substr(
                        x$properties$datetime,
                        1,
                        10
                    )
                },
                character(1)
            )
        ),

        satellite = vapply(
            items,
            function(x) {
                x$properties$platform
            },
            character(1)
        ),

        tile = vapply(
            items,
            s2_item_tile,
            character(1)
        ),



        cloud = vapply(
            items,
            function(x) {
                x$properties$`eo:cloud_cover`
            },
            numeric(1)
        )
    )

    # Summarise cloud cover for each acquisition.
    acquisition <- aggregate(
        cloud ~ date + satellite,
        data = info,
        FUN = mean
    )

    names(acquisition)[
        names(acquisition) == "cloud"
    ] <- "mean_cloud"

    # Determine the number of tiles represented by each acquisition.
    tile_count <- aggregate(
        tile ~ date + satellite,
        data = info,
        FUN = function(x) {
            length(unique(x))
        }
    )

    names(tile_count)[
        names(tile_count) == "tile"
    ] <- "n_tiles"

    acquisition <- merge(
        acquisition,
        tile_count,
        by = c(
            "date",
            "satellite"
        )
    )

    # The largest observed tile count represents complete coverage
    # for this search/AOI.
    required_tiles <- max(
        acquisition$n_tiles
    )

    # Retain complete acquisitions satisfying the cloud threshold.
    acquisition <- acquisition[
        acquisition$n_tiles == required_tiles &
            acquisition$mean_cloud <= max_cloud,
        ,
        drop = FALSE
    ]

    if (nrow(acquisition) == 0) {
        stop(
            "No complete acquisitions satisfy the cloud threshold.",
            call. = FALSE
        )
    }

    acquisition <- acquisition[
        order(acquisition$date),
        ,
        drop = FALSE
    ]

    acquisition$acquisition_id <- seq_len(
        nrow(acquisition)
    )

    # Select the clearest available acquisition, then exclude
    # acquisitions closer than `interval` days to it. Repeat until
    # no candidates remain.
    candidates <- acquisition[
        order(
            acquisition$mean_cloud,
            acquisition$date
        ),
        ,
        drop = FALSE
    ]

    selected_ids <- integer(0)

    while (nrow(candidates) > 0) {

        best <- candidates[
            1,
            ,
            drop = FALSE
        ]

        selected_ids <- c(
            selected_ids,
            best$acquisition_id
        )

        distance <- abs(
            as.numeric(
                candidates$date -
                    best$date
            )
        )

        candidates <- candidates[
            distance >= interval,
            ,
            drop = FALSE
        ]
    }

    selected <- acquisition[
        acquisition$acquisition_id %in%
            selected_ids,
        ,
        drop = FALSE
    ]

    selected <- selected[
        order(selected$date),
        ,
        drop = FALSE
    ]

    # Retain all STAC items belonging to the selected acquisitions.
    keep <- vapply(
        seq_along(items),
        function(i) {

            any(
                selected$date == info$date[i] &
                    selected$satellite ==
                    info$satellite[i]
            )
        },
        logical(1)
    )

    out <- search

    out$items$features <- items[
        keep
    ]

    out
}


# plot timeseries ---------------------------------------------------------

#' Plot a Sentinel-2 spectral index time series
#'
#' Plots the median spectral index through time together with the
#' interquartile range.
#'
#' @param x A data frame returned by `sentinel_timeseries()` or
#'   `index_timeseries()`.
#' @param index Name of the spectral index, used for the y-axis label.
#'
#' @return A `ggplot` object.
#'
#' @export
plot_timeseries <- function(
        x,
        index = "NBR"
) {

    required <- c(
        "date",
        "median",
        "q25",
        "q75"
    )

    missing <- setdiff(
        required,
        names(x)
    )

    if (length(missing) > 0) {
        stop(
            "`x` must contain: ",
            paste(
                required,
                collapse = ", "
            ),
            ".",
            call. = FALSE
        )
    }

    if (!inherits(x$date, "Date")) {
        stop(
            "`x$date` must be a Date vector.",
            call. = FALSE
        )
    }

    ggplot2::ggplot(
        x,
        ggplot2::aes(
            x = date,
            y = median
        )
    ) +
        ggplot2::geom_ribbon(
            ggplot2::aes(
                ymin = q25,
                ymax = q75
            ),
            alpha = 0.25
        ) +
        ggplot2::geom_line(
            linewidth = 0.8
        ) +
        ggplot2::geom_point(
            size = 2
        ) +
        ggplot2::scale_x_date(
            date_breaks = "2 weeks",
            date_labels = "%d %b"
        ) +
        ggplot2::labs(
            x = NULL,
            y = index,
            title = paste(
                index,
                "through time"
            )
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(
                angle = 45,
                hjust = 1
            )
        )
}

#' Calculate change in a spectral index through time
#'
#' Calculates the change in median spectral index between consecutive
#' observations in a time series.
#'
#' @param x A data frame returned by `sentinel_timeseries()` or
#'   `index_timeseries()`.
#'
#' @return A data frame containing the start and end dates of each
#'   interval, the median index values and the change between them.
#'
#' @export
timeseries_change <- function(x) {

    required <- c(
        "date",
        "median"
    )

    missing <- setdiff(
        required,
        names(x)
    )

    if (length(missing) > 0) {
        stop(
            "`x` must contain: ",
            paste(
                required,
                collapse = ", "
            ),
            ".",
            call. = FALSE
        )
    }

    if (!inherits(x$date, "Date")) {
        stop(
            "`x$date` must be a Date vector.",
            call. = FALSE
        )
    }

    if (nrow(x) < 2) {
        stop(
            "`x` must contain at least two observations.",
            call. = FALSE
        )
    }

    x <- x[
        order(x$date),
        ,
        drop = FALSE
    ]

    data.frame(
        start_date = x$date[-nrow(x)],
        end_date = x$date[-1],
        start_median = x$median[-nrow(x)],
        end_median = x$median[-1],
        change = diff(x$median)
    )
}

#' Detect disturbances in a spectral index time series
#'
#' Identifies intervals containing a large negative change in a
#' spectral index time series.
#'
#' This function identifies candidate disturbance events. A detected
#' disturbance does not necessarily represent fire and should be
#' interpreted using the underlying imagery and other evidence.
#'
#' @param x A data frame returned by `sentinel_timeseries()` or
#'   `index_timeseries()`.
#' @param threshold Minimum negative change required to identify a
#'   disturbance.
#'
#' @return A data frame containing intervals identified as candidate
#'   disturbances.
#'
#' @export
detect_disturbance <- function(
        x,
        threshold = -0.2
) {

    if (!is.numeric(threshold) ||
        length(threshold) != 1L ||
        is.na(threshold) ||
        threshold >= 0) {

        stop(
            "`threshold` must be a negative number.",
            call. = FALSE
        )
    }

    changes <- timeseries_change(x)

    out <- changes[
        changes$change <= threshold,
        ,
        drop = FALSE
    ]

    rownames(out) <- NULL

    out
}

#' Select pre- and post-disturbance dates
#'
#' Identifies the strongest candidate disturbance in a spectral index
#' time series and returns the acquisitions immediately before and
#' after the detected change.
#'
#' @param x A data frame returned by `sentinel_timeseries()` or
#'   `index_timeseries()`.
#' @param threshold Minimum negative change required to identify a
#'   disturbance.
#'
#' @return A list containing `pre`, `post` and `change`.
#'
#' @export
disturbance_dates <- function(
        x,
        threshold = -0.2
) {

    disturbances <- detect_disturbance(
        x,
        threshold = threshold
    )

    if (nrow(disturbances) == 0) {
        stop(
            "No candidate disturbance was detected.",
            call. = FALSE
        )
    }

    event <- disturbances[
        which.min(disturbances$change),
        ,
        drop = FALSE
    ]

    list(
        pre = event$start_date[[1]],
        post = event$end_date[[1]],
        change = event$change[[1]]
    )
}


# antecedent conditions ---------------------------------------------------
#' Summarise antecedent vegetation conditions
#'
#' Summarises vegetation condition and moisture during a period
#' preceding a specified assessment date.
#'
#' For NDVI and NDMI the function reports the current value, maximum
#' value within the look-back window, change from that maximum, and
#' the linear trend through observations in the window.
#'
#' @param ndvi An NDVI time series returned by `sentinel_timeseries()`
#'   or `index_timeseries()`.
#' @param ndmi An NDMI time series returned by `sentinel_timeseries()`
#'   or `index_timeseries()`.
#' @param date Assessment date.
#' @param window Number of days preceding `date` to include.
#'
#' @return A data frame containing antecedent vegetation-condition
#'   metrics.
#'
#' @export
antecedent_conditions <- function(
        ndvi,
        ndmi,
        date,
        window = 45
) {

    date <- as.Date(date)

    if (is.na(date)) {
        stop(
            "`date` must be a valid date.",
            call. = FALSE
        )
    }

    if (!is.numeric(window) ||
        length(window) != 1L ||
        is.na(window) ||
        window <= 0) {

        stop(
            "`window` must be a positive number.",
            call. = FALSE
        )
    }

    summarise_index <- function(x) {

        required <- c(
            "date",
            "median"
        )

        if (!all(required %in% names(x))) {
            stop(
                "Time series must contain `date` and `median`.",
                call. = FALSE
            )
        }

        if (!inherits(x$date, "Date")) {
            stop(
                "Time-series dates must be Date values.",
                call. = FALSE
            )
        }

        start_date <- date - window

        x <- x[
            x$date >= start_date &
                x$date <= date,
            ,
            drop = FALSE
        ]

        if (nrow(x) < 2) {
            stop(
                "At least two observations are required within the ",
                "antecedent window.",
                call. = FALSE
            )
        }

        x <- x[
            order(x$date),
            ,
            drop = FALSE
        ]

        # Use the latest observation on or before the
        # assessment date as the current condition.
        current <- x$median[
            which.max(x$date)
        ]

        peak <- max(
            x$median,
            na.rm = TRUE
        )

        days <- as.numeric(
            x$date - min(x$date)
        )

        model <- stats::lm(
            x$median ~ days
        )

        trend <- unname(
            stats::coef(model)[["days"]]
        )

        c(
            current = current,
            peak = peak,
            change = current - peak,
            trend = trend
        )
    }

    ndvi_summary <- summarise_index(ndvi)
    ndmi_summary <- summarise_index(ndmi)

    data.frame(
        date = date,
        window_days = window,

        ndvi_current =
            ndvi_summary[["current"]],

        ndvi_peak =
            ndvi_summary[["peak"]],

        ndvi_change =
            ndvi_summary[["change"]],

        ndvi_trend =
            ndvi_summary[["trend"]],

        ndmi_current =
            ndmi_summary[["current"]],

        ndmi_peak =
            ndmi_summary[["peak"]],

        ndmi_change =
            ndmi_summary[["change"]],

        ndmi_trend =
            ndmi_summary[["trend"]]
    )
}


# index trend -------------------------------------------------------------
#' Calculate spatial trends in a spectral index
#'
#' Calculates the linear trend in a spectral index through time for
#' every raster cell in a sequence of dated Sentinel-2 composites.
#'
#' The returned raster contains the slope of the spectral index
#' against time, expressed as index units per day. Negative values
#' indicate a declining index and positive values indicate an
#' increasing index.
#'
#' @param composites A named list of `SpatRaster` composites. Names
#'   must be valid dates.
#' @param index Spectral index to calculate. One of `"nbr"`, `"ndvi"`,
#'   `"ndmi"` or `"msi"`.
#' @param start Optional start date.
#' @param end Optional end date.
#' @param min_obs Minimum number of valid observations required to
#'   calculate a trend.
#'
#' @return A `SpatRaster` containing the per-cell linear trend in
#'   index units per day.
#'
#' @export
index_trend <- function(
        composites,
        index = c(
            "nbr",
            "ndvi",
            "ndmi",
            "msi"
        ),
        start = NULL,
        end = NULL,
        min_obs = 3
) {

    index <- match.arg(index)

    if (!is.list(composites) ||
        length(composites) == 0) {

        stop(
            "`composites` must be a non-empty list.",
            call. = FALSE
        )
    }

    if (is.null(names(composites)) ||
        any(names(composites) == "")) {

        stop(
            "Names of `composites` must be valid dates.",
            call. = FALSE
        )
    }

    dates <- as.Date(
        names(composites)
    )

    if (anyNA(dates)) {
        stop(
            "Names of `composites` must be valid dates.",
            call. = FALSE
        )
    }

    if (!is.null(start)) {
        start <- as.Date(start)
    }

    if (!is.null(end)) {
        end <- as.Date(end)
    }

    keep <- rep(
        TRUE,
        length(dates)
    )

    if (!is.null(start)) {
        keep <- keep &
            dates >= start
    }

    if (!is.null(end)) {
        keep <- keep &
            dates <= end
    }

    composites <- composites[keep]
    dates <- dates[keep]

    if (length(composites) < min_obs) {
        stop(
            "Fewer than `min_obs` observations are available ",
            "within the requested period.",
            call. = FALSE
        )
    }

    indices <- lapply(
        composites,
        function(x) {

            if (!inherits(x, "SpatRaster")) {
                stop(
                    "All composites must be SpatRaster objects.",
                    call. = FALSE
                )
            }

            switch(
                index,
                nbr = calc_nbr(x),
                ndvi = calc_ndvi(x),
                ndmi = calc_ndmi(x),
                msi = calc_msi(x)
            )
        }
    )

    # Align index rasters to the first observation if required.
    template <- indices[[1]]

    for (i in seq_along(indices)) {

        if (!terra::compareGeom(
            indices[[i]],
            template,
            stopOnError = FALSE
        )) {

            indices[[i]] <- terra::resample(
                indices[[i]],
                template,
                method = "bilinear"
            )
        }
    }

    stack <- indices[[1]]

    if (length(indices) > 1) {

        for (i in 2:length(indices)) {
            stack <- c(
                stack,
                indices[[i]]
            )
        }
    }

    days <- as.numeric(
        dates - min(dates)
    )

    trend_fun <- function(values) {

        valid <- is.finite(values)

        if (sum(valid) < min_obs) {
            return(NA_real_)
        }

        y <- values[valid]
        x <- days[valid]

        if (length(unique(x)) < 2) {
            return(NA_real_)
        }

        unname(
            stats::coef(
                stats::lm(
                    y ~ x
                )
            )[2]
        )
    }

    trend <- terra::app(
        stack,
        trend_fun
    )

    names(trend) <- paste0(
        index,
        "_trend"
    )

    trend
}

#' Calculate seasonal baseline
#' @param x Spatial raster
#' @param method Summary - either mean or median
#' @param min_years No yaers constituting baseline
#' @export
#' @return raster,
seasonal_baseline <- function(
        x,
        method = c("median", "mean"),
        min_years = 5
) {

    method <- match.arg(method)

    if (!is.list(x) || length(x) == 0) {
        stop(
            "`x` must be a non-empty list of annual SpatRaster objects.",
            call. = FALSE
        )
    }

    if (!all(vapply(
        x,
        inherits,
        logical(1),
        what = "SpatRaster"
    ))) {
        stop(
            "All elements of `x` must be SpatRaster objects.",
            call. = FALSE
        )
    }

    if (!is.numeric(min_years) ||
        length(min_years) != 1L ||
        is.na(min_years) ||
        min_years < 1) {
        stop(
            "`min_years` must be a positive integer.",
            call. = FALSE
        )
    }

    template <- x[[1]]

    x <- lapply(
        x,
        function(r) {

            if (!terra::compareGeom(
                r,
                template,
                stopOnError = FALSE
            )) {
                r <- terra::resample(
                    r,
                    template,
                    method = "bilinear"
                )
            }

            r
        }
    )

    stack <- terra::rast(x)

    n_years <- terra::app(
        stack,
        function(v) sum(!is.na(v))
    )

    baseline <- switch(
        method,
        median = terra::app(
            stack,
            median,
            na.rm = TRUE
        ),
        mean = terra::app(
            stack,
            mean,
            na.rm = TRUE
        )
    )

    baseline[n_years < min_years] <- NA

    names(baseline) <- "baseline"
    names(n_years) <- "n_years"

    list(
        baseline = baseline,
        n_years = n_years,
        method = method,
        min_years = min_years
    )
}

#' Build a multi-year seasonal baseline
#'
#' Combines annual seasonal index rasters into a historical baseline.
#'
#' @param x Named or unnamed list of single-layer `SpatRaster` objects,
#'   normally one raster per year.
#' @param method Summary statistic used across years: `"median"` or `"mean"`.
#' @param min_years Minimum number of valid years required for a pixel.
#'
#' @return A list containing `baseline`, `n_years`, `method`, and `min_years`.
#'
#' @export
seasonal_baseline <- function(
        x,
        method = c("median", "mean"),
        min_years = 5
) {
    method <- match.arg(method)

    if (!is.list(x) || length(x) == 0) {
        stop(
            "`x` must be a non-empty list of SpatRaster objects.",
            call. = FALSE
        )
    }

    if (!all(vapply(
        x,
        inherits,
        logical(1),
        what = "SpatRaster"
    ))) {
        stop(
            "All elements of `x` must be SpatRaster objects.",
            call. = FALSE
        )
    }

    if (!is.numeric(min_years) ||
        length(min_years) != 1L ||
        is.na(min_years) ||
        min_years < 1) {
        stop(
            "`min_years` must be a positive number.",
            call. = FALSE
        )
    }

    if (any(vapply(x, terra::nlyr, numeric(1)) != 1)) {
        stop(
            "Each element of `x` must contain exactly one layer.",
            call. = FALSE
        )
    }

    template <- x[[1]]

    x <- lapply(
        x,
        function(r) {
            if (!terra::compareGeom(
                r,
                template,
                stopOnError = FALSE
            )) {
                r <- terra::resample(
                    r,
                    template,
                    method = "bilinear"
                )
            }

            r
        }
    )


    stack <- terra::rast(x)

    if (terra::nlyr(stack) == 1) {

        n_years <- terra::ifel(
            is.na(stack),
            0,
            1
        )

        baseline <- stack

    } else {

        n_years <- terra::app(
            stack,
            function(v) {
                sum(!is.na(v))
            }
        )

        baseline <- switch(
            method,
            median = terra::app(
                stack,
                median,
                na.rm = TRUE
            ),
            mean = terra::app(
                stack,
                mean,
                na.rm = TRUE
            )
        )
    }

    baseline[n_years < min_years] <- NA

    names(baseline) <- "baseline"
    names(n_years) <- "n_years"

    list(
        baseline = baseline,
        n_years = n_years,
        method = method,
        min_years = min_years
    )
}

#' Calculate an index anomaly from a historical baseline
#'
#' Calculates the difference between a current index raster and a
#' historical seasonal baseline.
#'
#' @param current Single-layer `SpatRaster` containing the current index.
#' @param baseline Either a single-layer `SpatRaster` or the object returned
#'   by `seasonal_baseline()`.
#'
#' @return A single-layer `SpatRaster`. Negative values indicate values below
#'   the historical baseline.
#'
#' @export
index_anomaly <- function(
        current,
        baseline
) {
    if (!inherits(current, "SpatRaster")) {
        stop(
            "`current` must be a SpatRaster.",
            call. = FALSE
        )
    }

    if (terra::nlyr(current) != 1L) {
        stop(
            "`current` must contain exactly one layer.",
            call. = FALSE
        )
    }

    if (is.list(baseline) &&
        !is.null(baseline$baseline)) {
        baseline <- baseline$baseline
    }

    if (!inherits(baseline, "SpatRaster")) {
        stop(
            "`baseline` must be a SpatRaster or seasonal_baseline object.",
            call. = FALSE
        )
    }

    if (terra::nlyr(baseline) != 1L) {
        stop(
            "`baseline` must contain exactly one layer.",
            call. = FALSE
        )
    }

    if (!terra::compareGeom(
        current,
        baseline,
        stopOnError = FALSE
    )) {
        current <- terra::resample(
            current,
            baseline,
            method = "bilinear"
        )
    }

    out <- current - baseline
    names(out) <- "anomaly"

    out
}


# index anomaly -----------------------------------------------------------


# seasonal baseline -------------------------------------------------------


# keep collection acquisitions --------------------------------------------

keep_collection_acquisitions <- function(search, collection) {

    f <- collection$files |>
        dplyr::distinct(date, satellite)

    out <- lapply(
        seq_len(nrow(f)),
        function(i) {
            keep_acquisition(
                search,
                date = as.character(f$date[i]),
                satellite = f$satellite[i]
            )
        }
    )

    # combine the STAC item collections
    items <- do.call(c, lapply(out, `[[`, "items"))

    result <- search
    result$items <- items

    result
}

index_rasters <- function(
        composites,
        index = c(
            "nbr",
            "ndvi",
            "ndmi",
            "msi"
        ),
        boundary = NULL
) {

    index <- match.arg(index)

    if (!is.list(composites) ||
        length(composites) == 0) {

        stop(
            "`composites` must be a non-empty list.",
            call. = FALSE
        )
    }

    if (is.null(names(composites)) ||
        any(names(composites) == "")) {

        stop(
            "Names of `composites` must be valid dates.",
            call. = FALSE
        )
    }

    dates <- as.Date(
        names(composites)
    )

    if (anyNA(dates)) {

        stop(
            "Names of `composites` must be valid dates.",
            call. = FALSE
        )
    }

    out <- lapply(
        composites,
        function(x) {

            if (!inherits(x, "SpatRaster")) {
                stop(
                    "All composites must be SpatRaster objects.",
                    call. = FALSE
                )
            }

            idx <- calculate_index(
                x,
                index
            )

            if (!is.null(boundary)) {

                b <- aoi_to_spatvector(
                    boundary
                )

                if (!terra::same.crs(b, idx)) {
                    b <- terra::project(
                        b,
                        terra::crs(idx)
                    )
                }

                idx <- terra::crop(
                    idx,
                    b
                )

                idx <- terra::mask(
                    idx,
                    b
                )
            }

            idx
        }
    )

    names(out) <- names(composites)

    out
}


# select seasonal baseline ------------------------------------------------

select_seasonal_baseline <- function(
        x,
        current,
        window_days = 30
) {

    dates <- as.Date(names(x))
    current <- as.Date(current)

    if (anyNA(dates) || is.na(current)) {
        stop(
            "Dates must be valid.",
            call. = FALSE
        )
    }

    # Difference in day-of-year, allowing for year boundary
    doy <- as.integer(format(dates, "%j"))
    current_doy <- as.integer(
        format(current, "%j")
    )

    distance <- abs(doy - current_doy)

    distance <- pmin(
        distance,
        365 - distance
    )

    keep <-
        distance <= window_days &
        format(dates, "%Y") !=
        format(current, "%Y")

    x[keep]
}

