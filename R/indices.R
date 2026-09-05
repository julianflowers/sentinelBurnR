#---------------------------------------------------------
# NBR
#---------------------------------------------------------

#' Calculate the Normalized Burn Ratio
#'
#' @param x A SpatRaster containing `nir08` and `swir22`.
#'
#' @return A single-layer SpatRaster named `nbr`.
#'
#' @export
calc_nbr <- function(x) {

    .normalised_difference(
        x,
        numerator = "nir08",
        denominator = "swir22",
        name = "nbr"
    )

}

#---------------------------------------------------------
# NDVI
#---------------------------------------------------------

#' Calculate NDVI
#'
#' Calculates the Normalized Difference Vegetation Index.
#'
#' @param x A SpatRaster containing `nir08` and `red`.
#'
#' @return A single-layer SpatRaster named `ndvi`.
#'
#' @export
calc_ndvi <- function(x) {

    .normalised_difference(
        x,
        numerator = "nir08",
        denominator = "red",
        name = "ndvi"
    )

}

#---------------------------------------------------------
# NDMI
#---------------------------------------------------------

#' Calculate NDMI
#'
#' Calculates the Normalized Difference Moisture Index.
#'
#' @param x A SpatRaster containing `nir08` and `swir22`.
#'
#' @return A single-layer SpatRaster named `ndmi`.
#'
#' @export
calc_ndmi <- function(x) {

    .normalised_difference(
        x,
        numerator = "nir08",
        denominator = "swir16",
        name = "ndmi"
    )

}

# -----------------------------------------
# calculate normalised difference
# -----------------------------------------

#' Calculates Normalized Differences
#'
#' @param x A `SpatRaster` containing `band1` and `band2`.
#'
#' @return A single-layer `SpatRaster` named `ndvi`.
#' @param numerator Name of the numerator band.
#' @param denominator Name of the denominator band.
#' @param name Name of the output layer.
#' @export
.normalised_difference <- function(
        x,
        numerator,
        denominator,
        name
) {

    if (!inherits(x, "SpatRaster")) {
        stop(
            "`x` must be a SpatRaster.",
            call. = FALSE
        )
    }

    required <- c(
        numerator,
        denominator
    )

    missing <- setdiff(
        required,
        names(x)
    )

    if (length(missing) > 0L) {
        stop(
            "Missing required band(s): ",
            paste(missing, collapse = ", "),
            call. = FALSE
        )
    }

    a <- x[[numerator]]
    b <- x[[denominator]]

    out <- (a - b) / (a + b)

    names(out) <- name

    out
}

#-----------------------------------------------
# Calculate MSI
# ----------------------------------------------

calc_msi <- function(x) {

    stopifnot(
        inherits(x, "SpatRaster")
    )

    stopifnot(
        all(
            c("nir08", "swir16") %in%
                names(x)
        )
    )

    out <- x[["swir16"]] /
        x[["nir08"]]

    names(out) <- "msi"

    out
}

# get band ----------------------------------------------------------------

.get_band <- function(
        x,
        band
) {

    if (!inherits(x, "SpatRaster")) {
        stop(
            "`x` must be a terra::SpatRaster.",
            call. = FALSE
        )
    }

    if (!band %in% names(x)) {
        stop(
            "Band '",
            band,
            "' not found.",
            call. = FALSE
        )
    }

    x[[band]]

}


# summarise index --------------------------------------------------------

summarise_index <- function(x, index) {

    q <- terra::quantile(
        x,
        probs = c(
            0.05,
            0.25,
            0.5,
            0.75,
            0.95
        ),
        na.rm = TRUE
    )

    data.frame(
        index = index,
        mean = terra::global(
            x,
            "mean",
            na.rm = TRUE
        )[[1]],
        p05 = q[[1]],
        p25 = q[[2]],
        median = q[[3]],
        p75 = q[[4]],
        p95 = q[[5]]
    )

    summary <- rbind(
        summarise_index(ndvi, "NDVI"),
        summarise_index(ndmi, "NDMI"),
        summarise_index(msi, "MSI")
    )

    summary = summary
}

calculate_index <- function(
        x,
        index = c(
            "nbr",
            "ndvi",
            "ndmi",
            "msi"
        )
) {

    index <- match.arg(index)

    switch(
        index,
        nbr = calc_nbr(x),
        ndvi = calc_ndvi(x),
        ndmi = calc_ndmi(x),
        msi = calc_msi(x)
    )
}


# check raster geom -------------------------------------------------------

check_raster_geometry <- function(x) {

    if (!is.list(x) || length(x) < 2) {
        return(invisible(TRUE))
    }

    reference <- x[[1]]

    ok <- vapply(
        x[-1],
        function(r) {
            terra::compareGeom(
                reference,
                r,
                stopOnError = FALSE
            )
        },
        logical(1)
    )

    if (!all(ok)) {
        stop(
            "Rasters do not have matching geometry.",
            call. = FALSE
        )
    }

    invisible(TRUE)
}

