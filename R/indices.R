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

