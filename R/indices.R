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
        band1 = "nir08",
        band2 = "swir22",
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
        band1 = "nir08",
        band2 = "red",
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
        band1 = "nir08",
        band2 = "swir22",
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
#'
#' @export
.normalised_difference <- function(
        x,
        band1,
        band2,
        name
) {

    if (!inherits(x, "SpatRaster")) {
        stop(
            "`x` must be a SpatRaster.",
            call. = FALSE
        )
    }

    required <- c(
        band1,
        band2
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

    a <- x[[band1]]
    b <- x[[band2]]

    out <- (a - b) / (a + b)

    names(out) <- name

    out
}

# get band ----------------------------------------------------------------

#' @export
get_band <- function(
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

