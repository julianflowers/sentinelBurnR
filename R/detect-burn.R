#' Detect burned area from dNBR
#'
#' Creates a burned / not-burned raster from a dNBR raster.
#'
#' @param x A single-layer dNBR SpatRaster.
#' @param threshold Numeric dNBR threshold above which cells are
#'   classified as burned.
#'
#' @return A single-layer SpatRaster named `burned`.
#'
#' @export
detect_burn <- function(
        x,
        threshold = 0.27
) {

    if (!inherits(x, "SpatRaster")) {
        stop(
            "`x` must be a SpatRaster.",
            call. = FALSE
        )
    }

    if (terra::nlyr(x) != 1L) {
        stop(
            "`x` must contain one layer.",
            call. = FALSE
        )
    }

    if (!is.numeric(threshold) ||
        length(threshold) != 1L ||
        is.na(threshold)) {

        stop(
            "`threshold` must be a single numeric value.",
            call. = FALSE
        )
    }

    burned <- x >= threshold

    names(burned) <- "burned"

    burned
}

burn_area <- function(x, unit = "ha") {

    if (!inherits(x, "SpatRaster")) {
        stop(
            "`x` must be a SpatRaster.",
            call. = FALSE
        )
    }

    area <- terra::cellSize(
        x,
        unit = unit
    )

    area <- terra::mask(
        area,
        x,
        maskvalues = 0
    )

    as.numeric(
        terra::global(
            area,
            "sum",
            na.rm = TRUE
        )[1, 1]
    )
}
