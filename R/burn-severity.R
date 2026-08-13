#' Classify burn severity from dNBR
#'
#' Classifies a dNBR raster into burn severity classes.
#'
#' @param x A single-layer dNBR SpatRaster.
#'
#' @return A categorical SpatRaster containing burn severity classes.
#'
#' @export
classify_burn_severity <- function(x) {

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

    rcl <- matrix(
        c(
            -Inf, -0.25, 1,
            -0.25, -0.10, 2,
            -0.10,  0.10, 3,
            0.10,  0.27, 4,
            0.27,  0.44, 5,
            0.44,  0.66, 6,
            0.66,  Inf, 7
        ),
        ncol = 3,
        byrow = TRUE
    )

    severity <- terra::classify(
        x,
        rcl
    )

    names(severity) <- "severity"

    severity
}
