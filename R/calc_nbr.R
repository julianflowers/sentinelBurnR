#' Calculate the Normalized Burn Ratio
#'
#' @param x A composite SpatRaster containing
#'   "nir08" and "swir22".
#'
#' @return A single-layer SpatRaster.
#'
#' @export
calc_nbr <- function(x) {

    if (!inherits(x, "SpatRaster")) {
        stop(
            "`x` must be a SpatRaster.",
            call. = FALSE
        )
    }

    required <- c(
        "nir08",
        "swir22"
    )

    missing <- setdiff(
        required,
        names(x)
    )

    if (length(missing) > 0) {
        stop(
            "Missing required band(s): ",
            paste(missing, collapse = ", "),
            call. = FALSE
        )
    }

    nir <- x[["nir08"]]
    swir <- x[["swir22"]]

    nbr <- (
        nir - swir
    ) / (
        nir + swir
    )

    names(nbr) <- "nbr"

    nbr
}
