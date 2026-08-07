#' Calculate the Normalized Burn Ratio
#'
#' @param x A composite SpatRaster containing
#'   "nir08" and "swir22".
#'
#' @return A single-layer SpatRaster.
#'
#' @export
calc_nbr <- function(x) {

    stopifnot(
        inherits(x, "SpatRaster")
    )

    if (!all(c("nir08", "swir22") %in% names(x))) {
        stop(
            "Composite must contain 'nir08' and 'swir22' bands.",
            call. = FALSE
        )
    }

    nbr <-
        (x[["nir08"]] - x[["swir22"]]) /
        (x[["nir08"]] + x[["swir22"]])

    names(nbr) <- "nbr"

    nbr
}
