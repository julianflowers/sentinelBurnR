#' Calculate differenced Normalized Burn Ratio
#'
#' @param pre Pre-fire NBR raster.
#' @param post Post-fire NBR raster.
#'
#' @return A SpatRaster.
#'
#' @export
calc_dnbr <- function(
        pre,
        post
) {

    stopifnot(
        inherits(pre, "SpatRaster"),
        inherits(post, "SpatRaster")
    )

    terra::compareGeom(
        pre,
        post,
        stopOnError = TRUE
    )

    dnbr <- pre - post

    names(dnbr) <- "dnbr"

    dnbr

}
