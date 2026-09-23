#' Return the geometry from an AOI
#'
#' @param x An sbr_aoi object.
#'
#' @return A terra SpatVector.
#'
#' @export
geometry <- function(x) {
    UseMethod("geometry")
}

#' @export
geometry.sbr_aoi <- function(x) {

    stopifnot(
        inherits(x, "sbr_aoi")
    )

    aoi_geometry(x)
}


