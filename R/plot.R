#' Plot an Area of Interest
#'
#' @param x An sbr_aoi object.
#' @param ... Additional arguments passed to terra::plot().
#'
#' @export

plot.sbr_aoi <- function(
        x,
        ...
) {

    terra::plot(
        x$geometry,
        ...
    )

}
