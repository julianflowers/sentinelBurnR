# Median composite of a raster stack
#
# @param x A SpatRaster.
#
# @return A single-layer SpatRaster.

median_stack <- function(x) {

    if (!inherits(x, "SpatRaster")) {
        stop(
            "x must be a terra::SpatRaster",
            call. = FALSE
        )
    }

    terra::app(
        x,
        median,
        na.rm = TRUE
    )

}

mosaic_tiles <- function(tiles) {

    stopifnot(is.list(tiles))

    if (length(tiles) == 1)
        return(tiles[[1]])

    Reduce(
        function(x, y)
            terra::mosaic(x, y),
        tiles
    )
}
