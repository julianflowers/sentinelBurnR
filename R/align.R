# Align rasters to a common grid
#
# @param rasters Named list of SpatRaster objects.
# @param reference Name of the reference raster.
#
# @return Named list of aligned rasters.

align_bands <- function(
        rasters,
        reference = "red",
        method = "bilinear"
) {

    template <- rasters[[reference]]

    out <- rasters

    for (nm in names(rasters)) {

        if (nm == reference)
            next

        out[[nm]] <- terra::resample(
            rasters[[nm]],
            template,
            method = method
        )

    }

    out

}
