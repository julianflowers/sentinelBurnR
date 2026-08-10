# Mask cloudy pixels using the Sentinel-2 Scene Classification Layer (SCL)
#
# @param image A SpatRaster containing one or more image layers.
# @param scl A SpatRaster containing the corresponding SCL layer.
# @param keep Integer vector of SCL classes to retain.
#
# @return A SpatRaster with unwanted pixels set to NA.
#
# @keywords internal

mask_scl <- function(
        image,
        scl,
        keep = s2_scl_keep
) {

    stopifnot(
        inherits(image, "SpatRaster")
    )

    stopifnot(
        inherits(scl, "SpatRaster")
    )

    if (
        nrow(image) != nrow(scl) ||
        ncol(image) != ncol(scl)
    ) {

        scl <- terra::resample(
            scl,
            image,
            method = "near"
        )

    }

    keep_mask <- Reduce(
        `|`,
        lapply(
            keep,
            function(x) scl == x
        )
    )

    out <- image

    out[!keep_mask] <- NA

    out

}
