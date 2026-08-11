#' Build a median Sentinel-2 composite
#'
#' @param collection An sbr_collection.
#' @param assets Assets to include.
#'
#' @return A SpatRaster.
#'
#' @export

build_composite <- function(

    collection,

    assets = s2_burn_assets

) {

    if (!inherits(collection, "sbr_collection")) {

        stop(
            "`collection` must be an sbr_collection.",
            call. = FALSE
        )

    }

    band_rasters <- vector(
        "list",
        length(assets)
    )

    names(band_rasters) <- assets

    for (asset in assets) {

        message("Building ", asset)

        band_rasters[[asset]] <- build_band(

            collection = collection,

            asset = asset

        )

    }

    message("Aligning bands...")

    band_rasters <- align_bands(
        band_rasters
    )

    message("Stacking composite...")

    composite <- band_rasters[[1]]

    if (length(band_rasters) > 1) {

        for (nm in names(band_rasters)[-1]) {

            composite <- c(

                composite,

                band_rasters[[nm]]

            )

        }

    }

    names(composite) <- names(band_rasters)

    composite

}
