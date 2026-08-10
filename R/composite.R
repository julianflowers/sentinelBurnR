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

    band_rasters <- vector(
        "list",
        length(assets)
    )



    cat("\nAFTER ALIGNMENT\n")

    for (nm in names(band_rasters)) {

        cat("\n", nm, "\n")

        print(band_rasters[[nm]])

    }

    names(band_rasters) <- assets

    for (asset in assets) {

        message("Building ", asset)

        stacks <- read_band(
            collection,
            asset
        )

        if (asset == "scl") {

            #
            # Don't mask the SCL itself.
            #

            tile_composites <- lapply(

                stacks,

                median_stack

            )

        } else {

            scl_stacks <- read_band(

                collection,

                "scl"

            )

            tile_composites <- vector(

                "list",

                length(stacks)

            )

            for (i in seq_along(stacks)) {

                masked <- mask_scl(

                    stacks[[i]],

                    scl_stacks[[i]]

                )

                tile_composites[[i]] <-

                    median_stack(
                        masked
                    )

            }

        }

        band_rasters[[asset]] <-
            mosaic_tiles(
                tile_composites
            )

    }

band_rasters <- align_bands(
        band_rasters
    )

    #browser()

composite <- band_rasters[[1]]

for (nm in names(band_rasters)[-1]) {

    composite <- c(
        composite,
        band_rasters[[nm]]
    )

}

    names(composite) <- assets

    composite

}
