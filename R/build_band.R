#' Build bands
#'
#' @param collection An sbr_collection.
#' @param assets Assets to include.
#'
#' @return A SpatRaster.
#'
#' @export


build_band <- function(
        collection,
        asset,
        scl_stacks = NULL
) {

    stacks <- read_band(
        collection,
        asset
    )

    if (asset == "scl") {

        tile_composites <- lapply(
            stacks,
            median_stack
        )

    } else {

        has_scl <- "scl" %in% files(collection)$asset

        if (has_scl) {

            scl_stacks <- read_band(
                collection,
                "scl"
            )

        } else {

            scl_stacks <- NULL
        }

        tile_composites <- vector(
            "list",
            length(stacks)
        )

        for (i in seq_along(stacks)) {

            if (!is.null(scl_stacks)) {

                masked <- mask_scl(
                    stacks[[i]],
                    scl_stacks[[i]]
                )

            } else {

                masked <- stacks[[i]]
            }

            if (terra::nlyr(masked) == 1) {

                tile_composites[[i]] <- masked

            } else {

                tile_composites[[i]] <- median_stack(
                    masked
                )
            }
        }
    }

    # Crop individual tile composites to the AOI
    if (!is.null(collection$aoi)) {

        aoi <- collection$aoi$geometry

        # Transform AOI to the CRS of the Sentinel raster
        raster_crs <- terra::crs(
            tile_composites[[1]]
        )

        if (!terra::same.crs(aoi, tile_composites[[1]])) {

            aoi <- terra::project(
                aoi,
                raster_crs
            )
        }

        aoi_ext <- terra::ext(aoi)

        tile_composites <- lapply(
            tile_composites,
            function(x) {

                x_ext <- terra::ext(x)

                overlaps <-
                    x_ext$xmin < aoi_ext$xmax &&
                    x_ext$xmax > aoi_ext$xmin &&
                    x_ext$ymin < aoi_ext$ymax &&
                    x_ext$ymax > aoi_ext$ymin

                if (!overlaps) {
                    return(NULL)
                }

                terra::crop(
                    x,
                    aoi_ext
                )
            }
        )

        tile_composites <- Filter(
            Negate(is.null),
            tile_composites
        )
    }
    if (length(tile_composites) == 0L) {
        stop(
            "No Sentinel-2 tiles overlap the AOI.",
            call. = FALSE
        )
    }


    mosaic_tiles(
        tile_composites
    )
}
