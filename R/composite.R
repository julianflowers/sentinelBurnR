#' Build a median Sentinel-2 composite
#'
#' @param collection An sbr_collection.
#' @param assets Assets to include.
#'
#' @return A SpatRaster.
#'
#' @export
#'
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

    if (!is.null(collection$aoi)) {

        aoi <- collection$aoi$geometry

        if (!terra::same.crs(aoi, composite)) {

            aoi <- terra::project(
                aoi,
                terra::crs(composite)
            )
        }

        composite <- terra::mask(
            composite,
            aoi
        )
    }

    composite
}


# read bands --------------------------------------------------------------
#' Read one asset from a downloaded Sentinel-2 collection
#'
#' Returns one SpatRaster stack per MGRS tile.
#'
#' @param collection An sbr_collection.
#' @param asset Sentinel-2 asset name.
#
#' @return A named list of SpatRaster objects, one per tile.
#' @export
read_band <- function(
        collection,
        asset
) {

    stopifnot(
        inherits(
            collection,
            "sbr_collection"
        )
    )

    x <- files(collection)

    x <- x[
        x$asset == asset,
        ,
        drop = FALSE
    ]

    if (nrow(x) == 0) {
        stop(
            "Asset '",
            asset,
            "' is not present in this collection.",
            call. = FALSE
        )
    }

    if (!"tile" %in% names(x)) {
        stop(
            "Collection metadata does not contain a `tile` column.",
            call. = FALSE
        )
    }

    groups <- split(
        x,
        x$tile
    )

    result <- lapply(
        groups,
        function(tile_files) {

            tile_files <- tile_files[
                order(tile_files$date),
                ,
                drop = FALSE
            ]

            raster <- terra::rast(
                tile_files$file
            )

            names(raster) <- paste0(
                asset,
                "_",
                format(
                    tile_files$date,
                    "%Y%m%d"
                )
            )

            raster
        }
    )

    class(result) <- c(
        "sbr_band_stack",
        class(result)
    )

    result


}


#----build bands-----------------------------------

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


#------------mask scl-------------------------------------

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


#----------align bands------------------------------------

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

#----stack and mosaic------------------------------

median_stack <- function(x) {

    if (!inherits(x, "SpatRaster")) {
        stop(
            "x must be a terra::SpatRaster",
            call. = FALSE
        )
    }

    terra::app(
        x,
        stats::median,
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




