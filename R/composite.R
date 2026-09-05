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
        assets = s2_burn_assets,
        cache = TRUE,
        overwrite = FALSE
) {

    if (!inherits(collection, "sbr_collection")) {

        stop(
            "`collection` must be an sbr_collection.",
            call. = FALSE
        )
    }

    cache_file <- NULL

    if (cache) {

        cache_file <- composite_cache_file(
            collection,
            assets
        )

        if (
            file.exists(cache_file) &&
            !overwrite
        ) {

            message("Using cached composite")

            return(
                terra::rast(cache_file)
            )
        }
    }

    band_stacks <- stats::setNames(
        lapply(assets, function(asset) read_band(collection, asset)),
        assets
    )

    scl_masks <- NULL
    asset_table <- files(collection)

    if ("scl" %in% asset_table$asset && any(assets != "scl")) {
        scl_stacks <- if ("scl" %in% assets) {
            band_stacks[["scl"]]
        } else {
            read_band(collection, "scl")
        }

        scl_masks <- prepare_scl_masks(
            scl_stacks = scl_stacks,
            band_stacks = band_stacks[assets != "scl"]
        )
    }

    band_rasters <- stats::setNames(vector("list", length(assets)), assets)

    for (asset in assets) {

        message("Building ", asset)

        band_rasters[[asset]] <- build_band(
            collection = collection,
            asset = asset,
            stacks = band_stacks[[asset]],
            scl_masks = scl_masks[[asset]]
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

    if (cache) {

        message(
            "Caching composite: ",
            cache_file
        )

        terra::writeRaster(
            composite,
            cache_file,
            overwrite = TRUE
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
        stacks = read_band(collection, asset),
        scl_masks

) {

    if (missing(scl_masks)) {
        has_scl <- "scl" %in% files(collection)$asset
        scl_masks <- NULL

        if (asset != "scl" && has_scl) {
            one_band <- stats::setNames(list(stacks), asset)
            scl_masks <- prepare_scl_masks(
                scl_stacks = read_band(collection, "scl"),
                band_stacks = one_band
            )[[asset]]
        }
    }

    if (asset == "scl") {

        tile_composites <- lapply(
            stacks,
            median_stack
        )

    } else {

        tile_composites <- vector(
            "list",
            length(stacks)
        )

        names(tile_composites) <- names(stacks)

        for (i in seq_along(stacks)) {

            tile <- names(stacks)[[i]]

            if (!is.null(scl_masks)) {

                masked <- mask_scl(
                    stacks[[i]],
                    scl_masks[[tile]]
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


# Prepare SCL masks -------------------------------------------------------

prepare_scl_masks <- function(
        scl_stacks,
        band_stacks,
        keep = s2_scl_keep
) {

    stopifnot(is.list(scl_stacks), is.list(band_stacks))

    prepared <- stats::setNames(
        vector("list", length(band_stacks)),
        names(band_stacks)
    )
    cache <- list()
    n_prepared <- 0L

    for (asset in names(band_stacks)) {
        prepared[[asset]] <- list()

        for (tile in names(band_stacks[[asset]])) {
            image <- band_stacks[[asset]][[tile]]
            scl <- scl_stacks[[tile]]

            if (is.null(scl)) {
                stop("No SCL stack is available for tile '", tile, "'.",
                     call. = FALSE)
            }

            tile_cache <- cache[[tile]]
            match_index <- integer()

            if (length(tile_cache)) {
                matches <- vapply(
                    tile_cache,
                    function(x) terra::compareGeom(
                        image,
                        x$template,
                        lyrs = FALSE,
                        stopOnError = FALSE
                    ),
                    logical(1)
                )
                match_index <- which(matches)[1]
            }

            if (length(match_index) == 0L || is.na(match_index)) {
                mask <- prepare_scl_mask(scl, image, keep = keep)
                tile_cache[[length(tile_cache) + 1L]] <- list(
                    template = image,
                    mask = mask
                )
                match_index <- length(tile_cache)
                cache[[tile]] <- tile_cache
                n_prepared <- n_prepared + 1L
            }

            prepared[[asset]][[tile]] <- tile_cache[[match_index]]$mask
        }
    }

    attr(prepared, "n_prepared") <- n_prepared
    prepared
}


prepare_scl_mask <- function(
        scl,
        template,
        keep = s2_scl_keep
) {

    stopifnot(
        inherits(scl, "SpatRaster"),
        inherits(template, "SpatRaster")
    )

    if (!terra::compareGeom(
        scl,
        template,
        lyrs = FALSE,
        stopOnError = FALSE
    )) {
        scl <- terra::resample(scl, template, method = "near")
    }

    Reduce(
        `|`,
        lapply(keep, function(value) scl == value)
    )
}


#------------mask scl-------------------------------------

mask_scl <- function(
        image,
        mask
) {

    stopifnot(
        inherits(image, "SpatRaster")
    )

    stopifnot(
        inherits(mask, "SpatRaster")
    )

    if (!terra::compareGeom(
        image,
        mask,
        lyrs = FALSE,
        stopOnError = FALSE
    )) {
        stop("`mask` must already match the geometry of `image`.",
             call. = FALSE)
    }

    terra::mask(image, mask, maskvalues = 0)

}


#----------align bands------------------------------------


align_bands <- function(
        rasters,
        reference = "red",
        method = "bilinear"
) {

    if (length(rasters) == 0) {
        return(rasters)
    }

    if (!reference %in% names(rasters)) {

        resolutions <- vapply(
            rasters,
            function(x) {
                prod(terra::res(x))
            },
            numeric(1)
        )

        reference <- names(rasters)[
            which.min(resolutions)
        ]
    }

    template <- rasters[[reference]]

    out <- rasters

    for (nm in names(rasters)) {

        if (nm == reference) {
            next
        }

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


