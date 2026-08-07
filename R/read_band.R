# Read one asset from a downloaded Sentinel-2 collection
#
# Returns one SpatRaster stack per MGRS tile.
#
# @param collection An sbr_collection.
# @param asset Sentinel-2 asset name.
#
# @return A named list of SpatRaster objects, one per tile.

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
