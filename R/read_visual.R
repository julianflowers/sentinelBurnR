#' Read Sentinel-2 true-colour imagery
#'
#' Reads the Sentinel-2 `visual` asset for a single acquisition,
#' crops the imagery to the collection AOI, and mosaics multiple
#' Sentinel-2 tiles where necessary.
#'
#' @param collection An `sbr_collection` containing the `visual` asset.
#' @param date Date of the acquisition to read. If the collection contains
#'   only one acquisition, this may be omitted.
#' @param aoi Area of interest
#' @return A three-layer `terra::SpatRaster` containing red, green and blue
#'   true-colour imagery.
#'
#' @export
read_visual <- function(
        collection,
        aoi,
        date = NULL
) {

    stopifnot(
        inherits(collection, "sbr_collection"),
        inherits(aoi, "sbr_aoi")
    )

    x <- files(collection)

    x <- x[
        x$asset == "visual",
        ,
        drop = FALSE
    ]

    if (nrow(x) == 0) {
        stop(
            "The collection does not contain the `visual` asset.",
            call. = FALSE
        )
    }

    dates <- sort(unique(x$date))

    if (is.null(date)) {

        if (length(dates) != 1) {
            stop(
                "`date` must be supplied when the collection contains ",
                "more than one acquisition.",
                call. = FALSE
            )
        }

        date <- dates[[1]]

    } else {

        date <- as.Date(date)

        if (!date %in% dates) {
            stop(
                "No `visual` imagery is available for ",
                format(date),
                ".",
                call. = FALSE
            )
        }
    }

    x <- x[
        x$date == date,
        ,
        drop = FALSE
    ]

    boundary <- geometry(aoi)

    rasters <- lapply(
        x$file,
        function(file) {

            r <- terra::rast(file)

            if (terra::nlyr(r) != 3) {
                stop(
                    "The Sentinel-2 `visual` asset must contain three bands.",
                    call. = FALSE
                )
            }

            boundary_r <- terra::project(
                boundary,
                terra::crs(r)
            )

            terra::crop(
                r,
                boundary_r,
                mask = TRUE
            )
        }
    )

    if (length(rasters) == 1) {

        out <- rasters[[1]]

    } else {

        out <- do.call(
            terra::mosaic,
            rasters
        )
    }

    names(out) <- c(
        "red",
        "green",
        "blue"
    )

    out
}
