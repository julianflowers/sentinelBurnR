#' Count valid Sentinel-2 observations
#'
#' Counts acquisition dates with at least one SCL-valid observation.
#' Overlapping tiles from the same date count only once.
#'
#' @param collection An sbr_collection containing SCL files.
#' @param keep SCL classes treated as valid.
#' @param boundary Optional sf or SpatVector boundary.
#'
#' @return A SpatRaster named valid_count.
#' @export
valid_observation_count <- function(
        collection,
        keep = c(4L, 5L, 6L),
        boundary = NULL
) {

    stopifnot(inherits(collection, "sbr_collection"))

    f <- files(collection)
    f <- f[f$asset == "scl", , drop = FALSE]

    if (nrow(f) == 0L) {
        stop("Collection contains no SCL files.", call. = FALSE)
    }

    if (!is.null(boundary)) {
        boundary <- terra::vect(boundary)
    }

    dates <- sort(unique(f$date))

    date_masks <- lapply(dates, function(d) {

        fd <- f[f$date == d, , drop = FALSE]

        masks <- lapply(fd$file, function(path) {
            scl <- terra::rast(path)
            mask <- Reduce(
                `|`,
                lapply(keep, function(k) scl == k)
            )

            terra::ifel(mask, 1, 0)
        })

        # Align tile grids, preserving the union of their extents.
        reference <- masks[[1]]

        masks <- lapply(masks, function(x) {

            if (!terra::same.crs(x, reference)) {
                x <- terra::project(
                    x,
                    reference,
                    method = "near",
                    extend = TRUE
                )
            }

            x
        })



        # Merge same-date tiles: a date is valid if any tile is valid.
        Reduce(
            function(x, y) {terra::mosaic(x, y, fun = "max")},
            masks
        )
    })

    reference <- date_masks[[1]]

    date_masks <- lapply(date_masks, function(x) {
        terra::resample(x, reference, method = "near")
    })

    result <- terra::app(
        terra::rast(date_masks),
        sum,
        na.rm = TRUE
    )

    names(result) <- "valid_count"

    if (!is.null(boundary)) {
        if (!terra::same.crs(boundary, result)) {
            boundary <- terra::project(
                boundary,
                terra::crs(result)
            )
        }

        # Construct a common extent across all acquisition dates
        template <- Reduce(
            function(x, y) terra::extend(x, terra::ext(y)),
            date_masks
        )

        # Extend each date's raster to the common extent
        date_masks <- lapply(
            date_masks,
            function(x) terra::extend(x, template)
        )

        # Count valid acquisition dates
        result <- terra::app(
            terra::rast(date_masks),
            sum,
            na.rm = TRUE
        )

        names(result) <- "valid_count"
    }

    result
}
