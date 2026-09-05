#' Read an area of interest
#'
#' Reads an area of interest from a vector file, an `sf` object, an
#' `sfc` object, or a `terra::SpatVector`.
#'
#' Multiple features are optionally combined into a single geometry,
#' and invalid geometries are repaired where possible.
#'
#' @param aoi A filename, `sf`, `sfc`, or `terra::SpatVector` object.
#' @param layer Optional layer name when reading a multi-layer file such
#'   as a GeoPackage.
#' @param dissolve Logical. If `TRUE`, combine all features into one AOI.
#' @param make_valid Logical. If `TRUE`, attempt to repair invalid geometry.
#' @param target_crs Optional output coordinate reference system accepted
#'   by `terra::project()`, for example `"EPSG:27700"`.
#'
#' @return A `terra::SpatVector`.
#'
#' @examples
#' \dontrun{
#' aoi <- read_aoi("study_area.gpkg")
#'
#' aoi_bng <- read_aoi(
#'   "study_area.gpkg",
#'   target_crs = "EPSG:27700"
#' )
#' }
#'
#' @export

read_aoi <- function(
        aoi,
        layer = NULL,
        dissolve = TRUE,
        make_valid = TRUE,
        target_crs = NULL
) {

    result <- aoi_to_spatvector(
        aoi = aoi,
        layer = layer
    )

    if (nrow(result) == 0) {
        stop(
            "`aoi` contains no features.",
            call. = FALSE
        )
    }

    current_crs <- terra::crs(
        result,
        proj = TRUE
    )

    if (
        is.na(current_crs) ||
        !nzchar(current_crs)
    ) {
        stop(
            "`aoi` does not have a coordinate reference system.",
            call. = FALSE
        )
    }

    if (make_valid) {
        result <- terra::makeValid(result)
    }

    if (dissolve && nrow(result) > 1) {
        result <- terra::aggregate(result)
    }

    if (!is.null(target_crs)) {
        result <- terra::project(
            result,
            target_crs
        )
    }

    new_aoi(result)
}

# aoi to SpatVector -------------------------------------------------------

# Convert supported AOI inputs to a SpatVector.
#
# @param aoi Input AOI.
# @param layer Optional vector layer name.
#
# @return A terra SpatVector.
#
# @keywords internal

aoi_to_spatvector <- function(
        aoi,
        layer = NULL
) {

    if (inherits(aoi, "sbr_aoi")) {
        return(aoi$geometry)
    }

    if (inherits(aoi, "SpatVector")) {
        return(aoi)
    }

    if (
        inherits(aoi, "sf") ||
        inherits(aoi, "sfc")
    ) {
        return(terra::vect(aoi))
    }

    if (
        is.character(aoi) &&
        length(aoi) == 1L &&
        !is.na(aoi)
    ) {

        if (!file.exists(aoi)) {
            stop(
                "AOI file does not exist: ",
                aoi,
                call. = FALSE
            )
        }

        if (is.null(layer)) {
            return(terra::vect(aoi))
        }

        return(
            terra::vect(
                aoi,
                layer = layer
            )
        )
    }

    stop(
        paste(
            "`aoi` must be a vector filename,",
            "an sf/sfc object, or a terra SpatVector."
        ),
        call. = FALSE
    )
}

#------ create aoi ---------------------------
#' Create an Area of Interest
#'
#' Create an Area of Interest (AOI) from either a bounding box or
#' a point and radius.
#'
#' @param xmin,xmax,ymin,ymax Bounding box coordinates.
#' @param lon,lat Longitude and latitude of the centre point.
#' @param radius Radius (metres) around the centre point.
#' @param crs Coordinate reference system of the input coordinates.
#'
#' @return An sbr_aoi object.
#' @examples
#' \dontrun{
#' aoi <- create_aoi(
#'     xmin = 1.61,
#'     ymin = 52.24,
#'     xmax = 1.64,
#'     ymax = 52.26
#' )
#' }
#' @export
create_aoi <- function(

    xmin = NULL,
    xmax = NULL,
    ymin = NULL,
    ymax = NULL,

    lon = NULL,
    lat = NULL,
    radius = NULL,

    crs = "EPSG:4326"

) {

    ## ------------------------------------------------------------
    ## Point + radius
    ## ------------------------------------------------------------

    if (!is.null(lon) &&
        !is.null(lat) &&
        !is.null(radius)) {

        pt <- terra::vect(

            matrix(

                c(lon, lat),

                ncol = 2

            ),

            type = "points",

            crs = crs

        )

        pt <- terra::project(

            pt,

            "EPSG:3857"

        )

        aoi <- terra::buffer(

            pt,

            width = radius

        )

        aoi <- terra::project(

            aoi,

            crs

        )


        return(new_aoi(aoi))

    }



    ## ------------------------------------------------------------
    ## Bounding box
    ## ------------------------------------------------------------

    if (!is.null(xmin) &&
        !is.null(xmax) &&
        !is.null(ymin) &&
        !is.null(ymax)) {

        coords <- matrix(

            c(

                xmin, ymin,

                xmax, ymin,

                xmax, ymax,

                xmin, ymax,

                xmin, ymin

            ),

            byrow = TRUE,

            ncol = 2

        )

        aoi <- terra::vect(

            coords,

            type = "polygons",

            crs = crs

        )

        return(new_aoi(aoi)

        )


    }

    stop(

        paste(

            "Supply either",

            "(xmin, xmax, ymin, ymax)",

            "or",

            "(lon, lat, radius)."

        ),

        call. = FALSE

    )

}

