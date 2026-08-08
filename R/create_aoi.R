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
#'
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
