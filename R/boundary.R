#' Read a boundary
#'
#' Reads a vector boundary from disk or converts an existing spatial
#' object to a terra::SpatVector.
#'
#' @param x A file path, an sf object or a terra::SpatVector.
#'
#' @return A terra::SpatVector.
#' @param template Optional raster used to define the output CRS.
#' @export
read_boundary <- function(
        x,
        template = NULL
) {

    boundary <- UseMethod(
        "read_boundary"
    )

    if (!is.null(template) &&
        !terra::same.crs(
            boundary,
            template
        )) {

        boundary <- terra::project(
            boundary,
            terra::crs(template)
        )

    }

    boundary

}

#' @export
read_boundary.character <- function(x, template = NULL) {

    terra::vect(x)

}

#' @export
read_boundary.SpatVector <- function(x, template = NULL) {

    x

}

#' @export
read_boundary.sf <- function(x, template = NULL) {

    terra::vect(x)

}

#' @export
read_boundary.default <- function(x, template = NULL) {

    stop(
        "`x` must be a file path, sf object or SpatVector.",
        call. = FALSE
    )

}

#-----------------------------------------
# Prepare Boundary
#-----------------------------------------
prepare_boundary <- function(
        boundary,
        template
) {

    boundary <- read_boundary(
        boundary
    )

    if (!terra::same.crs(
        boundary,
        template
    )) {

        boundary <- terra::project(
            boundary,
            terra::crs(template)
        )
    }

    boundary
}


#-----------------------------------------
# Create bounding box
#-----------------------------------------
boundary_bbox <- function(boundary) {

    boundary <- read_boundary(boundary)

    boundary <- terra::project(
        boundary,
        "EPSG:4326"
    )

    e <- terra::ext(boundary)

    c(
        e$ymax,
        e$xmin,
        e$ymin,
        e$xmax
    )

}

era5_bbox <- function(boundary) {

    expand_bbox(
        boundary_bbox(boundary),
        min_width = 0.5,
        min_height = 0.5
    )

}
