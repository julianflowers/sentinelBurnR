#' Read a boundary
#'
#' Reads a vector boundary from disk or converts an existing spatial
#' object to a terra::SpatVector.
#'
#' @param x A file path, an sf object or a terra::SpatVector.
#'
#' @return A terra::SpatVector.
#'
#' @export
read_boundary <- function(x) {
    UseMethod("read_boundary")
}

#' @export
read_boundary.character <- function(x) {

    terra::vect(x)

}

#' @export
read_boundary.SpatVector <- function(x) {

    x

}

#' @export
read_boundary.sf <- function(x) {

    terra::vect(x)

}

#' @export
read_boundary.default <- function(x) {

    stop(
        "`x` must be a file path, sf object or SpatVector.",
        call. = FALSE
    )

}
