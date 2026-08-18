#' Return downloaded files
#'
#' @param x An sbr_collection.
#'
#' @return A data frame.
#'
#' @export
files <- function(x) {
    UseMethod("files")
}


#' Access collection files
#'
#' @param x An `sbr_collection`.
#' @param ... Not used.
#'
#' @export
files.sbr_collection <- function(x, ...) {
   x$files
}



#' Access AOI
#'
#' Returns the area of interest associated with an object.
#'
#' @param x An object.
#' @param ... Additional arguments passed to methods.
#'
#' @return An AOI object.
#'
#' @export
aoi <- function(x, ...) {
    UseMethod("aoi")
}


#' @export
aoi.sbr_collection <- function(x, ...) {
    x$aoi
}
