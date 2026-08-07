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

#' @export
files.sbr_collection <- function(x) {
    x$files
}
