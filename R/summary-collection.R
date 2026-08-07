#' Summarise a downloaded Sentinel-2 collection
#'
#' @param object An sbr_collection object.
#' @param ... Additional arguments (unused).
#'
#' @return A data frame describing the downloaded files.
#'
#' @export
summary.sbr_collection <- function(object, ...) {

    files(object)

}
