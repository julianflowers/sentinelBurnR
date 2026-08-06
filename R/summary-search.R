#' Summarise a Sentinel-2 search
#'
#' Returns scene-level metadata from a Sentinel-2 catalogue search.
#'
#' @param object An `sbr_search` object.
#' @param ... Additional arguments, currently unused.
#'
#' @return A data frame with one row per Sentinel-2 scene.
#'
#' @export
summary.sbr_search <- function(object, ...) {

    stopifnot(
        inherits(object, "sbr_search")
    )

    s2_item_metadata(
        object$items
    )
}
