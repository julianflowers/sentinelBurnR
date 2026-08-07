#' Download Sentinel-2 imagery
#'
#' @param x An sbr_search object.
#' @param assets Character vector of asset names.
#' @param output_dir Directory to store downloads.
#' @param overwrite Overwrite existing files?
#'
#' @return An sbr_collection object.
#'
#' @export
download_s2 <- function(
        x,
        assets = c("B04", "B08", "B8A", "B12"),
        output_dir = "data",
        overwrite = FALSE
) {

    stopifnot(
        inherits(x, "sbr_search")
    )

    dir.create(
        output_dir,
        recursive = TRUE,
        showWarnings = FALSE
    )

    stop(
        "Not implemented yet."
    )

}
