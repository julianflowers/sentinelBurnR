#' @export
print.sbr_aoi <- function(x, ...) {

    cat("\n")
    cat("<sentinelBurnR AOI>\n")

    geom <- x$geometry

    cat(
        "Features :",
        nrow(geom),
        "\n"
    )

    cat(
        "CRS      :",
        terra::crs(geom),
        "\n"
    )

    invisible(x)

}
