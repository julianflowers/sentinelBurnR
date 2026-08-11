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

#' @export
print.sbr_result <- function(
        x,
        ...
) {

    cat("\n")

    cat("sentinelBurnR result\n")

    cat("--------------------\n\n")

    cat(
        "Pre scenes : ",
        length(
            unique(
                files(
                    x$pre_collection
                )$scene
            )
        ),
        "\n",
        sep = ""
    )

    cat(
        "Post scenes: ",
        length(
            unique(
                files(
                    x$post_collection
                )$scene
            )
        ),
        "\n",
        sep = ""
    )

    cat(
        "Bands      : ",
        names(
            x$pre_composite
        ),
        "\n",
        sep = " "
    )

    invisible(x)

}

#' @export

print.sbr_project <- function(x, ...) {

    cat(
        "\n<sentinelBurnR project>\n\n",
        "Name : ", x$name, "\n",
        "Path : ", x$path, "\n\n",
        sep = ""
    )

    invisible(x)
}
