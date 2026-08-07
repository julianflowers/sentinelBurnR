#' @export
print.sbr_collection <- function(x, ...) {

    files <- x$files

    cat("\n<sentinelBurnR Collection>\n\n")

    cat(
        "Scenes :",
        length(unique(files$scene)),
        "\n"
    )

    cat(
        "Files  :",
        nrow(files),
        "\n"
    )

    cat(
        "Assets :",
        paste(
            unique(files$asset),
            collapse = ", "
        ),
        "\n"
    )

    invisible(x)

}
