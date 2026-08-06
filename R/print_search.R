#' @export
print.sbr_search <- function(x, ...) {

    metadata <- summary(x)
    scene_count <- nrow(metadata)

    cat("\n<sentinelBurnR Sentinel-2 search>\n\n")

    cat(
        "Requested period :",
        format(x$start),
        "to",
        format(x$end),
        "\n"
    )

    cat(
        "Scenes found     :",
        scene_count,
        "\n"
    )

    if (scene_count > 0L) {

        cat(
            "Acquisition dates:",
            format(min(metadata$date, na.rm = TRUE)),
            "to",
            format(max(metadata$date, na.rm = TRUE)),
            "\n"
        )

        if (any(!is.na(metadata$cloud_cover))) {

            cat(
                "Cloud cover     :",
                round(
                    min(metadata$cloud_cover, na.rm = TRUE),
                    1
                ),
                "to",
                round(
                    max(metadata$cloud_cover, na.rm = TRUE),
                    1
                ),
                "%\n"
            )
        }

        valid_tiles <- unique(
            metadata$tile[!is.na(metadata$tile)]
        )

        if (length(valid_tiles) > 0L) {

            cat(
                "MGRS tiles      :",
                paste(valid_tiles, collapse = ", "),
                "\n"
            )
        }
    }

    cat("\n")

    invisible(x)
}
