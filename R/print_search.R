#' @export
print.sbr_search <- function(x, ...) {

    features <- x$items$features
    n <- length(features)

    dates <- if (n > 0) {
        as.Date(vapply(
            features,
            function(item) {
                substr(item$properties$datetime, 1, 10)
            },
            character(1)
        ))
    } else {
        as.Date(character())
    }

    cloud <- if (n > 0) {
        vapply(
            features,
            function(item) {
                value <- item$properties[["eo:cloud_cover"]]

                if (is.null(value)) {
                    NA_real_
                } else {
                    as.numeric(value)
                }
            },
            numeric(1)
        )
    } else {
        numeric()
    }

    cat("\n<sentinelBurnR Sentinel-2 search>\n\n")
    cat("Requested period :", format(x$start), "to", format(x$end), "\n")
    cat("Scenes found     :", n, "\n")

    if (n > 0) {
        cat(
            "Acquisition dates:",
            format(min(dates, na.rm = TRUE)),
            "to",
            format(max(dates, na.rm = TRUE)),
            "\n"
        )

        if (any(!is.na(cloud))) {
            cat(
                "Cloud cover     :",
                round(min(cloud, na.rm = TRUE), 1),
                "to",
                round(max(cloud, na.rm = TRUE), 1),
                "%\n"
            )
        }
    }

    invisible(x)
}
