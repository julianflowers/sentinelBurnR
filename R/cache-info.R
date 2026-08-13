#' Cache information
#'
#' @export

sbr_cache_info <- function() {

    root <- cache_path()

    dirs <- c(

        raw = cache_raw(),

        downloads = cache_downloads(),

        temp = cache_temp()

    )

    size <- function(path) {

        if (!dir.exists(path))
            return(0)

        files <- list.files(
            path,
            recursive = TRUE,
            full.names = TRUE
        )

        if (length(files) == 0)
            return(0)

        sum(
            file.info(files)$size,
            na.rm = TRUE
        )

    }

    sizes <- vapply(
        dirs,
        size,
        numeric(1)
    )

    total <- sum(sizes)

    cat("\n")

    cat("---------------------------------\n")

    cat("sentinelBurnR cache\n\n")

    for (nm in names(sizes)) {

        cat(

            sprintf(

                "%-12s %7.1f MB\n",

                nm,

                sizes[[nm]] /
                    1024^2

            )

        )

    }

    cat("\n")

    cat(

        sprintf(

            "%-12s %7.1f MB\n",

            "TOTAL",

            total /
                1024^2

        )

    )

    cat("---------------------------------\n")

    invisible(sizes)

}
