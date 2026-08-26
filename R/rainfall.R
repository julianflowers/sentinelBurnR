#==========================================================
# Retrieve daily precipitation
#==========================================================
#' Retrieve daily precipitation from ERA5.
#'
#' Downloads (or reuses cached) ERA5 daily precipitation data for a
#' boundary and returns the mean daily precipitation over the area in
#' millimetres.
#' @param boundary Boundary polygon.
#' @param start,end Date range.
#' @param source Climate source.
#' @examples
#' \dontrun{
#'
#' burn <- detect_burns(
#'     pre = pre,
#'     post = post,
#'     boundary = boundary
#' )
#'
#' rain <- get_rainfall(
#'     boundary = boundary,
#'     start = "2024-07-01",
#'     end = "2024-07-31"
#' )
#'
#' head(rain)
#'
#' }
#' @return A data.frame with daily precipitation (mm).
#' @export


get_rainfall <- function(
        boundary,
        start,
        end,
        source = "era5"
) {

    boundary <- read_boundary(boundary)

    files <- download_climate(
        boundary = boundary,
        start = start,
        end = end,
        source = source
    )

    r <- read_climate(files)

    boundary <- prepare_boundary(
        boundary,
        r
    )

    vals <- terra::extract(
        r,
        boundary,
        fun = mean,
        na.rm = TRUE
    )

    vals <- vals[, -1, drop = FALSE]
    out <- data.frame(

        date = terra::time(r),

        precipitation_mm =
            as.numeric(vals[1, ]) * 1000,

        stringsAsFactors = FALSE

    )

    out <- out[
        out$date >= as.Date(start) &
            out$date <= as.Date(end),
    ]

    class(out) <- c(
        "sbr_rainfall",
        class(out)
    )

    attr(
        out,
        "source"
    ) <- source

    attr(
        out,
        "units"
    ) <- "mm"

    attr(
        out,
        "boundary"
    ) <- boundary

    out

}


read_climate <- function(files) {

    terra::rast(files)

}

#' @export
print.sbr_rainfall <- function(x, ...) {

    cat("\n")
    cat("sentinelBurnR rainfall\n")
    cat("----------------------\n")
    cat(sprintf("Days        : %d\n", nrow(x)))
    cat(sprintf("Total rain  : %.1f mm\n", sum(x$precipitation_mm, na.rm = TRUE)))
    cat(sprintf("Mean/day    : %.2f mm\n", mean(x$precipitation_mm, na.rm = TRUE)))
    cat(sprintf("Maximum day : %.1f mm\n", max(x$precipitation_mm, na.rm = TRUE)))
    cat("\n")

    print.data.frame(
        utils::head(x),
        row.names = FALSE
    )

    invisible(x)
}

