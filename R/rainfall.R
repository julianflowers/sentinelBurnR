#' Extract rainfall time series
#'
#' Extract the mean daily precipitation for a boundary from an ERA5
#' precipitation raster.
#'
#' @param climate A SpatRaster returned by \code{read_climate()}.
#' @param boundary Boundary polygon.
#' @param fun Summary function used when extracting rainfall.
#' @return An object of class \code{sbr_rainfall}.
#'
#' @export

extract_rainfall <- function(
        climate,
        boundary,
        fun = mean) {

    stopifnot(
        inherits(
            climate,
            "SpatRaster"
        )
    )

    boundary <- prepare_boundary(
        boundary,
        climate
    )

    vals <- terra::extract(
        climate,
        boundary,
        fun = fun,
        na.rm = TRUE
    )

    vals <- vals[, -1, drop = FALSE]

    out <- data.frame(

        date = as.Date(
            terra::time(climate)
        ),

        precipitation_mm =
            as.numeric(vals[1, ]) * 1000,

        stringsAsFactors = FALSE

    )

    class(out) <- c(
        "sbr_rainfall",
        "data.frame"
    )

    attr(out, "units") <- "mm"

    out

}



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

    climate <- read_climate(files)


    out <- extract_rainfall(
        climate,
        boundary
    )

    out <- out[
        out$date >= as.Date(start) &
            out$date <= as.Date(end),
    ]

    attr(out, "source") <- source
    attr(out, "boundary") <- boundary





    out

}

#' Read climate data
#'
#' Read a climate raster from a NetCDF file.
#'
#' @param files Path to one or more NetCDF files.
#'
#' @return A SpatRaster.
#'
#' @export
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

