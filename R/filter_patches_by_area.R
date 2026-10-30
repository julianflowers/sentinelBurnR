
#' Filter raster patches by minimum area
#'
#' @param patches A single-layer terra SpatRaster containing patch IDs.
#' @param min_area_ha Minimum patch area in hectares. Defaults to zero.
#'
#' @return A SpatRaster with undersized patches set to NA.
#'   Original patch IDs are preserved.
#' @export
filter_patches_by_area <- function(
        patches,
        min_area_ha = 0
) {

    if (!inherits(patches, "SpatRaster") ||
        terra::nlyr(patches) != 1L) {
        stop(
            "`patches` must be a single-layer SpatRaster.",
            call. = FALSE
        )
    }

    if (!is.numeric(min_area_ha) ||
        length(min_area_ha) != 1L ||
        is.na(min_area_ha) ||
        !is.finite(min_area_ha) ||
        min_area_ha < 0) {
        stop(
            "`min_area_ha` must be a finite, non-negative number.",
            call. = FALSE
        )
    }

    if (min_area_ha == 0) {
        return(patches)
    }

    cell_area <- terra::cellSize(
        patches,
        unit = "ha"
    )

    area_by_patch <- terra::zonal(
        cell_area,
        patches,
        fun = "sum",
        na.rm = TRUE
    )

    if (nrow(area_by_patch) == 0L) {
        return(patches)
    }

    remove_ids <- area_by_patch[[1]][
        area_by_patch[[2]] < min_area_ha
    ]

    if (length(remove_ids) == 0L) {
        return(patches)
    }

    terra::subst(
        patches,
        from = remove_ids,
        to = rep(NA_real_, length(remove_ids))
    )
}
