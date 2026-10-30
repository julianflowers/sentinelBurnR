
#' Summarise disturbance classifications by patch area
#'
#' @param disturbances A data frame containing `patch`,
#'   `classification`, and `disturbance_date`.
#' @param patches A single-layer SpatRaster containing patch IDs.
#' @param by_date Whether to group results by disturbance date.
#'
#' @return A tibble containing patch counts, total patch area
#'   and median patch area in hectares.
#' @export
summarise_disturbance_area <- function(
        disturbances,
        patches,
        by_date = FALSE
) {

    if (!inherits(patches, "SpatRaster") ||
        terra::nlyr(patches) != 1L) {
        stop(
            "`patches` must be a single-layer SpatRaster.",
            call. = FALSE
        )
    }

    required <- c("patch", "classification")

    if (isTRUE(by_date)) {
        required <- c(required, "disturbance_date")
    }

    missing <- setdiff(required, names(disturbances))

    if (length(missing) > 0L) {
        stop(
            "Missing required columns: ",
            paste(missing, collapse = ", "),
            call. = FALSE
        )
    }

    cell_area <- terra::cellSize(
        patches,
        unit = "ha"
    )

    areas <- terra::zonal(
        cell_area,
        patches,
        fun = "sum",
        na.rm = TRUE
    )

    names(areas) <- c("patch", "area_ha")

    x <- dplyr::left_join(
        disturbances,
        tibble::as_tibble(areas),
        by = "patch"
    )

    groups <- "classification"

    if (isTRUE(by_date)) {
        groups <- c(groups, "disturbance_date")
    }

    x |>
        dplyr::group_by(
            dplyr::across(dplyr::all_of(groups))
        ) |>
        dplyr::summarise(
            patches = dplyr::n(),
            total_area_ha = sum(area_ha, na.rm = TRUE),
            median_patch_ha = median(area_ha, na.rm = TRUE),
            .groups = "drop"
        )
}
