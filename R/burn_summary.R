#' Summarise burn statistics
#'
#' @param burn An `sbr_burn` object.
#' @param boundary Optional boundary. If omitted, `burn$boundary` is used.
#' @param id Optional attribute used to name polygons.
#'
#' @return An object of class `sbr_summary`.
#'
#' @export
burn_summary <- function(
        burn,
        boundary = NULL,
        id = NULL
) {

    if (!inherits(burn, "sbr_burn")) {
        stop(
            "`burn` must be an sbr_burn object.",
            call. = FALSE
        )
    }

    if (is.null(boundary)) {
        boundary <- burn$boundary
    }

    if (is.null(boundary)) {
        stop(
            "No boundary supplied.",
            call. = FALSE
        )
    }

    #
    # Read and project boundary to raster CRS
    #

    boundary <- prepare_boundary(
        boundary,
        burn$dnbr
    )

    if (!is.null(id) &&
        !id %in% names(boundary)) {

        stop(
            "`id` is not an attribute of `boundary`.",
            call. = FALSE
        )
    }

    raster_ext <- terra::ext(
        burn$dnbr
    )

    results <- list()

    for (i in seq_len(nrow(boundary))) {

        polygon <- boundary[i]

        poly_ext <- terra::ext(
            polygon
        )

        overlaps <-
            poly_ext$xmin < raster_ext$xmax &&
            poly_ext$xmax > raster_ext$xmin &&
            poly_ext$ymin < raster_ext$ymax &&
            poly_ext$ymax > raster_ext$ymin

        if (!overlaps) {
            next
        }

        stats <- summarise_polygon(
            burn,
            polygon
        )

        boundary_name <- if (is.null(id)) {

            paste(
                "Polygon",
                i
            )

        } else {

            as.character(
                boundary[[id]][i]
            )
        }

        stats$boundary <- boundary_name

        stats <- stats[
            c(
                "boundary",
                setdiff(
                    names(stats),
                    "boundary"
                )
            )
        ]

        results[[length(results) + 1L]] <- stats
    }

    if (length(results) == 0L) {

        stop(
            "No boundary polygons overlap the burn raster.",
            call. = FALSE
        )
    }

    out <- do.call(
        rbind,
        results
    )

    rownames(out) <- NULL

    class(out) <- c(
        "sbr_summary",
        "data.frame"
    )

    out
}


#==========================================================
# Summarise a single polygon (internal)
#==========================================================

summarise_polygon <- function(
        burn,
        polygon
) {

    dnbr <- terra::mask(
        terra::crop(
            burn$dnbr,
            polygon
        ),
        polygon
    )

    burned <- terra::mask(
        terra::crop(
            burn$burned,
            polygon
        ),
        polygon
    )

    severity <- terra::mask(
        terra::crop(
            burn$severity,
            polygon
        ),
        polygon
    )

    pixel_area_ha <-
        prod(
            terra::res(dnbr)
        ) / 10000

    analysed_cells <-
        terra::global(
            !is.na(dnbr),
            "sum",
            na.rm = TRUE
        )[1, 1]

    burned_cells <-
        terra::global(
            burned,
            "sum",
            na.rm = TRUE
        )[1, 1]

    mean_dnbr <-
        terra::global(
            dnbr,
            "mean",
            na.rm = TRUE
        )[1, 1]

    burn_pct <- if (analysed_cells > 0) {

        100 *
            burned_cells /
            analysed_cells

    } else {

        NA_real_

    }

    severity_values <-
        terra::values(
            severity,
            mat = FALSE
        )

    severity_counts <-
        table(
            factor(
                severity_values,
                levels = 1:7
            )
        )

    severity_ha <-
        as.numeric(
            severity_counts
        ) *
        pixel_area_ha

    names(severity_ha) <- c(

        "regrowth_high_ha",
        "regrowth_low_ha",
        "unburned_ha",
        "low_ha",
        "moderate_low_ha",
        "moderate_high_ha",
        "high_ha"

    )

    data.frame(

        area_ha =
            analysed_cells *
            pixel_area_ha,

        burned_ha =
            burned_cells *
            pixel_area_ha,

        burn_pct =
            burn_pct,

        mean_dnbr =
            mean_dnbr,

        t(severity_ha),

        check.names = FALSE

    )

}

#==========================================================
# Print method
#==========================================================

#' @export

print.sbr_summary <- function(
        x,
        ...
) {

    if (!inherits(
        x,
        "data.frame"
    )) {

        stop(
            "Invalid sbr_summary object."
        )

    }

    if (nrow(x) == 1L) {

        cat("\n")

        cat(
            "Burn summary\n"
        )

        cat(
            "------------\n\n"
        )

        cat(
            sprintf(
                "Boundary      : %s\n",
                x$boundary
            )
        )

        cat(
            sprintf(
                "Area analysed : %.1f ha\n",
                x$area_ha
            )
        )

        cat(
            sprintf(
                "Burned area   : %.1f ha\n",
                x$burned_ha
            )
        )

        cat(
            sprintf(
                "Burned        : %.1f %%\n",
                x$burn_pct
            )
        )

        cat(
            sprintf(
                "Mean dNBR     : %.3f\n",
                x$mean_dnbr
            )
        )

        cat("\n")

    } else {

        print.data.frame(
            x,
            row.names = FALSE
        )

    }

    invisible(
        x
    )

}

