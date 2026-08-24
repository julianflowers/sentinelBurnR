#' Summarise burn statistics within boundaries
#'
#' @param burn An sbr_burn object.
#' @param boundary Boundary polygons.
#' @param id Optional attribute giving polygon names.
#'
#' @return An sbr_summary object.
#'
#' @export

summarise_polygon <- function(
        burn,
        polygon
) {

    #
    # Crop
    #

    dnbr <- terra::crop(
        burn$dnbr,
        polygon
    )

    burned <- terra::crop(
        burn$burned,
        polygon
    )

    #
    # Mask
    #

    dnbr <- terra::mask(
        dnbr,
        polygon
    )

    burned <- terra::mask(
        burned,
        polygon
    )

    #
    # Pixel area
    #

    pixel_area_ha <-

        prod(
            terra::res(dnbr)
        ) / 10000

    #
    # Area analysed
    #

    analysed_cells <-

        terra::global(
            !is.na(dnbr),
            "sum",
            na.rm = TRUE
        )[1, 1]

    area_ha <-

        analysed_cells *

        pixel_area_ha

    #
    # Burned area
    #

    burned_cells <-

        terra::global(
            burned,
            "sum",
            na.rm = TRUE
        )[1, 1]

    burned_ha <-

        burned_cells *

        pixel_area_ha

    #
    # Mean dNBR
    #

    mean_dnbr <-

        terra::global(
            dnbr,
            "mean",
            na.rm = TRUE
        )[1, 1]


    list(

        area_ha = area_ha,

        burned_ha = burned_ha,

        burn_pct =

            100 *

            burned_ha /

            area_ha,

        mean_dnbr = mean_dnbr

    )

}


burn_summary <- function(
        burn,
        boundary = NULL,
        id = NULL
) {

    stopifnot(
        inherits(
            burn,
            "sbr_burn"
        )
    )

    if (is.null(boundary)) {

        boundary <- burn$boundary

    }

    if (is.null(boundary)) {

        stop(
            "No boundary supplied.",
            call. = FALSE
        )

    }

    boundary <- read_boundary(
        boundary
    )

    if (!terra::same.crs(
        boundary,
        burn$dnbr
    )) {

        boundary <- terra::project(
            boundary,
            terra::crs(
                burn$dnbr
            )
        )

    }

    out <- vector(
        "list",
        terra::nrow(boundary)
    )

    message("Boundary CRS:")
    print(terra::crs(boundary))

    message("Raster CRS:")
    print(terra::crs(burn$dnbr))

    message("Boundary extent:")
    print(terra::ext(boundary))

    message("Raster extent:")
    print(terra::ext(burn$dnbr))

    for (i in seq_len(
        terra::nrow(boundary)
    )) {

        stats <- summarise_polygon(
            burn,
            boundary[i]
        )

        if (is.null(id)) {

            stats$boundary <-

                paste(
                    "Polygon",
                    i
                )

        } else {

            stats$boundary <-

                boundary[[id]][i]

        }

        out[[i]] <- stats

    }

    out <-

        do.call(
            rbind,
            out
        )

    out <-

        out[
            c(
                "boundary",
                "area_ha",
                "burned_ha",
                "burn_pct",
                "mean_dnbr"
            )
        ]

    class(out) <-
        c(
            "sbr_summary",
            class(out)
        )

    out

}

