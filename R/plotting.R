


#----plot rgb -----------------------------------
#' Plot differenced Normalized Burn Ratio
#'
#' @param x A single-layer dNBR SpatRaster.
#' @param title Plot title.
#' @param subtitle Plot subtitle
#'
#' @return A ggplot object.
#' @export
plot_rgb <- function(
        x,
        title = "RGB composite",
        subtitle = NULL
) {

    if (!inherits(x, "SpatRaster")) {

        stop(
            "`x` must be a SpatRaster.",
            call. = FALSE
        )

    }

    needed <- c(
        "red",
        "green",
        "blue"
    )

    missing <- setdiff(
        needed,
        names(x)
    )

    if (length(missing) > 0) {

        stop(
            "Composite is missing: ",
            paste(
                missing,
                collapse = ", "
            ),
            call. = FALSE
        )

    }

    rgb <- x[[

        c(
            "red",
            "green",
            "blue"
        )

    ]]


    ggplot2::ggplot() +

        tidyterra::geom_spatraster_rgb(
            data = rgb
        ) +

        ggplot2::coord_sf(
            expand = FALSE
        ) +

        ggplot2::labs(

            title = title,

            subtitle = subtitle

        ) +

        theme_sbr()

}


#------ theme sbr ------------------------------------

theme_sbr <- function() {

    ggplot2::theme_minimal(
        base_size = 12
    ) +

        ggplot2::theme(

            panel.grid = ggplot2::element_blank(),

            axis.title = ggplot2::element_blank(),

            axis.text = ggplot2::element_blank(),

            axis.ticks = ggplot2::element_blank(),

            plot.title = ggplot2::element_text(
                face = "bold",
                size = 14
            ),

            plot.subtitle = ggplot2::element_text(
                size = 11
            ),

            legend.position = "right",

            legend.title = ggplot2::element_text(
                face = "bold"
            )

        )
}

#-----plot index ----------------------------------------

plot_index <- function(
        x,
        title = NULL,
        subtitle = NULL,
        palette = sbr_palette_nbr,
        limits = NULL,
        legend_title = NULL
) {

    if (!inherits(x, "SpatRaster")) {
        stop(
            "`x` must be a terra::SpatRaster.",
            call. = FALSE
        )
    }

    if (terra::nlyr(x) != 1L) {
        stop(
            "`x` must contain exactly one raster layer.",
            call. = FALSE
        )
    }


    return(

        ggplot2::ggplot() +

            tidyterra::geom_spatraster(

                data = x

            ) +

            ggplot2::scale_fill_viridis_c(
                option = "viridis",
                na.value = "transparent",
                limits = limits,
                name = legend_title
            ) +

            ggplot2::labs(

                title = title,

                subtitle = subtitle

            ) +

            ggplot2::coord_sf(

                expand = FALSE

            ) +

            theme_sbr()

    )
}

#------plot nbr --------------------------------

#' Plot Normalized Burn Ratio
#'
#' @param x A single-layer NBR SpatRaster.
#' @param title Plot title.
#' @return A ggplot object.
#' @export
plot_nbr <- function(
        x,
        title = "Normalized Burn Ratio"
) {

    plot_index(
        x = x,
        title = title,
        palette = sbr_palette_nbr,
        limits = c(-1, 1),
        legend_title = "NBR"
    )
}

#-------plot dnbr------------------------------------
#' Plot differenced Normalized Burn Ratio
#'
#' @param x A single-layer dNBR SpatRaster.
#' @param title Plot title.
#' @param subtitle Plot subtitle
#' @return A ggplot object.
#' @export
plot_dnbr <- function(
        x,
        title = "Differenced Normalized Burn Ratio",
        subtitle = NULL
) {

    plot_index(

        x = x,

        title = title,

        subtitle = subtitle,

        palette = rev(
            grDevices::hcl.colors(
                11,
                "RdYlGn"
            )
        ),

        legend_title = "dNBR"

    )

}

#------- plot scl -------------------------------
plot_scl <- function(
        collection,
        tile = 1,
        layer = 1
) {

    scl <- read_band(
        collection,
        "scl"
    )[[tile]][[layer]]

    ggplot2::ggplot() +

        tidyterra::geom_spatraster(
            data = scl
        ) +

        ggplot2::scale_fill_manual(

            values = s2_scl_colours,

            breaks = 0:11,

            labels = c(
                "No data",
                "Saturated",
                "Dark",
                "Shadow",
                "Vegetation",
                "Bare soil",
                "Water",
                "Unclassified",
                "Medium cloud",
                "High cloud",
                "Cirrus",
                "Snow"
            ),

            na.value = "transparent"

        ) +

        ggplot2::coord_equal() +

        ggplot2::theme_minimal()

}



