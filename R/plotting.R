#----plot rgb -----------------------------------
#' Plot an RGB composite
#'
#' @param x A composite containing red, green and blue bands.
#' @param title Plot title.
#' @param subtitle Optional subtitle.
#' @param rgb_stretch Apply stretch - linear, histogram or none
#' @param boundary Optional boundary to overlay on the plot.
#' @return A ggplot object.
#'
#' @export
plot_rgb <- function(

    x,

    title = "Sentinel-2 RGB composite",

    subtitle = NULL,

    rgb_stretch = c(
        "lin",
        "hist",
        "none"),

    boundary = NULL


) {

    if (!inherits(x, "SpatRaster")) {
        stop(
            "`x` must be a SpatRaster.",
            call. = FALSE
        )
    }

    rgb_stretch <- match.arg(rgb_stretch)

    rgb <- x[[c(
        "red",
        "green",
        "blue"
    )]]

    p <- ggplot2::ggplot()

    if (rgb_stretch == "none") {

        p <- p +
            tidyterra::geom_spatraster_rgb(
                data = rgb
            )

    } else {

        p <- p +
            tidyterra::geom_spatraster_rgb(
                data = rgb,
                stretch = rgb_stretch
            )

    }

    p <- p +
        ggplot2::coord_sf(
            expand = FALSE
        ) +
        ggplot2::labs(
            title = title,
            subtitle = subtitle,

        ) +
        theme_sbr_map()

    p <- overlay_boundary(
        p,
        boundary,
        rgb
    )

    p
}

#------ theme sbr ------------------------------------
#' Theme for spatial plots
#'
#' @keywords internal

theme_sbr_map <- function() {



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

            plot.caption = ggplot2::element_text(

                size = 9,

                colour = "grey40",

                hjust = 0

            ),

            legend.position = "right",

            legend.title = ggplot2::element_text(
                face = "bold"
            )

        )
}

#' Theme for charts
#'
#' @keywords internal

theme_sbr_plot <- function() {

    ggplot2::theme_minimal(
        base_size = 12
    ) +

        ggplot2::theme(

            panel.grid.minor =
                ggplot2::element_blank(),

            panel.grid.major.x =
                ggplot2::element_blank(),

            panel.grid.major.y =
                ggplot2::element_line(
                    colour = "grey85"
                ),

            plot.caption = ggplot2::element_text(

                size = 9,

                colour = "grey40",

                hjust = 0

            ),

            axis.title =
                ggplot2::element_text(),

            axis.text =
                ggplot2::element_text(),

            axis.ticks =
                ggplot2::element_line(),

            plot.title =
                ggplot2::element_text(
                    face = "bold",
                    size = 14
                ),

            plot.subtitle =
                ggplot2::element_text(
                    size = 11
                ),

            legend.position = "right",

            legend.title =
                ggplot2::element_text(
                    face = "bold"
                )

        )

}

#-----plot index ----------------------------------------
#' Plot a continuous raster index
#'
#' @param x A SpatRaster with one layer.
#' @param index Name of the registered index.
#' @export
plot_index <- function(
        x,
        index,
        title = NULL,
        subtitle = NULL,
        boundary = NULL,
        caption = NULL
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

    index <- tolower(index)

    info <- index_info[[index]]

    if (is.null(info)) {
        stop(
            "Unknown index: ",
            index,
            call. = FALSE
        )
    }

    if (is.null(title)) {
        title <- info$title
    }


    palette <- palette_lookup(info$palette)

    p <- ggplot2::ggplot() +

            tidyterra::geom_spatraster(

                data = x

            ) +

            ggplot2::scale_fill_gradientn(
                colours = palette,
                na.value = "transparent",
                name = info$name
            ) +

            ggplot2::labs(

                title = title,

                subtitle = subtitle,

                caption = caption



            ) +

            ggplot2::coord_sf(

                expand = FALSE

            ) +

            theme_sbr_map()

    p <- overlay_boundary(
        p,
        boundary,
        x
    )

    p

}


# rainfall ----------------------------------------------------------------


#' @importFrom graphics par
#' @importFrom rlang .data
#' @export
plot.sbr_rainfall <- function(
        x,
        title = "Daily rainfall",
        subtitle = NULL,
        ...
) {

    stopifnot(
        inherits(x, "sbr_rainfall")
    )

    ggplot2::ggplot(
        x,
        ggplot2::aes(
            x = .data$date,
            y = .data$precipitation_mm
        )
    ) +

        ggplot2::geom_col(
            fill = "#4C78A8",
            width = 0.9
        ) +

        ggplot2::labs(
            title = title,
            subtitle = subtitle,
            x = NULL,
            y = "Rainfall (mm)"
        ) +

        theme_sbr_plot()

}

# plot severity -----------------------------------------------------------

#' Plot Burn severity
#'
#' @param x A single-layer dNBR SpatRaster.
#' @param title Plot title.
#' @param subtitle Plot subtitle
#' @param boundary Optional boundary to overlay on the plot.
#' @return A ggplot object.
#' @export

plot_severity <- function(
        x,
        title = "Burn severity",
        subtitle = NULL,
        caption = NULL,
        boundary = NULL
) {

    plot_index(
        x = x,
        title = title,
        subtitle = subtitle,
        caption = caption,
        palette = sbr_palette_dnbr,
        legend_title = "Severty",
        boundary = boundary
    )

}

#     x <- terra::as.factor(x)
#
#     p <- ggplot2::ggplot() +
#
#         tidyterra::geom_spatraster(
#             data = x
#         ) +
#
#         ggplot2::scale_fill_manual(
#             values = burn_palette,
#             drop = FALSE,
#             name = "Burn severity"
#         ) +
#
#         ggplot2::coord_sf(expand = FALSE) +
#
#         ggplot2::labs(
#             title = title,
#             subtitle = subtitle,
#             caption = caption
#         ) +
#
#         theme_sbr_map()
#
#     if (!is.null(boundary)) {
#
#         boundary <- read_boundary(boundary)
#
#         if (!terra::same.crs(
#             boundary,
#             x
#         )) {
#
#             boundary <- terra::project(
#                 boundary,
#                 terra::crs(x)
#             )
#
#         }
#
#         p <- p +
#
#             tidyterra::geom_spatvector(
#                 data = boundary,
#                 fill = NA,
#                 colour = "black",
#                 linewidth = 0.6
#             )
#     }
#
#     p
#
# }


# overlay boundary --------------------------------------------------------

overlay_boundary <- function(
        p,
        boundary,
        raster
) {

    if (is.null(boundary)) {
        return(p)
    }

    boundary <- read_boundary(boundary)

    if (!terra::same.crs(
        boundary,
        raster
    )) {

        boundary <- terra::project(
            boundary,
            terra::crs(raster)
        )
    }

    p +
        tidyterra::geom_spatvector(
            data = boundary,
            fill = NA,
            linewidth = 0.6
        )
}

add_boundary <- function(
        p,
        boundary
) {

    if (is.null(boundary))
        return(p)

    p +

        geom_sf(
            ...
        )

}

add_caption <- function(
        p,
        caption
) {

    if (is.null(caption))
        return(p)

    p +

        labs(
            caption = caption
        )

}


