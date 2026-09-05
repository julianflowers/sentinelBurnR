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
#' @param x A `SpatRaster` containing the index to plot.
#' @param index Character name of the spectral index.
#' @param title Optional plot title.
#' @param subtitle Optional plot subtitle.
#' @param boundary Optional spatial boundary to overlay on the plot.
#' @param caption Optional plot caption.
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
                name = info$name,
                limits = info$limits,
                oob = scales:::squish

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
#' @param caption Optional plot caption
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

    x <- terra::as.factor(x)

    levels(x) <- data.frame(
        ID = 1:7,
        severity = dnbr_labels
    )

    p <- ggplot2::ggplot() +

        tidyterra::geom_spatraster(
            data = x
        ) +

        ggplot2::scale_fill_manual(
            values = stats::setNames(
                burn_palette,
                dnbr_labels
            ),
            drop = FALSE,
            name = "Burn severity"
        ) +

        ggplot2::coord_sf(expand = FALSE) +

        ggplot2::labs(
            title = title,
            subtitle = subtitle,
            caption = caption
        ) +

        ggplot2::theme_minimal()

    add_boundary(
        p,
        boundary = boundary
    )
}

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

    if (is.null(boundary)){
        return(p)

}
if (inherits(boundary, "SpatVector")) {
    boundary <- sf::st_as_sf(boundary)
}

    p +

        ggplot2::geom_sf(
            data = boundary,
            fill = NA
        )

}

add_caption <- function(
        p,
        caption
) {

    if (is.null(caption))
        return(p)

    p +

        ggplot2::labs(
            caption = caption
        )

}


# print sbr_drought -------------------------------------------------------
#' @export
print.sbr_drought <- function(x, ...) {

    s <- x$summary

    cat("<sbr_drought>\n")
    cat(
        "Current date:      ",
        format(s$current_date),
        "\n",
        sep = ""
    )

    cat(
        "Baseline:          ",
        s$baseline_start,
        "-",
        s$baseline_end,
        " (",
        s$baseline_years,
        " years)\n",
        sep = ""
    )

    cat(
        "Seasonal window:   ±",
        s$window_days,
        " days\n",
        sep = ""
    )

    cat(
        "Valid coverage:    ",
        sprintf("%.1f%%", 100 * s$valid_coverage),
        "\n",
        sep = ""
    )

    cat(
        "Median anomaly:    ",
        sprintf("%.3f", s$anomaly_median),
        "\n",
        sep = ""
    )

    cat(
        "Pixels below -2SD: ",
        sprintf(
            "%.1f%%",
            100 * s$proportion_below_minus_2sd
        ),
        "\n",
        sep = ""
    )

    invisible(x)
}


# plot drought ------------------------------------------------------------

#' Plot drought analysis
#'
#' Plot vegetation moisture anomalies from an `sbr_drought`
#' analysis.
#'
#' @param x An object of class `sbr_drought`.
#' @param index Drought product to plot. Either `"anomaly"` or
#'   `"standardised"`.
#' @param boundary Optional boundary to overlay.
#' @param title Optional plot title.
#' @param subtitle Optional plot subtitle.
#' @param caption Optional plot caption.
#'
#' @return A ggplot object.
#'
#' @export
plot_drought <- function(
        x,
        index = c("anomaly", "standardised"),
        boundary = NULL,
        title = NULL,
        subtitle = NULL,
        caption = NULL
) {

    if (!inherits(x, "sbr_drought")) {
        stop("`x` must be an sbr_drought object.")
    }

    index <- match.arg(index)

    s <- x$summary

    if (index == "anomaly") {

        r <- x$anomaly

        if (is.null(title)) {
            title <- "Vegetation moisture anomaly"
        }

        if (is.null(subtitle)) {
            subtitle <- paste0(
                "NDMI anomaly on ",
                format(s$current_date),
                " relative to ",
                s$baseline_start,
                "\u2013",
                s$baseline_end,
                " seasonal baseline"
            )
        }


    } else {

        r <- x$standardised

        if (is.null(title)) {
            title <- "Standardised vegetation moisture anomaly"
        }

        if (is.null(subtitle)) {
            subtitle <- paste0(
                format(s$current_date),
                " relative to ",
                s$baseline_start,
                "\u2013",
                s$baseline_end,
                " seasonal baseline"
            )
        }

    }

    plot_index(
        r,
        index = names(r)[1],
        title = title,
        subtitle = subtitle,
        boundary = boundary,
        caption = caption    )
}






