#' Plot a raster index
#'
#' Plot a single-layer raster such as NBR, NDVI or dNBR.
#'
#' @param x A single-layer terra SpatRaster.
#' @param title Optional plot title.
#' @param subtitle Optional plot subtitle.
#' @param palette Vector of colours.
#' @param limits Optional numeric vector of length two giving the
#'   colour-scale limits.
#' @param legend_title Legend title.
#'
#' @return A ggplot object.
#'
#' @export

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
