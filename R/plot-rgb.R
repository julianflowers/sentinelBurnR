#' Plot RGB composite
#'
#' @param x Composite SpatRaster.
#' @param title Plot title.
#'
#' @return ggplot object.
#'
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






