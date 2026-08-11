plot_rgb <- function(
        x
) {

    rgb <- x[[

        c(
            "red",
            "green",
            "blue"
        )

    ]]

    ggplot() +

        tidyterra::geom_spatraster_rgb(

            data = rgb

        ) +

        coord_sf(

            expand = FALSE

        ) +

        theme_sbr()

}
