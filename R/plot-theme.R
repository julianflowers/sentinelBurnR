# Shared sentinelBurnR plotting theme
#
# @keywords internal

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
