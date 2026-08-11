#' Plot differenced Normalized Burn Ratio
#'
#' @param x A single-layer dNBR SpatRaster.
#' @param title Plot title.
#'
#' @return A ggplot object.
#'
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
            hcl.colors(
                11,
                "RdYlGn"
            )
        ),

        legend_title = "dNBR"

    )

}
