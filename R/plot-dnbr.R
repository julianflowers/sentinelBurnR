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
        title = "Differenced Normalized Burn Ratio"
) {

    plot_index(
        x = x,
        title = title,
        palette = sbr_palette_dnbr,
        limits = NULL,
        legend_title = "dNBR"
    )
}
