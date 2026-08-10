#' Plot Normalized Burn Ratio
#'
#' @param x A single-layer NBR SpatRaster.
#' @param title Plot title.
#'
#' @return A ggplot object.
#'
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
