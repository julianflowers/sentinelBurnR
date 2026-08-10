as_plot_data <- function(x) {

    stopifnot(
        inherits(x, "SpatRaster")
    )

    tidyterra::as_tibble(
        x,
        xy = TRUE,
        na.rm = FALSE
    )

}
