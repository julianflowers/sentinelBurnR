#' Plot Sentinel-2 Scene Classification Layer
#'
#' @param collection sbr_collection.
#' @param tile Tile index.
#' @param layer Date/layer index.
#' @param overlay Draw cloud classes on top of an RGB image?
#'
#' @export

plot_scl <- function(
        collection,
        tile = 1,
        layer = 1
) {

    scl <- read_band(
        collection,
        "scl"
    )[[tile]][[layer]]

    ggplot2::ggplot() +

        tidyterra::geom_spatraster(
            data = scl
        ) +

        ggplot2::scale_fill_manual(

            values = s2_scl_colours,

            breaks = 0:11,

            labels = c(
                "No data",
                "Saturated",
                "Dark",
                "Shadow",
                "Vegetation",
                "Bare soil",
                "Water",
                "Unclassified",
                "Medium cloud",
                "High cloud",
                "Cirrus",
                "Snow"
            ),

            na.value = "transparent"

        ) +

        ggplot2::coord_equal() +

        ggplot2::theme_minimal()

}
