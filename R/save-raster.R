#' Save a raster to a project
#'
#' @param x A SpatRaster.
#' @param filename Output filename.
#' @param project sbr_project.
#' @param folder Project folder.
#'
#' @return Invisible filename.
#'
#' @keywords internal

save_raster <- function(
        x,
        filename,
        project,
        folder
) {

    stopifnot(
        inherits(x, "SpatRaster")
    )

    stopifnot(
        inherits(project, "sbr_project")
    )

    out <- file.path(
        project[[folder]],
        filename
    )

    terra::writeRaster(

        x,

        out,

        overwrite = TRUE

    )

    invisible(out)

}
