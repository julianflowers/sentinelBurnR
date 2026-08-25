#' Create a sentinelBurnR project
#'
#' @param name Project name.
#' @param root Root project directory. If NULL, uses the
#'   `sbr.project_dir` option.
#'
#' @return An sbr_project object.
#'
#' @export

create_project <- function(
        name,
        root = getOption("sbr.project_dir")
) {

    if (is.null(root)) {
        stop(
            "No project root configured. Set it with sbr_options().",
            call. = FALSE
        )
    }

    project_dir <- file.path(
        path.expand(root),
        name
    )

    dirs <- c(
        "raw",
        "composites",
        "indices",
        "outputs",
        "figures",
        "report",
        "logs"
    )

    for (d in dirs) {
        dir.create(
            file.path(project_dir, d),
            recursive = TRUE,
            showWarnings = FALSE
        )
    }

    structure(
        list(
            name = name,
            path = project_dir,
            raw = file.path(project_dir, "raw"),
            composites = file.path(project_dir, "composites"),
            indices = file.path(project_dir, "indices"),
            outputs = file.path(project_dir, "outputs"),
            figures = file.path(project_dir, "figures"),
            report = file.path(project_dir, "report"),
            logs = file.path(project_dir, "logs")
        ),
        class = "sbr_project"
    )
}

#---- save raster ----------------------------------------
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

#------ save metadata -----------------------------------------
s2_item_metadata <- function(items) {

    features <- items$features

    if (is.null(features) || length(features) == 0L) {
        return(
            data.frame(
                id = character(),
                datetime = as.POSIXct(character(), tz = "UTC"),
                date = as.Date(character()),
                cloud_cover = numeric(),
                tile = character(),
                platform = character(),
                stringsAsFactors = FALSE
            )
        )
    }

    rows <- lapply(
        features,
        function(item) {

            properties <- item$properties

            datetime_text <- properties$datetime

            datetime <- as.POSIXct(
                datetime_text,
                format = "%Y-%m-%dT%H:%M:%OSZ",
                tz = "UTC"
            )

            cloud_cover <- properties[["eo:cloud_cover"]]

            if (is.null(cloud_cover)) {
                cloud_cover <- NA_real_
            }

            tile <- properties[["mgrs:tile"]]

            if (is.null(tile)) {
                tile <- NA_character_
            }

            platform <- properties$platform

            if (is.null(platform)) {
                platform <- NA_character_
            }

            data.frame(
                id = item$id,
                datetime = datetime,
                date = as.Date(datetime),
                cloud_cover = as.numeric(cloud_cover),
                tile = as.character(tile),
                platform = as.character(platform),
                stringsAsFactors = FALSE
            )
        }
    )

    result <- do.call(
        rbind,
        rows
    )

    rownames(result) <- NULL

    result[order(result$datetime), ]
}

