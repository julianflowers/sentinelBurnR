#' Download Sentinel-2 assets
#'
#' Download selected assets from the scenes returned by
#' `search_s2()`.
#'
#' @param x An `sbr_search` object.
#' @param assets Character vector of asset names.
#' @param output_dir Directory for downloaded files.
#' @param overwrite Should existing files be overwritten?
#'
#' @return A data.frame describing downloaded files.
#'
#' @export
download_s2 <- function(
        x,
        limit = NULL,
        assets = s2_default_assets,
        output_dir = tools::R_user_dir(
            "sentinelBurnR",
            "cache"
        ),
        overwrite = FALSE
) {

    stopifnot(
        inherits(x, "sbr_search")
    )

    dir.create(
        output_dir,
        recursive = TRUE,
        showWarnings = FALSE
    )

    invalid <- setdiff(
        assets,
        names(s2_bands)
    )

    if (length(invalid) > 0) {

        stop(
            "Unknown asset(s): ",
            paste(invalid, collapse = ", "),
            call. = FALSE
        )

    }

    downloads <- data.frame(
        scene = character(),
        asset = character(),
        file = character(),
        stringsAsFactors = FALSE
    )

    scenes <- x$items$features

    if (!is.null(limit)) {
        scenes <- head(scenes, limit)
    }

    for (scene in scenes) {

        scene_id <- scene$id

        scene_dir <- file.path(
            output_dir,
            scene_id
        )

        dir.create(
            scene_dir,
            recursive = TRUE,
            showWarnings = FALSE
        )

        for (asset in assets) {

            if (!asset %in% names(scene$assets))
                next

            url <- scene$assets[[asset]]$href

            ext <- tools::file_ext(url)

            outfile <- file.path(
                scene_dir,
                paste0(asset, ".", ext)
            )

            if (!file.exists(outfile) || overwrite) {

                message(
                    "Downloading ",
                    basename(outfile)
                )

                utils::download.file(
                    url = url,
                    destfile = outfile,
                    mode = "wb",
                    quiet = TRUE
                )

            }

            downloads <- rbind(

                downloads,

                data.frame(

                    scene = scene_id,

                    asset = asset,

                    file = outfile,

                    stringsAsFactors = FALSE

                )

            )

        }

    }

    new_s2_collection(downloads)

}
