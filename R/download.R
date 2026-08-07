#' Download Sentinel-2 assets
#'
#' Download selected assets from scenes returned by `search_s2()`.
#'
#' @param x An `sbr_search` object.
#' @param assets Character vector of asset names.
#' @param limit Optional maximum number of scenes to download.
#' @param output_dir Directory for downloaded files.
#' @param overwrite Should existing files be overwritten?
#'
#' @return An `sbr_collection` object.
#'
#' @export
download_s2 <- function(
        x,
        assets = s2_default_assets,
        limit = NULL,
        output_dir = tools::R_user_dir(
            "sentinelBurnR",
            which = "cache"
        ),
        overwrite = FALSE
) {

    stopifnot(
        inherits(x, "sbr_search")
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

    dir.create(
        output_dir,
        recursive = TRUE,
        showWarnings = FALSE
    )

    scenes <- x$items$features

    if (!is.null(limit)) {
        scenes <- head(
            scenes,
            limit
        )
    }

    downloads <- data.frame(
        scene = character(),
        tile = character(),
        date = as.Date(character()),
        satellite = character(),
        asset = character(),
        file = character(),
        stringsAsFactors = FALSE
    )

    for (scene in scenes) {

        scene_id <- scene$id

        parts <- strsplit(
            scene$id,
            "_",
            fixed = TRUE
        )[[1]]

        tile <- parts[2]

        acquisition_date <- as.Date(
            substr(
                scene$properties$datetime,
                1,
                10
            )
        )

        satellite <- scene$properties$platform

        if (is.null(satellite)) {
            satellite <- NA_character_
        }

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

            if (!asset %in% names(scene$assets)) {
                warning(
                    "Asset '",
                    asset,
                    "' not found in scene ",
                    scene_id,
                    call. = FALSE
                )

                next
            }

            url <- scene$assets[[asset]]$href

            ext <- tools::file_ext(url)

            if (!nzchar(ext)) {
                ext <- "tif"
            }

            outfile <- file.path(
                scene_dir,
                paste0(
                    asset,
                    ".",
                    ext
                )
            )

            if (!file.exists(outfile) || overwrite) {

                message(
                    "Downloading ",
                    scene_id,
                    " / ",
                    asset
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
                    tile = as.character(tile),
                    date = acquisition_date,
                    satellite = as.character(satellite),
                    asset = asset,
                    file = outfile,
                    stringsAsFactors = FALSE
                )
            )
        }
    }

    downloads <- downloads[
        order(
            downloads$date,
            downloads$tile,
            downloads$asset
        ),
        ,
        drop = FALSE
    ]

    rownames(downloads) <- NULL

    new_s2_collection(
        downloads
    )
}
