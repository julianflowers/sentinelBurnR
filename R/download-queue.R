build_s2_download_queue <- function(
        scenes,
        assets,
        output_dir
) {

    jobs <- list()
    n <- 0L

    for (scene in scenes) {

        scene_id <- scene$id

        tile <- extract_s2_tile(scene)

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

            if (!asset %in% names(scene$assets))
                next

            href <- scene$assets[[asset]]$href

            ext <- tools::file_ext(href)

            if (!nzchar(ext))
                ext <- "tif"

            outfile <- file.path(
                scene_dir,
                paste0(asset, ".", ext)
            )

            n <- n + 1L

            jobs[[n]] <- data.frame(

                scene = scene_id,

                tile = tile,

                date = acquisition_date,

                satellite = satellite,

                asset = asset,

                url = href,

                file = outfile,

                stringsAsFactors = FALSE

            )

        }

    }

    queue <- do.call(
        rbind,
        jobs
    )

    rownames(queue) <- NULL

    queue

}
