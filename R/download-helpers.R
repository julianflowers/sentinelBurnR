# Build a Sentinel-2 download queue
#
# @param scenes List of STAC scene features.
# @param assets Character vector of asset names.
# @param output_dir Cache directory.
#
# @return A data frame containing one row per requested asset.

build_download_queue <- function(
        scenes,
        assets,
        output_dir
) {

    jobs <- list()

    n <- 0L

    for (scene in scenes) {

        scene_id <- scene$id

        tile <- extract_s2_tile(
            scene
        )

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

            extension <- tools::file_ext(url)

            if (!nzchar(extension)) {
                extension <- "tif"
            }

            outfile <- file.path(
                scene_dir,
                paste0(
                    asset,
                    ".",
                    extension
                )
            )

            n <- n + 1L

            jobs[[n]] <- data.frame(
                scene = scene_id,
                tile = tile,
                date = acquisition_date,
                satellite = as.character(satellite),
                asset = asset,
                url = url,
                file = outfile,
                stringsAsFactors = FALSE
            )
        }
    }

    if (length(jobs) == 0L) {

        return(
            data.frame(
                scene = character(),
                tile = character(),
                date = as.Date(character()),
                satellite = character(),
                asset = character(),
                url = character(),
                file = character(),
                stringsAsFactors = FALSE
            )
        )
    }

    queue <- do.call(
        rbind,
        jobs
    )

    queue <- queue[
        order(
            queue$date,
            queue$tile,
            queue$asset
        ),
        ,
        drop = FALSE
    ]

    rownames(queue) <- NULL

    queue
}

# Download one Sentinel-2 asset
#
# @param job One-row data frame from build_download_queue().
# @param overwrite Logical. Overwrite an existing file?
#
# @return One-row data frame describing the result.

download_s2_file <- function(
        job,
        overwrite = FALSE
) {

    outfile <- job$file[[1]]
    url <- job$url[[1]]

    status <- "cached"

    if (!file.exists(outfile) || overwrite) {

        status <- tryCatch({

            tmpfile <- paste0(
                outfile,
                ".part"
            )

            if (file.exists(tmpfile)) {
                unlink(tmpfile)
            }

            utils::download.file(
                url = url,
                destfile = tmpfile,
                mode = "wb",
                quiet = TRUE
            )

            if (!file.exists(tmpfile)) {
                stop("Download did not create a file.")
            }

            if (file.info(tmpfile)$size <= 0) {
                stop("Downloaded file is empty.")
            }

            if (file.exists(outfile)) {
                unlink(outfile)
            }

            ok <- file.rename(
                tmpfile,
                outfile
            )

            if (!ok) {
                stop("Could not move temporary download into cache.")
            }

            "downloaded"

        }, error = function(e) {

            warning(
                "Failed: ",
                job$scene[[1]],
                " / ",
                job$asset[[1]],
                " — ",
                conditionMessage(e),
                call. = FALSE
            )

            "failed"
        })
    }

    data.frame(
        scene = job$scene[[1]],
        tile = job$tile[[1]],
        date = job$date[[1]],
        satellite = job$satellite[[1]],
        asset = job$asset[[1]],
        file = outfile,
        status = status,
        stringsAsFactors = FALSE
    )
}
