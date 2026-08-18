#' Download Sentinel-2 assets
#'
#' Download selected Sentinel-2 assets from an `sbr_search` object.
#' Existing cached files are skipped unless `overwrite = TRUE`.
#'
#' @param x An `sbr_search` object.
#' @param assets Character vector of asset names.
#' @param limit Optional maximum number of scenes to download.
#' @param max_cloud Set numerical value for cloud cover cut off.
#' @param output_dir Directory used to cache downloaded files.
#' @param overwrite Logical. Overwrite existing files?
#' @param workers Number of parallel download workers.
#' @param project Project name description
#'
#' @return An `sbr_collection` object.
#'
#' @export
download_s2 <- function(
        x,
        assets = NULL,
        limit = NULL,
        max_cloud = NULL,
        project = NULL,
        output_dir = cache_downloads(),
        overwrite = FALSE,
        workers = 4
)   {

    if (!is.null(project)) {

        if (!inherits(project, "sbr_project")) {
            stop(
                "`project` must be an sbr_project.",
                call. = FALSE
            )
        }

        output_dir <- project$raw
    }

    if (is.null(assets)) {
        assets <- s2_default_assets
    }

    if (!inherits(x, "sbr_search")) {
        stop(
            "`x` must be an sbr_search object.",
            call. = FALSE
        )
    }

    if (!is.numeric(workers) ||
        length(workers) != 1L ||
        workers < 1) {

        stop(
            "`workers` must be a positive integer.",
            call. = FALSE
        )
    }

    workers <- as.integer(workers)

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

    n_before <- length(scenes)

    if (!is.null(max_cloud)) {

        keep <- vapply(
            scenes,
            function(scene) {

                cloud <- scene$properties[["eo:cloud_cover"]]

                if (is.null(cloud) || is.na(cloud)) {
                    return(FALSE)
                }

                cloud <= max_cloud
            },
            logical(1)
        )

        scenes <- scenes[keep]

        message(

            "Cloud filter (<e2><89><a4> ",

            max_cloud,

            "%): ",

            length(scenes),

            " of ",

            n_before,

            " scenes retained."

        )


    }

    if (!is.null(limit)) {
        scenes <- utils::head(
            scenes,
            limit
        )
    }

    if (length(scenes) == 0L) {
        stop(
            "No scenes available to download.",
            call. = FALSE
        )
    }

    start_time <- Sys.time()

    queue <- build_s2_download_queue(
        scenes = scenes,
        assets = assets,
        output_dir = output_dir
    )

    if (nrow(queue) == 0L) {
        stop(
            "No downloadable assets were found.",
            call. = FALSE
        )
    }

    message(
        "Download queue: ",
        nrow(queue),
        " files from ",
        length(unique(queue$scene)),
        " scenes"
    )

    message(
        "Downloading ",
        nrow(queue),
        " assets using ",
        workers,
        " worker(s)..."
    )


    progressr::handlers(
        progressr::handler_txtprogressbar(
            style = 3
        )
    )

    results <- progressr::with_progress({

        parallel_apply(

            X = seq_len(
                nrow(queue)
            ),

            FUN = function(i) {

                download_s2_asset(

                    job = queue[
                        i,
                        ,
                        drop = FALSE
                    ],

                    overwrite = overwrite

                )

            },

            workers = workers

        )

    })

    results <- do.call(
        rbind,
        results
    )

    elapsed <- difftime(

        Sys.time(),

        start_time,

        units = "secs"

    )

    rownames(results) <- NULL

    failed <- results$status == "failed"

    if (any(failed)) {

        warning(
            sum(failed),
            " download(s) failed.",
            call. = FALSE
        )
    }

    successful <- results[
        !failed,
        ,
        drop = FALSE
    ]

    cat("\n")

    cat("------------------------------------\n")

    cat("Sentinel-2 download complete\n\n")

    cat(
        sprintf(
            "Scenes       : %d\n",
            length(unique(queue$scene))
        )
    )

    cat(
        sprintf(
            "Files        : %d\n",
            nrow(queue)
        )
    )

    cat(
        sprintf(
            "Downloaded   : %d\n",
            sum(results$status == "downloaded")
        )
    )

    cat(
        sprintf(
            "Cached       : %d\n",
            sum(results$status == "cached")
        )
    )

    cat(
        sprintf(
            "Failed       : %d\n",
            sum(results$status == "failed")
        )
    )

    cat(
        sprintf(
            "Cache hit    : %.1f%%\n",
            100 * sum(results$status == "cached") / nrow(results)
        )
    )

    cat(
        sprintf(
            "Download hit : %.1f%%\n",
            100 * sum(results$status == "downloaded") / nrow(results)
        )
    )

    cat(
        sprintf(
            "Retries      : %d\n",
            sum(results$attempts > 1)
        )
    )

    cat(
        sprintf(
            "Elapsed      : %s\n",
            format_elapsed(elapsed)
        )
    )

    cat("------------------------------------\n\n")

    new_s2_collection(
        files = successful,
        aoi = x$aoi
    )
}


#----- download queue ----------------------------

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

#-------extract tile -----------------------

extract_s2_tile <- function(scene) {

    tile <- scene$properties[["mgrs:tile"]]

    if (!is.null(tile) &&
        !is.na(tile) &&
        nzchar(tile)) {

        return(
            as.character(tile)
        )

    }

    parts <- strsplit(
        scene$id,
        "_",
        fixed = TRUE
    )[[1]]

    if (length(parts) >= 2)
        return(parts[2])

    NA_character_

}

#------download s2 asset------------------------------------------

download_s2_asset <- function(
        job,
        overwrite = FALSE,
        retries = 4
) {
    outfile <- job$file[[1]]
    url <- job$url[[1]]

    # ------------------------------------------------------------
    # Existing cached file
    # ------------------------------------------------------------

    if (file.exists(outfile) && !overwrite) {

        return(
            data.frame(
                scene = job$scene[[1]],
                tile = job$tile[[1]],
                date = job$date[[1]],
                satellite = job$satellite[[1]],
                asset = job$asset[[1]],
                file = outfile,
                status = "cached",
                attempts = 0L,
                stringsAsFactors = FALSE
            )
        )
    }

    # ------------------------------------------------------------
    # Temporary partial-download file
    # ------------------------------------------------------------

    tmpfile <- paste0(
        outfile,
        ".part"
    )

    success <- FALSE
    attempts <- 0L
    last_error <- NULL

    # ------------------------------------------------------------
    # Download with retry
    # ------------------------------------------------------------

    for (attempt in seq_len(retries)) {

        attempts <- attempt

        if (file.exists(tmpfile)) {
            unlink(tmpfile)
        }

        result <- tryCatch({

            utils::download.file(
                url = url,
                destfile = tmpfile,
                mode = "wb",
                quiet = TRUE,
                method = "libcurl"
            )

            if (!file.exists(tmpfile)) {
                stop(
                    "Download did not create a file."
                )
            }

            size <- file.info(tmpfile)$size

            if (is.na(size) || size <= 0) {
                stop(
                    "Downloaded file is empty."
                )
            }

            TRUE

        }, error = function(e) {

            last_error <<- conditionMessage(e)

            FALSE
        })

        if (isTRUE(result)) {

            success <- TRUE

            break
        }

        # Exponential-ish backoff:
        # 2 sec, 4 sec, 8 sec, 16 sec...

        if (attempt < retries) {

            Sys.sleep(
                2 ^ attempt
            )
        }
    }

    # ------------------------------------------------------------
    # Failed after all attempts
    # ------------------------------------------------------------

    if (!success) {

        if (file.exists(tmpfile)) {
            unlink(tmpfile)
        }

        warning(
            "Failed after ",
            attempts,
            " attempts: ",
            job$scene[[1]],
            " / ",
            job$asset[[1]],
            if (!is.null(last_error))
                paste0(" <e2><80><94> ", last_error)
            else
                "",
            call. = FALSE
        )

        return(
            data.frame(
                scene = job$scene[[1]],
                tile = job$tile[[1]],
                date = job$date[[1]],
                satellite = job$satellite[[1]],
                asset = job$asset[[1]],
                file = outfile,
                status = "failed",
                attempts = attempts,
                stringsAsFactors = FALSE
            )
        )
    }

    # ------------------------------------------------------------
    # Move completed download into cache
    # ------------------------------------------------------------

    if (file.exists(outfile)) {
        unlink(outfile)
    }

    moved <- file.rename(
        tmpfile,
        outfile
    )

    if (!moved) {

        warning(
            "Downloaded file could not be moved into cache: ",
            outfile,
            call. = FALSE
        )

        return(
            data.frame(
                scene = job$scene[[1]],
                tile = job$tile[[1]],
                date = job$date[[1]],
                satellite = job$satellite[[1]],
                asset = job$asset[[1]],
                file = outfile,
                status = "failed",
                attempts = attempts,
                stringsAsFactors = FALSE
            )
        )
    }

    # ------------------------------------------------------------
    # Success
    # ------------------------------------------------------------

    data.frame(
        scene = job$scene[[1]],
        tile = job$tile[[1]],
        date = job$date[[1]],
        satellite = job$satellite[[1]],
        asset = job$asset[[1]],
        file = outfile,
        status = "downloaded",
        attempts = attempts,
        stringsAsFactors = FALSE
    )
}


#-------files ------------------------------------------
files <- function(x) {
    UseMethod("files")
}

#------- new s2 collection -----------------------------

new_s2_collection <- function(
        files,
        aoi = NULL
) {

    stopifnot(
        is.data.frame(files)
    )

    if (!is.null(aoi)) {
        stopifnot(
            inherits(aoi, "sbr_aoi")
        )
    }

    structure(
        list(
            files = files,
            aoi = aoi
        ),
        class = "sbr_collection"
    )
}


