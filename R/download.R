#' Download Sentinel-2 assets
#'
#' Download selected Sentinel-2 assets from an `sbr_search` object.
#' Existing cached files are skipped unless `overwrite = TRUE`.
#'
#' @param x An `sbr_search` object.
#' @param assets Character vector of asset names.
#' @param limit Optional maximum number of scenes to download.
#' @param output_dir Directory used to cache downloaded files.
#' @param overwrite Logical. Overwrite existing files?
#' @param workers Number of parallel download workers.
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
        output_dir = tools::R_user_dir(
            "sentinelBurnR",
            which = "cache"
        ),
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

            "Cloud filter (≤ ",

            max_cloud,

            "%): ",

            length(scenes),

            " of ",

            n_before,

            " scenes retained."

        )


    }

    if (!is.null(limit)) {
        scenes <- head(
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
        successful
    )
}
