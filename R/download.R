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
        assets = s2_default_assets,
        limit = NULL,
        output_dir = tools::R_user_dir(
            "sentinelBurnR",
            which = "cache"
        ),
        overwrite = FALSE,
        workers = 1
) {

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

    queue <- build_download_queue(
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



    results <- future.apply::future_lapply(
        seq_len(nrow(queue)),
        function(i) {

            download_s2_file(
                job = queue[i, , drop = FALSE],
                overwrite = overwrite
            )
        },
        future.seed = TRUE
    )

    message(
        "Downloading ",
        nrow(queue),
        " assets using ",
        workers,
        " worker(s)..."
    )

    results <- parallel_apply(

        X = seq_len(nrow(queue)),

        FUN = function(i) {

            download_s2_file(

                job = queue[i, , drop = FALSE],

                overwrite = overwrite

            )

        },

        workers = workers

    )

    results <- do.call(
        rbind,
        results
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

    new_s2_collection(
        successful
    )
}
