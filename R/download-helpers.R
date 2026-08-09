# Download one Sentinel-2 asset
#
# @param job One-row data frame from build_download_queue().
# @param overwrite Logical. Overwrite an existing file?
# @param retries Maximum number of download attempts.
#
# @return One-row data frame describing the result.

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
                paste0(" — ", last_error)
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
