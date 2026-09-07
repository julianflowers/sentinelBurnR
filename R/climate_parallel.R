download_climate_months <- function(
        boundary,
        years,
        months,
        source = "era5",
        cache = cache_climate(),
        variable = "total_precipitation",
        statistic = "daily_sum",
        overwrite = FALSE,
        workers = 1
) {

    libpath <- .libPaths()

    if (source != "era5") {
        stop(
            "Unsupported climate source.",
            call. = FALSE
        )
    }

    if (!requireNamespace("callr", quietly = TRUE)) {
        stop(
            "Package `callr` is required for parallel climate downloads.",
            call. = FALSE
        )
    }

    years <- as.integer(years)
    months <- as.integer(months)
    workers <- as.integer(workers)

    if (
        !length(years) ||
        !length(months) ||
        anyNA(years) ||
        anyNA(months)
    ) {
        stop(
            "`years` and `months` must contain valid values.",
            call. = FALSE
        )
    }

    if (any(months < 1L | months > 12L)) {
        stop(
            "`months` must be between 1 and 12.",
            call. = FALSE
        )
    }

    if (
        length(workers) != 1L ||
        is.na(workers) ||
        workers < 1L
    ) {
        stop(
            "`workers` must be a positive integer.",
            call. = FALSE
        )
    }

    ## Reduce spatial boundary to ordinary numeric values.
    bbox <- era5_bbox(
        read_boundary(boundary)
    )

    requests <- expand.grid(
        year = years,
        month = months,
        KEEP.OUT.ATTRS = FALSE,
        stringsAsFactors = FALSE
    )

    requests <- requests[
        order(requests$year, requests$month),
        ,
        drop = FALSE
    ]

    ## Work out cache filenames in the parent process.
    requests$file <- vapply(
        seq_len(nrow(requests)),
        function(i) {
            climate_cache_file(
                source = source,
                year = requests$year[i],
                month = requests$month[i],
                variable = variable,
                statistic = statistic,
                cache = cache
            )
        },
        character(1)
    )

    ## Anything already cached requires no child process.
    cached <- file.exists(requests$file) & !overwrite

    if (any(cached)) {
        for (i in which(cached)) {
            message(
                sprintf(
                    "Using cached %04d-%02d",
                    requests$year[i],
                    requests$month[i]
                )
            )
        }
    }

    pending <- which(!cached)

    if (!length(pending)) {
        return(requests$file)
    }

    n_total <- nrow(requests)
    n_cached <- n_total - length(pending)

    message(
        sprintf(
            "%d monthly files: %d cached, %d to download",
            n_total,
            n_cached,
            length(pending)
        )
    )

    ## Serial path retains the normal package machinery.
    if (workers == 1L) {

        for (i in pending) {

            message(
                sprintf(
                    "Downloading %04d-%02d...",
                    requests$year[i],
                    requests$month[i]
                )
            )

            download_era5_month(
                bbox = bbox,
                year = requests$year[i],
                month = requests$month[i],
                variable = variable,
                statistic = statistic,
                outfile = requests$file[i]
            )
        }

        n_done <- sum(file.exists(requests$file))

        message(
            sprintf(
                "Completed %04d-%02d [%d/%d, %.0f%%]",
                requests$year[i],
                requests$month[i],
                n_done,
                n_total,
                100 * n_done / n_total
            )
        )

        return(requests$file)
    }

    key <- ecmwfr::wf_get_key()
    libpath <- .libPaths()

    ## Parallel path: independent R processes.
    run_one <- function(i) {

        message(
            sprintf(
                "Starting %04d-%02d",
                requests$year[i],
                requests$month[i]
            )
        )

        callr::r_bg(
            func = function(
                    bbox,
                    year,
                    month,
                    variable,
                    statistic,
                    outfile
                ) {

        sentinelBurnR:::download_era5_month(
                    bbox = bbox,
                    year = year,
                    month = month,
                    variable = variable,
                    statistic = statistic,
                    outfile = outfile
                )
        },

        args = list(
            bbox = bbox,
            year = requests$year[i],
            month = requests$month[i],
            variable = variable,
            statistic = statistic,
            outfile = requests$file[i]
        ),
        libpath = libpath,
        env = c(
            ecmwfr_PAT = key
        ),
        stdout = "|",
        stderr = "|"
        )
    }
    running <- list()
    next_job <- 1L

    while (
        next_job <= length(pending) ||
        length(running) > 0L
    ) {

        ## Fill available worker slots.
        while (
            next_job <= length(pending) &&
            length(running) < workers
        ) {

            i <- pending[next_job]

            running[[as.character(i)]] <- run_one(i)

            next_job <- next_job + 1L
        }

        Sys.sleep(1)

        finished <- vapply(
            running,
            function(p) !p$is_alive(),
            logical(1)
        )

        if (!any(finished)) {
            next
        }

        for (nm in names(running)[finished]) {

            i <- as.integer(nm)
            p <- running[[nm]]

            ## get_result() propagates errors from the child.
            p$get_result()

            if (!file.exists(requests$file[i])) {
                stop(
                    sprintf(
                        "Climate download did not produce file: %04d-%02d",
                        requests$year[i],
                        requests$month[i]
                    ),
                    call. = FALSE
                )
            }

            n_done <- sum(file.exists(requests$file))


            message(
                sprintf(
                    "Completed %04d-%02d [%d/%d, %.0f%%]",
                    requests$year[i],
                    requests$month[i],
                    n_done,
                    n_total,
                    100 * n_done / n_total
                )
            )

            running[[nm]] <- NULL
        }
    }

    requests$file
}
