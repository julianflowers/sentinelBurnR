
# download climate months -------------------------------------------------


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

    jobs <- unlist(
        lapply(
            years,
            function(year) {
                make_climate_download_jobs(
                    year = year,
                    months = months,
                    source = source,
                    variable = variable,
                    statistic = statistic,
                    cache = cache
                )
            }
        ),
        recursive = FALSE
    )

    n_jobs <- length(jobs)

    if (n_jobs == 0L) {
        message("All requested climate data are already cached.")

        files <- unique(
            unlist(
                lapply(
                    years,
                    function(year) {
                        vapply(
                            months,
                            function(month) {
                                find_climate_cache(
                                    source = source,
                                    year = year,
                                    month = month,
                                    variable = variable,
                                    statistic = statistic,
                                    cache = cache
                                )
                            },
                            character(1)
                        )
                    }
                )
            )
        )

        return(files)
    }

    n_months <- sum(
        vapply(
            jobs,
            function(x) length(x$months),
            integer(1)
        )
    )

    message(
        sprintf(
            "%d months missing; %d download jobs (%d workers)",
            n_months,
            n_jobs,
            workers
        )
    )

    ## Serial path
    if (workers == 1L) {

        for (i in seq_along(jobs)) {

            job <- jobs[[i]]

            month_label <- paste(
                sprintf("%02d", job$months),
                collapse = "-"
            )

            message(
                sprintf(
                    "Downloading %04d-%s...",
                    job$year,
                    month_label
                )
            )

            run_climate_download_job(
                job = job,
                bbox = bbox,
                variable = variable,
                statistic = statistic
            )

            message(
                sprintf(
                    "Completed %04d-%s [%d/%d jobs, %.0f%%]",
                    job$year,
                    month_label,
                    i,
                    n_jobs,
                    100 * i / n_jobs
                )
            )
        }

    } else {

        key <- ecmwfr::wf_get_key()
        libpath <- .libPaths()

        run_one <- function(i) {

            job <- jobs[[i]]

            month_label <- paste(
                sprintf("%02d", job$months),
                collapse = "-"
            )

            message(
                sprintf(
                    "Starting %04d-%s",
                    job$year,
                    month_label
                )
            )

            callr::r_bg(
                func = function(
        job,
        bbox,
        variable,
        statistic
                ) {
                    sentinelBurnR:::run_climate_download_job(
                        job = job,
                        bbox = bbox,
                        variable = variable,
                        statistic = statistic
                    )
                },
        args = list(
            job = job,
            bbox = bbox,
            variable = variable,
            statistic = statistic
        ),
        env = c(
            ecmwfr_PAT = key
        ),
        libpath = libpath,
        stdout = "|",
        stderr = "|"
            )
        }

        running <- list()
        next_job <- 1L
        n_done <- 0L

        while (
            next_job <= n_jobs ||
            length(running) > 0L
        ) {

            while (
                next_job <= n_jobs &&
                length(running) < workers
            ) {

                i <- next_job

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
                job <- jobs[[i]]

                p$get_result()

                if (!file.exists(job$outfile)) {
                    stop(
                        sprintf(
                            "Climate download did not produce file: %s",
                            job$outfile
                        ),
                        call. = FALSE
                    )
                }

                n_done <- n_done + 1L

                month_label <- paste(
                    sprintf("%02d", job$months),
                    collapse = "-"
                )

                message(
                    sprintf(
                        "Completed %04d-%s [%d/%d jobs, %.0f%%]",
                        job$year,
                        month_label,
                        n_done,
                        n_jobs,
                        100 * n_done / n_jobs
                    )
                )

                running[[nm]] <- NULL
            }
        }
    }

    ## Return all files required for the requested period,
    ## including files that were already cached.
    files <- unique(
        unlist(
            lapply(
                years,
                function(year) {
                    vapply(
                        months,
                        function(month) {
                            find_climate_cache(
                                source = source,
                                year = year,
                                month = month,
                                variable = variable,
                                statistic = statistic,
                                cache = cache
                            )
                        },
                        character(1)
                    )
                }
            )
        )
    )

    files
}

# download parallel era5 --------------------------------------------------

download_era5_period <- function(
        bbox,
        year,
        months,
        outfile,
        variable = "total_precipitation",
        statistic = "daily_sum",
        max_tries = 5,
        time_out = 7200
) {

    year <- as.integer(year)
    months <- sort(unique(as.integer(months)))

    request <- era5_request(
        variable = variable,
        daily_statistic = statistic
    )

    request$year <- sprintf("%04d", year)
    request$month <- sprintf("%02d", months)

    ## Request days 01-31. CDS will return only valid dates
    ## for each requested month.
    request$day <- sprintf("%02d", 1:31)

    request$area <- bbox
    request$target <- basename(outfile)

    for (attempt in seq_len(max_tries)) {

        message(
            sprintf(
                "ERA5 %04d months %s, attempt %d/%d",
                year,
                paste(sprintf("%02d", months), collapse = ","),
                attempt,
                max_tries
            )
        )

        result <- tryCatch(
            {
                ecmwfr::wf_request(
                    request = request,
                    transfer = TRUE,
                    path = dirname(outfile),
                    time_out = time_out
                )
                TRUE
            },
            error = function(e) {
                message(conditionMessage(e))
                FALSE
            }
        )

        if (result && file.exists(outfile)) {
            return(outfile)
        }

        if (attempt < max_tries) {
            Sys.sleep(10 * 2^(attempt - 1))
        }
    }

    stop(
        sprintf(
            "ERA5 download failed for %04d months %s.",
            year,
            paste(months, collapse = ",")
        ),
        call. = FALSE
    )
}

split_era5_months <- function(x) {

    if (is.character(x)) {
        x <- terra::rast(x)
    }

    dates <- as.Date(terra::time(x))

    if (length(dates) != terra::nlyr(x)) {
        stop(
            "ERA5 raster must have one date per layer.",
            call. = FALSE
        )
    }

    groups <- format(dates, "%Y-%m")

    lapply(
        unique(groups),
        function(group) {
            x[[groups == group]]
        }
    ) |>
        stats::setNames(unique(groups))
}

cache_era5_period <- function(
        file,
        source,
        variable,
        statistic,
        cache
) {

    rasters <- split_era5_months(file)

    out <- character(length(rasters))

    for (i in seq_along(rasters)) {

        ym <- names(rasters)[i]

        year <- as.integer(substr(ym, 1, 4))
        month <- as.integer(substr(ym, 6, 7))

        outfile <- climate_cache_file(
            source = source,
            year = year,
            month = month,
            variable = variable,
            statistic = statistic,
            cache = cache
        )

        dir.create(
            dirname(outfile),
            recursive = TRUE,
            showWarnings = FALSE
        )

        r <- rasters[[i]]

        unit <- unique(terra::units(r))

        if (length(unit) != 1L || !nzchar(unit)) {
            unit <- ""
        }

        terra::writeCDF(
            r,
            outfile,
            overwrite = TRUE,
            varname = variable,
            longname = variable,
            unit = unit
        )

        out[i] <- outfile
    }

    out
}

climate_period_cache_file <- function(
        source,
        year,
        months,
        variable,
        statistic,
        cache
) {

    months <- sort(unique(as.integer(months)))

    if (length(months) < 2L) {
        stop(
            "Period cache requires at least two months.",
            call. = FALSE
        )
    }

    dir <- file.path(
        cache,
        source,
        variable,
        sprintf("%04d", year)
    )

    month_string <- paste(
        sprintf("%02d", range(months)),
        collapse = "-"
    )

    file.path(
        dir,
        sprintf(
            "%04d_%s_%s.nc",
            year,
            month_string,
            statistic
        )
    )
}

climate_month_batches <- function(months, batch_size = 2L) {

    months <- sort(unique(as.integer(months)))

    split(
        months,
        ceiling(seq_along(months) / batch_size)
    ) |>
        unname()
}

make_climate_batches <- function(requests, batch_size = 2L) {

    pending <- requests[!file.exists(requests$file), , drop = FALSE]

    if (nrow(pending) == 0L) {
        return(list())
    }

    by_year <- split(pending, pending$year)

    batches <- list()

    for (year_requests in by_year) {

        year_requests <- year_requests[
            order(year_requests$month),
            ,
            drop = FALSE
        ]

        months <- year_requests$month

        # Identify runs of consecutive months
        run <- cumsum(c(TRUE, diff(months) != 1L))

        runs <- split(months, run)

        for (x in runs) {

            # Split each consecutive run into batches of at most batch_size
            groups <- split(
                x,
                ceiling(seq_along(x) / batch_size)
            )

            batches <- c(batches, unname(groups))
        }
    }

    batches
}

find_climate_cache <- function(
        source,
        year,
        month,
        variable,
        statistic,
        cache
) {

    # First prefer the conventional monthly cache file
    monthly <- climate_cache_file(
        source = source,
        year = year,
        month = month,
        variable = variable,
        statistic = statistic,
        cache = cache
    )

    if (file.exists(monthly)) {
        return(monthly)
    }

    # Otherwise look for period files in the same directory
    dir <- dirname(monthly)

    if (!dir.exists(dir)) {
        return(NA_character_)
    }

    period_files <- list.files(
        dir,
        pattern = sprintf(
            "^%04d_[0-9]{2}-[0-9]{2}_%s\\.nc$",
            year,
            statistic
        ),
        full.names = TRUE
    )

    if (length(period_files) == 0L) {
        return(NA_character_)
    }

    # Determine which period contains the requested month
    for (file in period_files) {

        name <- basename(file)

        match <- regexec(
            sprintf(
                "^%04d_([0-9]{2})-([0-9]{2})_%s\\.nc$",
                year,
                statistic
            ),
            name
        )

        parts <- regmatches(name, match)[[1]]

        if (length(parts) != 3L) {
            next
        }

        first <- as.integer(parts[2])
        last  <- as.integer(parts[3])

        if (month >= first && month <= last) {
            return(file)
        }
    }

    NA_character_
}


# make climate job --------------------------------------------------------


make_climate_download_jobs <- function(
        year,
        months,
        source,
        variable,
        statistic,
        cache,
        batch_size = 2L
) {

    months <- sort(unique(as.integer(months)))

    # Find months that are not already satisfied by either
    # a monthly or period cache file.
    cached <- vapply(
        months,
        function(month) {
            !is.na(
                find_climate_cache(
                    source = source,
                    year = year,
                    month = month,
                    variable = variable,
                    statistic = statistic,
                    cache = cache
                )
            )
        },
        logical(1)
    )

    pending <- months[!cached]

    if (length(pending) == 0L) {
        return(list())
    }

    # Don't combine months across gaps caused by cached months.
    run <- cumsum(c(TRUE, diff(pending) != 1L))
    runs <- split(pending, run)

    batches <- unlist(
        lapply(
            runs,
            function(x) {
                split(
                    x,
                    ceiling(seq_along(x) / batch_size)
                )
            }
        ),
        recursive = FALSE
    )

    lapply(
        batches,
        function(x) {

            if (length(x) == 1L) {

                outfile <- climate_cache_file(
                    source = source,
                    year = year,
                    month = x,
                    variable = variable,
                    statistic = statistic,
                    cache = cache
                )

            } else {

                outfile <- climate_period_cache_file(
                    source = source,
                    year = year,
                    months = x,
                    variable = variable,
                    statistic = statistic,
                    cache = cache
                )
            }

            list(
                year = as.integer(year),
                months = as.integer(x),
                outfile = outfile
            )
        }
    )
}


# climate job -------------------------------------------------------------


run_climate_download_job <- function(
        job,
        bbox,
        variable,
        statistic
) {

    if (file.exists(job$outfile)) {
        return(job$outfile)
    }

    dir.create(
        dirname(job$outfile),
        recursive = TRUE,
        showWarnings = FALSE
    )

    if (length(job$months) == 1L) {

        download_era5_month(
            boundary = NULL,
            bbox = bbox,
            year = job$year,
            month = job$months,
            variable = variable,
            statistic = statistic,
            outfile = job$outfile
        )

    } else {

        download_era5_period(
            bbox = bbox,
            year = job$year,
            months = job$months,
            variable = variable,
            statistic = statistic,
            outfile = job$outfile
        )
    }

    job$outfile
}
