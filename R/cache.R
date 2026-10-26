cache_path <- function() {

    path <- tools::R_user_dir(
        "sentinelBurnR",
        which = "cache"
    )

    dir.create(
        path,
        recursive = TRUE,
        showWarnings = FALSE
    )

    path

}

cache_raw <- function() {

    path <- file.path(
        cache_path(),
        "raw"
    )

    dir.create(
        path,
        recursive = TRUE,
        showWarnings = FALSE
    )

    path

}

cache_temp <- function() {

    path <- file.path(
        cache_path(),
        "temp"
    )

    dir.create(
        path,
        recursive = TRUE,
        showWarnings = FALSE
    )

    path

}

cache_downloads <- function() {

    path <- file.path(
        cache_path(),
        "downloads"
    )

    dir.create(
        path,
        recursive = TRUE,
        showWarnings = FALSE
    )

    path

}


# cache clean -------------------------------------------------------------
#' Clean cache
#'
#' @param temp Remove temporary files.
#' @param downloads Remove downloaded scenes.
#' @param composites Remove cached sentinel-2 composites
#' @export
sbr_cache_clean <- function(

    temp = TRUE,

    downloads = FALSE,

    composites = FALSE

) {

    if (temp) {

        unlink(

            cache_temp(),

            recursive = TRUE

        )

    }

    if (downloads) {

        unlink(

            cache_downloads(),

            recursive = TRUE

        )

    }

    if (composites) {

            unlink(

                cache_composites(),

                recursive = TRUE

        )

    }

    invisible(TRUE)

}


# cache info --------------------------------------------------------------
#' Cache information
#' @param breakdown Character. Level of cache detail to report.
#'   One of `"cache"`, `"asset"`, or `"year"`.
#' @export
sbr_cache_info <- function(
        breakdown = c("cache", "asset", "year")
) {

    breakdown <- match.arg(breakdown)

    dirs <- c(
        raw = cache_raw(),
        downloads = cache_downloads(),
        temp = cache_temp(),
        composite = cache_composites()
    )

    file_size <- function(path) {

        if (!dir.exists(path)) {
            return(0)
        }

        files <- list.files(
            path,
            recursive = TRUE,
            full.names = TRUE
        )

        if (length(files) == 0) {
            return(0)
        }

        sum(
            file.info(files)$size,
            na.rm = TRUE
        )
    }

    format_size <- function(bytes) {

        if (bytes >= 1024^3) {
            sprintf("%.1f GB", bytes / 1024^3)
        } else {
            sprintf("%.1f MB", bytes / 1024^2)
        }
    }

    # ------------------------------------------------------
    # Cache-level summary
    # ------------------------------------------------------

    if (breakdown == "cache") {

        sizes <- vapply(
            dirs,
            file_size,
            numeric(1)
        )

        total <- sum(sizes)

        cat("\n")
        cat("---------------------------------\n")
        cat("sentinelBurnR cache\n\n")

        for (nm in names(sizes)) {

            cat(
                sprintf(
                    "%-12s %10s\n",
                    nm,
                    format_size(sizes[[nm]])
                )
            )
        }

        cat("\n")

        cat(
            sprintf(
                "%-12s %10s\n",
                "TOTAL",
                format_size(total)
            )
        )

        cat("---------------------------------\n")

        return(
            invisible(sizes)
        )
    }

    # ------------------------------------------------------
    # Download-level summaries
    # ------------------------------------------------------

    path <- cache_downloads()

    files <- list.files(
        path,
        recursive = TRUE,
        full.names = TRUE
    )

    if (length(files) == 0) {
        message("Download cache is empty.")
        return(invisible(NULL))
    }

    info <- file.info(files)

    x <- data.frame(
        file = files,
        size = info$size,
        stringsAsFactors = FALSE
    )

    x <- x[
        !is.na(x$size) &
            !info$isdir,
        ,
        drop = FALSE
    ]

    x$asset <- tools::file_path_sans_ext(
        basename(x$file)
    )

    scene <- basename(
        dirname(x$file)
    )

    # Scene names contain dates such as:
    # S2B_31UCT_20260716_0_L2A

    date_string <- sub(
        "^.*_([0-9]{8})_[0-9]+_L2A$",
        "\\1",
        scene
    )

    x$year <- suppressWarnings(
        as.integer(
            substr(
                date_string,
                1,
                4
            )
        )
    )

    if (breakdown == "asset") {

        out <- stats::aggregate(
            size ~ asset,
            data = x,
            FUN = sum
        )

        counts <- table(x$asset)

        out$files <- as.integer(
            counts[out$asset]
        )

        out$size_gb <- out$size / 1024^3

        out <- out[
            order(
                out$size,
                decreasing = TRUE
            ),
        ]

        out$size <- NULL

    } else {

        out <- stats::aggregate(
            size ~ year,
            data = x,
            FUN = sum
        )

        counts <- table(x$year)

        out$files <- as.integer(
            counts[as.character(out$year)]
        )

        out$size_gb <- out$size / 1024^3

        out <- out[
            order(out$year),
        ]

        out$size <- NULL
    }

    rownames(out) <- NULL

    print(
        out,
        row.names = FALSE
    )

    invisible(out)
}

#' Remove old cached files
#'
#' Deletes cached files older than a specified age.
#'
#' @param max_age Maximum age (days) to retain.
#' @param downloads Prune download cache.
#' @param temp Prune temporary files.
#' @param composites Prune composites cache.
#' @param dry_run If TRUE, report what would be deleted.
#' @param asset Optional character vector of Sentinel-2 asset names
#'   to prune, for example `"visual"` or `"red"`. Applies to the
#'   downloads cache only.
#' @param year Optional numeric or integer vector of acquisition
#'   years to prune, for example `2025` or `2020:2024`.
#' @return Invisibly returns a data frame describing deleted files.
#'
#' @export

sbr_cache_prune <- function(
        max_age = 30,
        downloads = TRUE,
        temp = TRUE,
        composites = FALSE,
        asset = NULL,
        year = NULL,
        dry_run = TRUE
) {

    if (!is.null(max_age)) {

        if (
            length(max_age) != 1 ||
            !is.numeric(max_age) ||
            is.na(max_age) ||
            max_age < 0
        ) {
            stop(
                "`max_age` must be NULL or a non-negative number.",
                call. = FALSE
            )
        }
    }

    if (!is.null(year)) {

        if (
            !is.numeric(year) ||
            anyNA(year)
        ) {
            stop(
                "`year` must be NULL or a numeric year.",
                call. = FALSE
            )
        }

        year <- as.integer(year)
    }

    if (!is.null(asset)) {
        asset <- as.character(asset)
    }

    paths <- character()

    if (downloads) {
        paths <- c(
            paths,
            cache_downloads()
        )
    }

    if (temp) {
        paths <- c(
            paths,
            cache_temp()
        )
    }

    if (composites) {
        paths <- c(
            paths,
            cache_composites()
        )
    }

    if (length(paths) == 0) {
        message("No cache components selected.")
        return(invisible(character()))
    }

    files <- unlist(
        lapply(
            paths,
            function(path) {

                if (!dir.exists(path)) {
                    return(character())
                }

                list.files(
                    path,
                    recursive = TRUE,
                    full.names = TRUE
                )
            }
        ),
        use.names = FALSE
    )

    if (length(files) == 0) {
        message("No cached files found.")
        return(invisible(character()))
    }

    info <- file.info(files)

    keep <- !info$isdir

    # ------------------------------------------------------
    # Age filter
    # ------------------------------------------------------

    if (!is.null(max_age)) {

        cutoff <- Sys.time() -
            max_age * 24 * 60 * 60

        keep <- keep &
            info$mtime < cutoff
    }

    # ------------------------------------------------------
    # Asset filter
    #
    # Only meaningful for files inside downloads.
    # ------------------------------------------------------

    if (!is.null(asset)) {

        download_root <- normalizePath(
            cache_downloads(),
            mustWork = FALSE
        )

        normalized <- normalizePath(
            files,
            mustWork = FALSE
        )

        in_downloads <- startsWith(
            normalized,
            paste0(
                download_root,
                .Platform$file.sep
            )
        )

        file_asset <- tools::file_path_sans_ext(
            basename(files)
        )

        keep <- keep &
            in_downloads &
            file_asset %in% asset
    }

    # ------------------------------------------------------
    # Year filter
    # ------------------------------------------------------

    if (!is.null(year)) {

        scene <- basename(
            dirname(files)
        )

        date_string <- sub(
            "^.*_([0-9]{8})_[0-9]+_L2A$",
            "\\1",
            scene
        )

        file_year <- suppressWarnings(
            as.integer(
                substr(
                    date_string,
                    1,
                    4
                )
            )
        )

        keep <- keep &
            !is.na(file_year) &
            file_year %in% year
    }

    remove <- files[keep]

    if (length(remove) == 0) {
        message("No cache files match the pruning criteria.")
        return(invisible(character()))
    }

    bytes <- sum(
        file.info(remove)$size,
        na.rm = TRUE
    )

    cat(
        sprintf(
            "%d files matched (%.2f GB).\n",
            length(remove),
            bytes / 1024^3
        )
    )

    if (dry_run) {

        cat(
            "Dry run only; no files removed.\n"
        )

        return(
            invisible(remove)
        )
    }

    remove_size <- file.info(remove)$size

    ok <- file.remove(remove)

    if (!all(ok)) {
        warning(
            sum(!ok),
            " file(s) could not be removed.",
            call. = FALSE
        )
    }

    removed_bytes <- sum(
        remove_size[ok],
        na.rm = TRUE
    )

    # Remove directories left empty by pruning.

    if (downloads) {

        dirs <- list.dirs(
            cache_downloads(),
            recursive = TRUE,
            full.names = TRUE
        )

        dirs <- dirs[
            dirs != cache_downloads()
        ]

        dirs <- dirs[
            order(
                nchar(dirs),
                decreasing = TRUE
            )
        ]

        for (dir in dirs) {

            if (
                length(
                    list.files(
                        dir,
                        all.files = TRUE,
                        no.. = TRUE
                    )
                ) == 0
            ) {
                unlink(dir)
            }
        }
    }

    cat(
        sprintf(
            "%d files removed (%.2f GB).\n",
            sum(ok),
            removed_bytes / 1024^3
        )
    )

    invisible(remove[ok])
}

cache_size <- function(path) {

    files <- list.files(
        path,
        recursive = TRUE,
        full.names = TRUE
    )

    if (length(files) == 0)
        return(0)

    sum(
        file.info(files)$size,
        na.rm = TRUE
    )

}


# cache climate ----------------------------------------------------------

cache_climate <- function() {

    path <- file.path(
        tools::R_user_dir(
            "sentinelBurnR",
            "cache"
        ),
        "climate"
    )

    dir.create(
        path,
        recursive = TRUE,
        showWarnings = FALSE
    )

    path

}


# cache key ---------------------------------------------------------------

composite_cache_key <- function(
        collection,
        assets,
        mask_clouds = FALSE,
        scl_keep = s2_scl_keep
) {

    f <- files(collection)

    required_cols <- c(
        "asset",
        "file"
    )

    missing_cols <- setdiff(
        required_cols,
        names(f)
    )

    if (length(missing_cols)) {

        stop(
            "Collection file table is missing required column(s): ",
            paste(missing_cols, collapse = ", "),
            ".",
            call. = FALSE
        )
    }

    # Only source assets relevant to this composite.
    # SCL also affects the result when available.
    required <- unique(
        c(
            assets,
            if (mask_clouds) "scl"
        )
    )

    f <- f[
        f$asset %in% required,
        ,
        drop = FALSE
    ]

    sort_cols <- intersect(
        c("date", "tile", "asset", "file", "satellite"),
        names(f)
    )

    f <- f[
        do.call(
            order,
            unname(f[sort_cols])
        ),
        ,
        drop = FALSE
    ]

    source_key <- apply(
        f[sort_cols],
        1,
        paste,
        collapse = "|"
    )

    aoi_key <- aoi_cache_key(
        collection$aoi
    )

    hash_text(
        paste(
            paste0("version=", composite_cache_version),
            paste0("mask_clouds=", mask_clouds),
            paste0(
                "scl_keep=",
                if (mask_clouds) {
                    paste(sort(unique(scl_keep)), collapse = ",")
                } else {
                    "none"
                }
            ),
            paste(assets, collapse = ","),
            aoi_key,
            paste(source_key, collapse = "|"),
            sep = "\n"
        )
    )
}


# composite cache ---------------------------------------------------------

composite_cache_version <- 1L


cache_composites <- function() {

    path <- file.path(
        cache_path(),
        "composites"
    )

    dir.create(
        path,
        recursive = TRUE,
        showWarnings = FALSE
    )

    path
}



# composite cache files ---------------------------------------------------------

composite_cache_file <- function(
        collection,
        assets,
        mask_clouds = TRUE,
        scl_keep = s2_scl_keep
) {

    key <- composite_cache_key(
        collection,
        assets,
        mask_clouds = mask_clouds,
        scl_keep = scl_keep
    )

    file.path(
        cache_composites(),
        paste0(
            "composite_",
            key,
            ".tif"
        )
    )
}


# hashing  ----------------------------------------------------------------

hash_text <- function(x) {

    tmp <- tempfile()
    on.exit(unlink(tmp), add = TRUE)

    writeLines(
        enc2utf8(x),
        tmp,
        useBytes = TRUE
    )

    unname(
        tools::md5sum(tmp)
    )
}

aoi_cache_key <- function(aoi) {

    if (is.null(aoi)) {
        return("no-aoi")
    }

    if (inherits(aoi, "sbr_aoi")) {
        aoi <- aoi_geometry(aoi)
    }

    if (!inherits(aoi, "SpatVector")) {
        stop(
            "`aoi` must be an sbr_aoi or SpatVector.",
            call. = FALSE
        )
    }

    geom <- terra::as.data.frame(
        aoi,
        geom = "WKT"
    )

    wkt <- geom$geometry

    hash_text(
        paste(
            terra::crs(aoi),
            paste(wkt, collapse = "|"),
            sep = "\n"
        )
    )
}



