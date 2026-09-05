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
#'
#' @export
sbr_cache_info <- function() {

    root <- cache_path()

    dirs <- c(

        raw = cache_raw(),

        downloads = cache_downloads(),

        temp = cache_temp(),

        composite = cache_composites()

    )

    size <- function(path) {

        if (!dir.exists(path))
            return(0)

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

    sizes <- vapply(
        dirs,
        size,
        numeric(1)
    )

    total <- sum(sizes)

    cat("\n")

    cat("---------------------------------\n")

    cat("sentinelBurnR cache\n\n")

    for (nm in names(sizes)) {

        cat(

            sprintf(

                "%-12s %7.1f MB\n",

                nm,

                sizes[[nm]] /
                    1024^2

            )

        )

    }

    cat("\n")

    cat(

        sprintf(

            "%-12s %7.1f MB\n",

            "TOTAL",

            total /
                1024^2

        )

    )

    cat("---------------------------------\n")

    invisible(sizes)

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
#'
#' @return Invisibly returns a data frame describing deleted files.
#'
#' @export

sbr_cache_prune <- function(

    max_age = 30,

    downloads = TRUE,

    temp = TRUE,

    composites = FALSE,

    dry_run = TRUE

) {

    dirs <- character()

    if (downloads)
        dirs <- c(dirs, cache_downloads())

    if (temp)
        dirs <- c(dirs, cache_temp())

    if(composites)
        dirs <- c(dirs, cache_composites())

    files <- unlist(

        lapply(

            dirs,

            list.files,

            recursive = TRUE,

            full.names = TRUE

        )

    )

    if (length(files) == 0) {

        message("Cache is empty.")

        return(invisible(NULL))

    }

    info <- file.info(files)

    age_days <-

        as.numeric(

            Sys.time() -

                info$mtime,

            units = "days"

        )

    prune <- age_days > max_age

    if (!any(prune)) {

        message(

            "No cached files older than ",

            max_age,

            " days."

        )

        return(

            invisible(

                info[FALSE, ]

            )

        )

    }

    victims <- files[prune]

    reclaimed <-

        sum(

            info$size[prune],

            na.rm = TRUE

        )

    cat(

        sprintf(

            "%d file(s), %.1f MB\n",

            length(victims),

            reclaimed / 1024^2

        )

    )

    if (!dry_run) {

        unlink(

            victims,

            recursive = FALSE,

            force = TRUE

        )

        message("Cache pruned.")

    } else {

        message(

            "Dry run only. Set dry_run = FALSE to delete."

        )

    }

    invisible(

        data.frame(

            file = victims,

            size = info$size[prune],

            modified = info$mtime[prune],

            age_days = age_days[prune]

        )

    )

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
        assets
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
            if ("scl" %in% f$asset) "scl"
        )
    )

    f <- f[
        f$asset %in% required,
        ,
        drop = FALSE
    ]

    sort_cols <- intersect(
        c("date", "tile", "asset", "file"),
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

    source_files <- normalizePath(
        f$file,
        mustWork = TRUE
    )

    source_md5 <- unname(
        tools::md5sum(source_files)
    )

    aoi_key <- aoi_cache_key(
        collection$aoi
    )

    hash_text(
        paste(
            paste0("version=", composite_cache_version),
            paste(assets, collapse = ","),
            aoi_key,
            paste(source_md5, collapse = "|"),
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
        assets
) {

    key <- composite_cache_key(
        collection,
        assets
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
        aoi <- aoi$geometry
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



