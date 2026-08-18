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
#' @export
sbr_cache_clean <- function(

    temp = TRUE,

    downloads = FALSE

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

        temp = cache_temp()

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
#' @param dry_run If TRUE, report what would be deleted.
#'
#' @return Invisibly returns a data frame describing deleted files.
#'
#' @export

sbr_cache_prune <- function(

    max_age = 30,

    downloads = TRUE,

    temp = TRUE,

    dry_run = TRUE

) {

    dirs <- character()

    if (downloads)
        dirs <- c(dirs, cache_downloads())

    if (temp)
        dirs <- c(dirs, cache_temp())

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
