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

