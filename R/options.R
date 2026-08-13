# Default package options

.onLoad <- function(libname, pkgname) {

    op <- options()

    op.sbr <- list(

        sbr.project_dir = NULL,

        sbr.cache_dir = tools::R_user_dir(
            "sentinelBurnR",
            "cache"
        ),

        sbr.temp_dir = tempdir()

    )

    toset <- !(names(op.sbr) %in% names(op))

    if (any(toset)) {

        options(
            op.sbr[toset]
        )

    }

}

sbr_options <- function(

    project_dir = NULL,

    cache_dir = NULL,

    temp_dir = NULL

) {

    if (!is.null(project_dir))


        dir.create(
            project_dir,
            recursive = TRUE,
            showWarnings = FALSE
        )

        options(
            sbr.project_dir = project_dir
        )

    if (!is.null(cache_dir))
        options(
            sbr.cache_dir = cache_dir
        )

    if (!is.null(temp_dir)) {

        dir.create(
            temp_dir,
            recursive = TRUE,
            showWarnings = FALSE
        )

        options(
            sbr.temp_dir = temp_dir
        )

        terra::terraOptions(
            tempdir = cache_temp()
        )

    }

    invisible(NULL)

}
