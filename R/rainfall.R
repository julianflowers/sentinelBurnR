#==========================================================
# Rainfall
#==========================================================

#' Retrieve rainfall data
#'
#' Retrieves daily rainfall for a boundary and date range.
#'
#' @param boundary Boundary polygon.
#' @param start Start date.
#' @param end End date.
#' @param project Optional sbr_project.
#'
#' @return A data.frame.
#'
#' @export

get_rainfall <- function(

    boundary,

    start,

    end,

    project = NULL

) {

    boundary <- read_boundary(
        boundary
    )

    start <- as.Date(start)
    end <- as.Date(end)

    if (start > end) {

        stop(
            "`start` must be before `end`.",
            call. = FALSE
        )

    }

    cache <- if (is.null(project)) {

        cache_rainfall()

    } else {

        file.path(
            project$cache,
            "rainfall"
        )

    }

    dir.create(

        cache,

        recursive = TRUE,

        showWarnings = FALSE

    )

    message(
        "Rainfall cache: ",
        cache
    )

    message(
        "Date range: ",
        start,
        " to ",
        end
    )

    invisible(

        data.frame(

            date = seq(
                start,
                end,
                by = "day"
            ),

            rainfall_mm = NA_real_

        )

    )

}
