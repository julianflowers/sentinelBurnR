#' Clean cache
#'
#' @param temp Remove temporary files.
#' @param downloads Remove downloaded scenes.
#'
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
