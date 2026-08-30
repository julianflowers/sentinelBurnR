#' Build burn provenance
#'
#' Construct provenance metadata for a burn analysis.
#'
#' @param pre Pre-fire Sentinel-2 collection.
#' @param post Post-fire Sentinel-2 collection.
#' @param threshold Burn detection threshold.
#' @param assets Spectral assets used in the analysis.
#'
#' @return A list containing provenance information.
#'
#' @keywords internal
build_provenance <- function(pre, post, threshold, assets) {

    list(

        pre = pre$provenance,

        post = post$provenance,

        processing = list(

            threshold = threshold,
            assets = assets,

            created = Sys.time(),

            package_version =
                as.character(
                    utils::packageVersion(
                        "sentinelBurnR"
                    )
                )

        )

    )

}



# burn caption ------------------------------------------------------------

#' Create a caption describing a burn analysis
#'
#' @param x A burn analysis object.
#'
#' @return A character string suitable for use as a plot caption.
#' @export
#'
burn_caption <- function(x) {

    stopifnot(
        inherits(x, "sbr_burn")
    )

    pre  <- x$provenance$pre$summary
    post <- x$provenance$post$summary

    sprintf(

        paste(

            "Pre-fire: %d acquisitions (%s-%s), mean cloud %.1f%%",

            "\n",

            "Post-fire: %d acquisitions (%s-%s), mean cloud %.1f%%"

        ),

        pre$n_acquisitions,

        format(pre$start, "%d %b %Y"),

        format(pre$end, "%d %b %Y"),

        pre$mean_cloud,

        post$n_acquisitions,

        format(post$start, "%d %b %Y"),

        format(post$end, "%d %b %Y"),

        post$mean_cloud

    )

}
