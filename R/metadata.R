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


burn_caption <- function(x) {

    stopifnot(
        inherits(x, "sbr_burn")
    )

    pre  <- x$provenance$pre$summary
    post <- x$provenance$post$summary

    sprintf(

        paste(

            "Pre-fire: %d acquisitions (%s–%s), mean cloud %.1f%%",

            "\n",

            "Post-fire: %d acquisitions (%s–%s), mean cloud %.1f%%"

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
