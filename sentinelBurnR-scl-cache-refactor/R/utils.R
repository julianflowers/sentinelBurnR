format_elapsed <- function(x) {

    x <- round(as.numeric(x))

    h <- x %/% 3600

    m <- (x %% 3600) %/% 60

    s <- x %% 60

    if (h > 0) {

        sprintf("%dh %02dm %02ds", h, m, s)

    } else if (m > 0) {

        sprintf("%dm %02ds", m, s)

    } else {

        sprintf("%ds", s)

    }

}

new_sbr_result <- function(

    aoi,

    pre_collection,

    post_collection,

    pre_composite,

    post_composite,

    nbr_pre,

    nbr_post,

    dnbr

) {

    structure(

        list(

            aoi = aoi,

            pre_collection = pre_collection,

            post_collection = post_collection,

            pre_composite = pre_composite,

            post_composite = post_composite,

            nbr_pre = nbr_pre,

            nbr_post = nbr_post,

            dnbr = dnbr

        ),

        class = "sbr_result"

    )

}
