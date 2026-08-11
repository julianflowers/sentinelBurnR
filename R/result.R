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
