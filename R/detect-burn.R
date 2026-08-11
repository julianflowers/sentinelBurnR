detect_burn <- function(

    aoi,

    pre,

    post,

    max_cloud = 40,

    workers = 4

) {

    message(
        "Searching pre-fire imagery..."
    )

    pre_search <- search_s2(

        aoi,

        pre[1],

        pre[2]

    )

    message(
        "Searching post-fire imagery..."
    )

    post_search <- search_s2(

        aoi,

        post[1],

        post[2]

    )

    message(
        "Downloading pre-fire imagery..."
    )

    pre_collection <- download_s2(

        pre_search,

        workers = workers,

        max_cloud = max_cloud

    )

    message(
        "Downloading post-fire imagery..."
    )

    post_collection <- download_s2(

        post_search,

        workers = workers,

        max_cloud = max_cloud

    )

    message(
        "Building composites..."
    )

    pre_comp <- build_composite(
        pre_collection
    )

    post_comp <- build_composite(
        post_collection
    )

    message(
        "Calculating NBR..."
    )

    nbr_pre <- calc_nbr(
        pre_comp
    )

    nbr_post <- calc_nbr(
        post_comp
    )

    message(
        "Calculating dNBR..."
    )

    dnbr <- calc_dnbr(

        nbr_pre,

        nbr_post

    )

    if (!is.null(project)) {

        save_raster(

            pre_comp,

            "pre_composite.tif",

            project,

            "composites"

        )

        save_raster(

            post_comp,

            "post_composite.tif",

            project,

            "composites"

        )

        save_raster(

            nbr_pre,

            "nbr_pre.tif",

            project,

            "indices"

        )

        save_raster(

            nbr_post,

            "nbr_post.tif",

            project,

            "indices"

        )

        save_raster(

            dnbr,

            "dnbr.tif",

            project,

            "indices"

        )

    }

    new_sbr_result(

        aoi = aoi,

        pre_collection = pre_collection,

        post_collection = post_collection,

        pre_composite = pre_comp,

        post_composite = post_comp,

        nbr_pre = nbr_pre,

        nbr_post = nbr_post,

        dnbr = dnbr

    )

}
