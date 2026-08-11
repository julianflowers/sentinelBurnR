save_metadata <- function(
        project,
        pre,
        post,
        max_cloud
) {

    x <- list(

        created = Sys.time(),

        pre = pre,

        post = post,

        max_cloud = max_cloud

    )

    jsonlite::write_json(

        x,

        file.path(
            project$logs,
            "analysis.json"
        ),

        pretty = TRUE,

        auto_unbox = TRUE

    )

}
