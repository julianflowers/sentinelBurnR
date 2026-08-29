search_provenance <- function(x) {

    stopifnot(
        inherits(x, "sbr_search")
    )

    features <- x$items$features

    scene_table <- do.call(

        rbind,

        lapply(features, function(f) {

            p <- f$properties

            cloud <- p$`eo:cloud_cover`

            if (is.null(cloud) || length(cloud) == 0) {
                cloud <- NA_real_
            }

            if (!is.na(cloud) && (cloud < 0 || cloud > 100)) {
                cloud <- NA_real_
            }


            data.frame(

                acquisition = f$id,

                date = as.Date(
                    substr(
                        p$datetime,
                        1,
                        10
                    )
                ),

                satellite = p$platform,


                cloud_cover = cloud,


                cloud_shadow =
                    p$`s2:cloud_shadow_percentage`,

                medium_cloud =
                    p$`s2:medium_proba_clouds_percentage`,

                high_cloud =
                    p$`s2:high_proba_clouds_percentage`,

                vegetation =
                    p$`s2:vegetation_percentage`,

                water =
                    p$`s2:water_percentage`,

                stringsAsFactors = FALSE

            )

        })

    )

    message("Cloud cover summary")
    print(summary(scene_table$cloud_cover))

    message("Largest values")
    print(
        tail(
            sort(unique(scene_table$cloud_cover)),
            20
        )
    )

    ## ------------------------------------------------------------
    ## Clean obvious missing values
    ## ------------------------------------------------------------

    scene_table$cloud_cover[
        scene_table$cloud_cover > 100
    ] <- NA

    ## ------------------------------------------------------------
    ## One record per acquisition
    ## (same satellite, same date)
    ## ------------------------------------------------------------

    acquisition_table <- stats::aggregate(

        cloud_cover ~ date + satellite,

        data = scene_table,

        FUN = function(x) {

            if (all(is.na(x))) {
                return(NA_real_)
            }

            mean(
                x,
                na.rm = TRUE
            )

        },

        na.action = NULL

    )

    acquisition_table$acquisition <- paste(
        acquisition_table$satellite,
        acquisition_table$date,
        sep = "_"
    )

    acquisition_table <- acquisition_table[
        order(acquisition_table$date),
        ,
        drop = FALSE
    ]
    ## ------------------------------------------------------------
    ## Summary
    ## ------------------------------------------------------------

    summary <- list(

        start = min(
            acquisition_table$date
        ),

        end = max(
            acquisition_table$date
        ),

        n_acquisitions =
            nrow(acquisition_table),

        satellites =
            sort(
                unique(
                    acquisition_table$satellite
                )
            ),

        mean_cloud =
            if (all(is.na(acquisition_table$cloud_cover))) {
                NA_real_
            } else {
                mean(
                    acquisition_table$cloud_cover,
                    na.rm = TRUE
                )
            },

        max_cloud =
            if (all(is.na(acquisition_table$cloud_cover))) {
                NA_real_
            } else {
                max(
                    acquisition_table$cloud_cover,
                    na.rm = TRUE
                )
            }

    )

    list(

        summary = summary,

        scenes = scene_table,

        acquisitions = acquisition_table

    )

}
