make_test_search <- function(
        date = "2026-06-01",
        cloud_cover = 10,
        satellite = "sentinel-2a"
) {

    feature <- list(
        id = paste0("test_scene_", gsub("-", "", date)),
        properties = list(
            datetime = paste0(date, "T10:30:00Z"),
            platform = satellite,
            `eo:cloud_cover` = cloud_cover,
            `s2:cloud_shadow_percentage` = 2,
            `s2:medium_proba_clouds_percentage` = 3,
            `s2:high_proba_clouds_percentage` = 1,
            `s2:vegetation_percentage` = 60,
            `s2:water_percentage` = 5
        )
    )

    structure(
        list(
            items = list(
                features = list(feature)
            ),
            aoi = NULL,
            start = as.Date(date),
            end = as.Date(date)
        ),
        class = "sbr_search"
    )
}


make_test_collection <- function(
        date = "2026-06-01",
        cloud_cover = 10,
        satellite = "sentinel-2a"
) {

    search <- make_test_search(
        date = date,
        cloud_cover = cloud_cover,
        satellite = satellite
    )

    downloads <- data.frame(
        scene = paste0(
            "test_scene_",
            gsub("-", "", date)
        ),
        asset = c(
            "red",
            "nir08",
            "swir16",
            "swir22"
        ),
        file = c(
            "red.tif",
            "nir08.tif",
            "swir16.tif",
            "swir22.tif"
        ),
        stringsAsFactors = FALSE
    )

    new_s2_collection(
        files = downloads,
        search = search
    )
}

