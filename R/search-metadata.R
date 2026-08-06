# Convert Sentinel-2 STAC items to a metadata table
#
# @param items A STAC item collection returned by rstac.
#
# @return A data frame with one row per scene.
#
# @keywords internal
s2_item_metadata <- function(items) {

    features <- items$features

    if (is.null(features) || length(features) == 0L) {
        return(
            data.frame(
                id = character(),
                datetime = as.POSIXct(character(), tz = "UTC"),
                date = as.Date(character()),
                cloud_cover = numeric(),
                tile = character(),
                platform = character(),
                stringsAsFactors = FALSE
            )
        )
    }

    rows <- lapply(
        features,
        function(item) {

            properties <- item$properties

            datetime_text <- properties$datetime

            datetime <- as.POSIXct(
                datetime_text,
                format = "%Y-%m-%dT%H:%M:%OSZ",
                tz = "UTC"
            )

            cloud_cover <- properties[["eo:cloud_cover"]]

            if (is.null(cloud_cover)) {
                cloud_cover <- NA_real_
            }

            tile <- properties[["mgrs:tile"]]

            if (is.null(tile)) {
                tile <- NA_character_
            }

            platform <- properties$platform

            if (is.null(platform)) {
                platform <- NA_character_
            }

            data.frame(
                id = item$id,
                datetime = datetime,
                date = as.Date(datetime),
                cloud_cover = as.numeric(cloud_cover),
                tile = as.character(tile),
                platform = as.character(platform),
                stringsAsFactors = FALSE
            )
        }
    )

    result <- do.call(
        rbind,
        rows
    )

    rownames(result) <- NULL

    result[order(result$datetime), ]
}
