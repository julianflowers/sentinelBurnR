extract_s2_tile <- function(scene) {

    tile <- scene$properties[["mgrs:tile"]]

    if (!is.null(tile) &&
        !is.na(tile) &&
        nzchar(tile)) {

        return(
            as.character(tile)
        )
    }

    parts <- strsplit(
        scene$id,
        "_",
        fixed = TRUE
    )[[1]]

    if (length(parts) >= 2L) {
        return(parts[[2]])
    }

    NA_character_
}
