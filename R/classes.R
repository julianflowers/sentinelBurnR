# Constructor for an AOI object
new_aoi <- function(geometry) {

    stopifnot(
        inherits(geometry, "SpatVector")
    )

    structure(
        list(
            geometry = geometry
        ),
        class = "sbr_aoi"
    )
}


# Constructor for a Sentinel-2 search
new_s2_search <- function(items, aoi, start, end) {

    stopifnot(
        inherits(aoi, "sbr_aoi"),
        is.list(items)
    )

    structure(
        list(
            items = items,
            aoi = aoi,
            start = as.Date(start),
            end = as.Date(end)
        ),
        class = "sbr_search"
    )


}

new_s2_collection <- function(files) {

    stopifnot(
        is.data.frame(files)
    )

    structure(
        list(
            files = files
        ),
        class = "sbr_collection"
    )

}

