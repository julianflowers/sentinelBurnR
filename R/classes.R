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
new_s2_search <- function(
        items,
        aoi,
        query
) {

    stopifnot(
        inherits(aoi, "sbr_aoi")
    )

    structure(
        list(
            items = items,
            aoi = aoi,
            query = query
        ),
        class = "sbr_s2_search"
    )
}
