


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
new_s2_collection <- function(
        files,
        aoi = NULL
) {

    stopifnot(
        is.data.frame(files)
    )

    if (!is.null(aoi)) {
        stopifnot(
            inherits(aoi, "sbr_aoi")
        )
    }

    structure(
        list(
            files = files,
            aoi = aoi
        ),
        class = "sbr_collection"
    )
}


# Constructor for an sbr_aoi object
#
# @param geometry A terra SpatVector.
#
# @return An sbr_aoi object.
#
# @keywords internal

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


