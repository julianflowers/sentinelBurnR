


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
        search = NULL,
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
            aoi = aoi,
            search = search,
            provenance =

                if (!is.null(search))
                    search_provenance(search)
            else
                NULL
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
            packed_geometry = terra::wrap(geometry)
        ),
        class = "sbr_aoi"
    )
}


aoi_geometry <- function(x) {

    stopifnot(
        inherits(x, "sbr_aoi")
    )

    if (!is.null(x$packed_geometry)) {
        return(
            terra::unwrap(x$packed_geometry)
        )
    }

    # Backwards compatibility with existing sbr_aoi objects
    if (!is.null(x$geometry)) {
        return(x$geometry)
    }

    stop(
        "`sbr_aoi` does not contain geometry.",
        call. = FALSE
    )
}



