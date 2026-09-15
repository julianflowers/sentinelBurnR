#' Search Sentinel-2 imagery
#'
#' @param aoi An sbr_aoi object.
#' @param start Start date.
#' @param end End date.
#'
#' @return A raw STAC result.
#'
#' @export
search_s2 <- function(
        aoi,
        start,
        end
) {

    stopifnot(
        inherits(aoi, "sbr_aoi")
    )

    geom <- geometry(aoi)

    bbox <- terra::ext(
        terra::project(
            geom,
            "EPSG:4326"
        )
    )

    bbox <- c(
        terra::xmin(bbox),
        terra::ymin(bbox),
        terra::xmax(bbox),
        terra::ymax(bbox)
    )

    datetime = paste0(
        start,
        "T00:00:00Z/",
        end,
        "T23:59:59Z"
    )

    print(datetime)
    print(bbox)

    q <- rstac::stac(
        "https://earth-search.aws.element84.com/v1"
    ) |>
        rstac::stac_search(
            collections = "sentinel-2-l2a",
            bbox = bbox,
            datetime = datetime,
            limit = 100
        )

    cat("\nbbox:\n")
    print(bbox)
    cat("bbox class:", class(bbox), "\n")

    items <- rstac::post_request(q)

    cat(
        "after post_request:",
        length(items$features),
        "\n"
    )

    items <- rstac::items_fetch(items)

    if (!is.list(items$features)) {
        stop(
            "STAC response does not contain a valid `features` list.",
            call. = FALSE
        )
    }

    cat(
        "after items_fetch:",
        length(items$features),
        "\n"
    )

    new_s2_search(
        items = items$features,
        aoi = aoi,
        start = start,
        end = end
    )

 }


