library(sf)
library(httr2)
library(jsonlite)

download_ngd <- function(
        collection,
        area,
        key = Sys.getenv("OS_API_KEY"),
        limit = 100,
        clip = FALSE,
        quiet = FALSE
) {

    # ---- checks ----

    if (!inherits(area, c("sf", "sfc"))) {
        stop("`area` must be an sf or sfc object.")
    }

    if (!nzchar(key)) {
        stop("No OS API key supplied.")
    }

    if (limit > 100) {
        stop("OS NGD API has a maximum limit of 100.")
    }

    # ---- prepare search area ----

    area_bng <- sf::st_transform(area, 27700)

    bb <- sf::st_bbox(area_bng)

    bbox_string <- paste(
        bb["xmin"],
        bb["ymin"],
        bb["xmax"],
        bb["ymax"],
        sep = ","
    )

    base_url <- paste0(
        "https://api.os.uk/features/ngd/ofa/v1/collections/",
        collection,
        "/items"
    )

    # ---- download ----

    pages <- list()
    offset <- 0

    repeat {

        if (!quiet) {
            message(
                "Requesting features ",
                offset + 1, "–", offset + limit,
                "..."
            )
        }

        response <- httr2::request(base_url) |>
            httr2::req_url_query(
                key = key,
                bbox = bbox_string,
                `bbox-crs` =
                    "http://www.opengis.net/def/crs/EPSG/0/27700",
                crs =
                    "http://www.opengis.net/def/crs/EPSG/0/27700",
                limit = limit,
                offset = offset
            ) |>
            httr2::req_retry(max_tries = 5) |>
            httr2::req_perform()

        # Get raw GeoJSON directly
        geojson <- httr2::resp_body_string(response)

        # Check how many features this page contains
        parsed <- jsonlite::fromJSON(
            geojson,
            simplifyVector = FALSE
        )

        n_page <- length(parsed$features)

        if (!quiet) {
            message("  received ", n_page)
        }

        if (n_page == 0) {
            break
        }

        # Write this API response directly to temporary GeoJSON
        tmp <- tempfile(fileext = ".geojson")

        writeLines(
            geojson,
            tmp,
            useBytes = TRUE
        )

        # Read page directly with sf
        page <- sf::st_read(
            tmp,
            quiet = TRUE
        )

        unlink(tmp)

        # OS returned BNG coordinates
        sf::st_crs(page) <- 27700

        pages[[length(pages) + 1]] <- page

        if (n_page < limit) {
            break
        }

        offset <- offset + limit
    }

    # ---- combine pages ----

    if (length(pages) == 0) {
        return(
            sf::st_sf(
                geometry = sf::st_sfc(crs = 27700)
            )
        )
    }

    x <- do.call(rbind, pages)

    if (!quiet) {
        message(
            "Downloaded ",
            nrow(x),
            " features in total."
        )
    }

    # ---- select features intersecting actual area ----

    hits <- lengths(
        sf::st_intersects(
            x,
            sf::st_union(area_bng)
        )
    ) > 0

    x <- x[hits, ]

    if (!quiet) {
        message(
            nrow(x),
            " features intersect requested area."
        )
    }

    # ---- optionally clip geometry ----

    if (clip) {
        x <- sf::st_intersection(
            x,
            sf::st_union(area_bng)
        )
    }

    x
}
