library(sentinelBurnR)

make_demo_collection <- function(
        collection,
        aoi,
        name,
        demo_dir = "inst/extdata/demo"
) {

    if (inherits(x, "sbr_collection")) {

        x$files$file <- file.path(
            demo_dir,
            x$files$file
        )

        demo_aoi <- readRDS(
            file.path(
                demo_dir,
                "demo_aoi.rds"
            )
        )

        aoi <- read_aoi(demo_aoi)

        x$aoi <- aoi
        x$search$aoi <- aoi
    }

     raster_dir <- file.path(
        demo_dir,
        "sentinel",
        name
    )

    dir.create(
        raster_dir,
        recursive = TRUE,
        showWarnings = FALSE
    )

    demo_collection <- collection

    new_files <- purrr::map2_chr(
        collection$files$file,
        seq_len(nrow(collection$files)),
        \(file, i) {

            row <- collection$files[i, ]

            r <- terra::rast(file)

            boundary <- if (inherits(aoi, "SpatVector")) {
                aoi
            } else {
                terra::vect(aoi)
            }

            boundary <- terra::project(
                boundary,
                terra::crs(r)
            )

            r <- terra::crop(
                r,
                boundary
            )

            outfile <- file.path(
                raster_dir,
                paste0(
                    row$scene,
                    "_",
                    row$asset,
                    ".tif"
                )
            )

            terra::writeRaster(
                r,
                outfile,
                overwrite = TRUE
            )

            file.path(
                "sentinel",
                name,
                basename(outfile)
            )
        }
    )

    demo_collection$files$file <- new_files

    demo_collection
}

## ------------------------------------------------------------------
## Assets
## ------------------------------------------------------------------

demo_assets <- unique(c(
    s2_burn_assets,
    s2_vegetation_assets
))


## ------------------------------------------------------------------
## AOI
## ------------------------------------------------------------------

aoi <- sentinelBurnR:::read_aoi(
    system.file(
        "extdata",
        "dunwich.gpkg",
        package = "sentinelBurnR"
    )
)

demo_aoi <- sf::st_as_sf(aoi$geometry)

saveRDS(
    demo_aoi,
    "inst/extdata/demo/demo_aoi.rds"
)

## ------------------------------------------------------------------
## Search
## ------------------------------------------------------------------

pre_search <- search_s2(
    aoi,
    start = "2026-07-14",
    end   = "2026-07-29"
)

post_search <- search_s2(
    aoi,
    start = "2026-07-29",
    end   = "2026-08-22"
)

## ------------------------------------------------------------------
## Download
## ------------------------------------------------------------------

pre_collection <- download_s2(
    pre_search,
    limit = 2,
    workers = 1,
    assets = demo_assets

)

post_collection <- download_s2(
    post_search,
    limit = 2,
    workers = 1,
    assets = demo_assets
)

## ------------------------------------------------------------------
## Demo collections
## ------------------------------------------------------------------


demo_pre_collection <- make_demo_collection(
    collection = pre_collection,
    aoi = aoi$geometry,
    name = "pre"
)

demo_post_collection <- make_demo_collection(
    collection = post_collection,
    aoi = aoi$geometry,
    name = "post"
)

## ------------------------------------------------------------------
## Burn analysis
## ------------------------------------------------------------------

burn <- analyse_burn(
    pre_collection,
    post_collection
)

## ------------------------------------------------------------------
## Vegetation analysis
## ------------------------------------------------------------------

vegetation <- analyse_vegetation(
    pre_collection
)

## ------------------------------------------------------------------
## True colour images
## ------------------------------------------------------------------

make_demo_visual <- function(
        search,
        aoi,
        name,
        date,
        tiles = c("31UCT", "31UDT"),
        demo_dir = "inst/extdata/demo"
) {

    date <- as.Date(date)

    features <- purrr::keep(
        search$items$features,
        \(x) {
            scene <- x$id

            scene_date <- as.Date(
                x$properties$datetime
            )

            tile <- stringr::str_extract(
                scene,
                "31U[A-Z]{2}"
            )

            scene_date == date &&
                tile %in% tiles
        }
    )

    if (length(features) == 0) {
        stop(
            "No Sentinel-2 scenes found for ",
            date,
            " and requested tiles."
        )
    }

    message(
        "Using scenes:\n",
        paste(
            purrr::map_chr(features, "id"),
            collapse = "\n"
        )
    )

    rasters <- purrr::map(
        features,
        \(x) {

            r <- terra::rast(
                x$assets$visual$href
            )

            boundary <- terra::project(
                aoi$geometry,
                terra::crs(r)
            )

            terra::crop(
                r,
                boundary
            )
        }
    )

    r <- if (length(rasters) == 1) {

        rasters[[1]]

    } else {

        do.call(
            terra::mosaic,
            rasters
        )
    }

    visual_dir <- file.path(
        demo_dir,
        "visual"
    )

    dir.create(
        visual_dir,
        recursive = TRUE,
        showWarnings = FALSE
    )

    outfile <- file.path(
        visual_dir,
        paste0(name, ".tif")
    )

    terra::writeRaster(
        r,
        outfile,
        overwrite = TRUE
    )

    invisible(outfile)
}

## ------------------------------------------------------------------
## Save
## ------------------------------------------------------------------

dir.create(
    "inst/extdata/demo",
    recursive = TRUE,
    showWarnings = FALSE
)

saveRDS(
    aoi,
    "inst/extdata/demo/demo_aoi.rds"
)

saveRDS(
    pre_collection,
    "inst/extdata/demo/demo_pre_collection.rds"
)

saveRDS(
    post_collection,
    "inst/extdata/demo/demo_post_collection.rds"
)

saveRDS(
    burn,
    "inst/extdata/demo/demo_burn.rds"
)

saveRDS(
    demo_pre_collection,
    "inst/extdata/demo/demo_pre_collection.rds"
)

saveRDS(
    demo_post_collection,
    "inst/extdata/demo/demo_post_collection.rds"
)

