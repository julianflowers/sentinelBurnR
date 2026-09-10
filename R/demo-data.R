#' Load sentinelBurnR demo data
#'
#' Loads the small Dunwich demonstration datasets supplied with
#' sentinelBurnR. Sentinel-2 collections contain cropped imagery and
#' can be passed directly to the package analysis functions.
#'
#' @param name Demo dataset to load. Currently one of `"aoi"`,
#'   `"pre"`, or `"post"`.
#'
#' @return The requested demo object.
#'
#' @export
demo_data <- function(name = c("aoi", "pre", "post", "visual_pre", "visual_post",
                               "drought_historical", "drought_current",
                               "rainfall")) {

    name <- match.arg(name)

    demo_dir <- system.file(
        "extdata",
        "demo",
        package = "sentinelBurnR"
    )

    if (!nzchar(demo_dir)) {
        stop(
            "sentinelBurnR demo data could not be found.",
            call. = FALSE
        )
    }

    if (name == "rainfall") {

        file <- file.path(
            demo_dir,
            "demo_rainfall.rds"
        )

        if (!file.exists(file)) {
            stop("Rainfall demo data not found.")
        }

        return(readRDS(file))
    }

    # Drought data are stored as GeoTIFFs so that terra objects
    # are reconstructed rather than serialised in RDS files.
    if (name == "drought_historical") {

        files <- file.path(
            demo_dir,
            "drought",
            paste0("ndmi_", 2018:2025, ".tif")
        )

        if (!all(file.exists(files))) {
            stop("Historical drought demo rasters not found.")
        }

        rasters <- purrr::map(
            files,
            terra::rast
        )

        names(rasters) <- as.character(2018:2025)

        return(rasters)
    }

    if (name == "drought_current") {

        file <- file.path(
            demo_dir,
            "drought",
            "ndmi_2026-07-29.tif"
        )

        if (!file.exists(file)) {
            stop("Current drought demo raster not found.")
        }

        raster <- terra::rast(file)

        return(
            stats::setNames(
                list(raster),
                "2026-07-29"
            )
        )
    }

    if (name %in% c("visual_pre", "visual_post")) {

        filename <- switch(
            name,
            visual_pre = "pre.tif",
            visual_post = "post.tif"
        )

        file <- file.path(
            demo_dir,
            "visual",
            filename
        )

        if (!file.exists(file)) {
            stop(
                "True-colour demo image not found.",
                call. = FALSE
            )
        }

        return(terra::rast(file))
    }

    filename <- switch(
        name,
        aoi  = "demo_aoi.rds",
        pre  = "demo_pre_collection.rds",
        post = "demo_post_collection.rds"
    )

    # AOI is stored as sf so that no terra external
    # pointers are serialised.
    if (name == "aoi") {

        aoi_sf <- readRDS(
            file.path(demo_dir, filename)
        )

        return(read_aoi(aoi_sf))
    }

    # Collections contain only serialisable metadata.
    x <- readRDS(
        file.path(demo_dir, filename)
    )

    if (name %in% c("visual_pre", "visual_post")) {

        filename <- switch(
            name,
            visual_pre = "pre.tif",
            visual_post = "post.tif"
        )

        file <- file.path(
            demo_dir,
            "visual",
            filename
        )

        if (!file.exists(file)) {
            stop("True-colour demo image not found.")
        }

        return(terra::rast(file))
    }

    if (inherits(x, "sbr_collection")) {

        # Resolve portable relative raster paths.
        x$files$file <- file.path(
            demo_dir,
            x$files$file
        )

        # Reconstruct the AOI in this R process rather than
        # restoring a serialised terra object.
        aoi_sf <- readRDS(
            file.path(demo_dir, "demo_aoi.rds")
        )

        aoi <- read_aoi(aoi_sf)

        x$aoi <- aoi

        if (!is.null(x$search)) {
            x$search$aoi <- aoi
        }
    }

    x
}
