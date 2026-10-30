#' Build a collection from cached downloads
#'
#' Scans the download cache for Sentinel-2 files previously fetched with
#' [download_s2()] and returns an `sbr_collection` without needing a new
#' search. Files are expected in the layout written by [download_s2()]:
#' `<dir>/<scene_id>/<asset>.<ext>`.
#'
#' Scene metadata (satellite, tile and acquisition date) is parsed from
#' the scene ID, e.g. `S2A_31UCT_20260501_0_L2A`. Scenes whose IDs do not
#' follow this pattern are kept, with `NA` for the unparsed fields.
#'
#' @param assets Character vector of asset names to keep (e.g.
#'   `c("red", "nir08")`). If `NULL`, all cached assets are returned.
#' @param start,end Optional dates (anything accepted by [as.Date()])
#'   limiting the acquisition dates returned.
#' @param tile Optional character vector of tile IDs to keep.
#' @param aoi Optional `sbr_aoi` to attach to the collection.
#' @param dir Directory to scan. Defaults to the download cache, which
#'   follows `getOption("sbr.cache_dir")`.
#'
#' @return An `sbr_collection`. Its `search` and `provenance` are `NULL`
#'   because no search was run.
#' @export
sbr_cache_collection <- function(
        assets = NULL,
        start = NULL,
        end = NULL,
        tile = NULL,
        aoi = NULL,
        dir = cache_downloads()
) {

    if (!dir.exists(dir)) {
        stop(
            "Cache directory does not exist: ", dir,
            call. = FALSE
        )
    }

    rel <- list.files(
        dir,
        recursive = TRUE
    )

    # Only <scene_id>/<asset>.<ext>; ignore partial downloads
    rel <- rel[
        lengths(strsplit(rel, "/", fixed = TRUE)) == 2L &
            !grepl("\\.(part|tmp|download)$", rel)
    ]

    paths <- file.path(dir, rel)

    if (length(paths) == 0L) {
        stop(
            "No cached files found in: ", dir,
            call. = FALSE
        )
    }

    scene <- basename(dirname(paths))
    parts <- strsplit(scene, "_", fixed = TRUE)

    part <- function(i) {
        vapply(
            parts,
            \(p) if (length(p) >= i) p[[i]] else NA_character_,
            character(1)
        )
    }

    sat_code <- tolower(part(1))
    satellite <- ifelse(
        grepl("^s2[a-d]$", sat_code),
        paste0("sentinel-2", substr(sat_code, 3, 3)),
        NA_character_
    )

    date <- suppressWarnings(
        as.Date(part(3), format = "%Y%m%d")
    )

    files <- data.frame(
        scene = scene,
        tile = part(2),
        date = date,
        satellite = satellite,
        asset = tools::file_path_sans_ext(basename(paths)),
        file = paths,
        status = "cached",
        attempts = 0L,
        stringsAsFactors = FALSE
    )

    keep <- rep(TRUE, nrow(files))

    if (!is.null(assets)) {
        keep <- keep & files$asset %in% assets
    }

    if (!is.null(tile)) {
        keep <- keep & files$tile %in% tile
    }

    if (!is.null(start)) {
        keep <- keep & !is.na(files$date) & files$date >= as.Date(start)
    }

    if (!is.null(end)) {
        keep <- keep & !is.na(files$date) & files$date <= as.Date(end)
    }

    files <- files[keep, , drop = FALSE]

    if (nrow(files) == 0L) {
        stop(
            "No cached files match the requested filters.",
            call. = FALSE
        )
    }

    files <- files[
        order(files$date, files$scene, files$asset),
        ,
        drop = FALSE
    ]
    rownames(files) <- NULL

    new_s2_collection(
        files = files,
        aoi = aoi
    )
}
