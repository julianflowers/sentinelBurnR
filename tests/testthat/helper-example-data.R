aoi <- read_aoi(
    system.file(
        "extdata",
        "dunwich.gpkg",
        package = "sentinelBurnR"
    )
)

search <- search_s2(
    aoi,
    start = "...",
    end = "..."
)

pre_collection <- download_s2(
    search,
    limit = 1
)
