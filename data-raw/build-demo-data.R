library(sentinelBurnR)

## ------------------------------------------------------------------
## AOI
## ------------------------------------------------------------------

aoi <- read_aoi(
    system.file(
        "extdata",
        "dunwich.gpkg",
        package = "sentinelBurnR"
    )
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
    assets = s2_burn_assets

)

post_collection <- download_s2(
    post_search,
    limit = 2,
    workers = 1,
    assets = s2_burn_assets
)

## ------------------------------------------------------------------
## Burn analysis
## ------------------------------------------------------------------

burn <- analyse_burn(
    pre_collection,
    post_collection
)

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
