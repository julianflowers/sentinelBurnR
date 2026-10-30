make_fake_download_cache <- function() {

    dir <- withr::local_tempdir(.local_envir = parent.frame())

    scenes <- c(
        "S2A_31UCT_20260501_0_L2A",
        "S2B_31UDT_20260511_0_L2A"
    )

    for (scene in scenes) {
        dir.create(file.path(dir, scene))
        for (asset in c("red", "nir08")) {
            file.create(file.path(dir, scene, paste0(asset, ".tif")))
        }
    }

    dir
}

test_that("sbr_cache_collection returns an sbr_collection from cache", {

    dir <- make_fake_download_cache()

    x <- sbr_cache_collection(dir = dir)

    expect_s3_class(x, "sbr_collection")
    expect_null(x$search)
    expect_equal(nrow(x$files), 4)
    expect_equal(
        unique(x$files$satellite),
        c("sentinel-2a", "sentinel-2b")
    )
    expect_equal(
        unique(x$files$date),
        as.Date(c("2026-05-01", "2026-05-11"))
    )
    expect_equal(unique(x$files$tile), c("31UCT", "31UDT"))
    expect_true(all(file.exists(x$files$file)))
})

test_that("sbr_cache_collection filters by asset, tile and date", {

    dir <- make_fake_download_cache()

    x <- sbr_cache_collection(assets = "red", dir = dir)
    expect_equal(x$files$asset, c("red", "red"))

    x <- sbr_cache_collection(tile = "31UDT", dir = dir)
    expect_equal(unique(x$files$scene), "S2B_31UDT_20260511_0_L2A")

    x <- sbr_cache_collection(start = "2026-05-05", dir = dir)
    expect_equal(unique(x$files$date), as.Date("2026-05-11"))

    expect_error(
        sbr_cache_collection(assets = "swir22", dir = dir),
        "No cached files match"
    )
})

test_that("sbr_cache_collection errors on missing or empty cache", {

    expect_error(
        sbr_cache_collection(dir = file.path(tempdir(), "nope")),
        "does not exist"
    )

    empty <- withr::local_tempdir()
    expect_error(
        sbr_cache_collection(dir = empty),
        "No cached files found"
    )
})

test_that("sbr_cache_collection defaults to sbr.cache_dir", {

    cache_dir <- withr::local_tempdir()
    withr::local_options(sbr.cache_dir = cache_dir)

    scene_dir <- file.path(cache_dir, "downloads", "S2A_31UCT_20260501_0_L2A")
    dir.create(scene_dir, recursive = TRUE)
    file.create(file.path(scene_dir, "red.tif"))

    x <- sbr_cache_collection()

    expect_equal(nrow(x$files), 1)
    expect_true(startsWith(x$files$file, cache_dir))
})
