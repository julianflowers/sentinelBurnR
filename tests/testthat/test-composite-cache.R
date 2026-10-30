make_cache_test_collection <- function() {

    dir <- tempfile("sbr-cache-source-")
    dir.create(dir)

    collection <- make_test_collection()

    r <- terra::rast(
        nrows = 10,
        ncols = 10,
        xmin = 0,
        xmax = 100,
        ymin = 0,
        ymax = 100,
        crs = "EPSG:3857"
    )

    assets <- collection$files$asset

    paths <- file.path(
        dir,
        paste0(assets, ".tif")
    )

    for (i in seq_along(assets)) {

        rr <- r

        terra::values(rr) <-
            seq_len(terra::ncell(rr)) + i

        terra::writeRaster(
            rr,
            paths[i],
            overwrite = TRUE
        )
    }

    collection$files$file <- paths

    collection
}

test_that("composite cache key is deterministic", {

    collection <- make_cache_test_collection()

    key1 <- composite_cache_key(
        collection,
        c("red", "nir08")
    )

    key2 <- composite_cache_key(
        collection,
        c("red", "nir08")
    )

    expect_identical(key1, key2)
})


test_that("different assets produce different composite cache keys", {

    collection <- make_cache_test_collection()

    key1 <- composite_cache_key(
        collection,
        c("red", "nir08")
    )

    key2 <- composite_cache_key(
        collection,
        c("nir08", "swir16")
    )

    expect_false(
        identical(key1, key2)
    )
})


test_that("composite cache version contributes to cache key", {

    expect_true(
        is.integer(composite_cache_version)
    )

    expect_length(
        composite_cache_version,
        1
    )
})
test_that("build_composite reuses cached composite", {

    cache_dir <- tempfile("sbr-cache-")
    dir.create(cache_dir)

    withr::local_options(
        list(
            sbr.cache_dir = cache_dir
        )
    )

    collection <- make_cache_test_collection()

    cached <- terra::rast(
        nrows = 10,
        ncols = 10,
        xmin = 0,
        xmax = 100,
        ymin = 0,
        ymax = 100,
        crs = "EPSG:3857",
        nlyrs = 2
    )

    terra::values(cached) <- seq_len(
        terra::ncell(cached) *
            terra::nlyr(cached)
    )

    names(cached) <- c(
        "red",
        "nir08"
    )

    path <- composite_cache_file(
        collection,
        c("red", "nir08")
    )

    terra::writeRaster(
        cached,
        path,
        overwrite = TRUE
    )

    expect_message(
        result <- build_composite(
            collection,
            assets = c("red", "nir08")
        ),
        "Using cached composite"
    )

    expect_true(
        terra::compareGeom(
            cached,
            result,
            stopOnError = FALSE
        )
    )

    expect_equal(
        terra::values(result),
        terra::values(cached)
    )
})

test_that("overwrite bypasses cached composite", {

    cache_dir <- tempfile("sbr-cache-")
    dir.create(cache_dir)

    withr::local_options(
        list(
            sbr.cache_dir = cache_dir
        )
    )

    collection <- make_cache_test_collection()

    build_composite(
        collection,
        assets = c("red", "nir08"),
        cache = TRUE
    )

    expect_message(
        build_composite(
            collection,
            assets = c("red", "nir08"),
            cache = TRUE,
            overwrite = TRUE
        ),
        "Building red"
    )
})


test_that("composite cache key does not require downloaded files", {

    collection <- make_test_collection()

    expect_false(
        any(file.exists(collection$files$file))
    )

    expect_no_error(
        key <- composite_cache_key(
            collection,
            c("red", "nir08")
        )
    )

    expect_type(key, "character")
    expect_length(key, 1)
})

test_that("sbr_cache_prune filters downloads by asset", {

    root <- withr::local_tempdir()

    downloads <- file.path(root, "downloads")
    temp <- file.path(root, "temp")
    composites <- file.path(root, "composites")

    dir.create(downloads)
    dir.create(temp)
    dir.create(composites)

    scene <- file.path(
        downloads,
        "S2A_31UCT_20260716_0_L2A"
    )

    dir.create(scene)

    visual <- file.path(scene, "visual.tif")
    red <- file.path(scene, "red.tif")

    writeLines("visual", visual)
    writeLines("red", red)

    testthat::local_mocked_bindings(
        cache_downloads = function() downloads,
        cache_temp = function() temp,
        cache_composites = function() composites
    )

    result <- sbr_cache_prune(
        max_age = NULL,
        asset = "visual",
        temp = FALSE,
        dry_run = TRUE
    )

    expect_equal(
        result,
        visual
    )

    expect_true(
        file.exists(visual)
    )

    expect_true(
        file.exists(red)
    )
})

test_that("sbr_cache_prune filters downloads by year", {

    root <- withr::local_tempdir()

    downloads <- file.path(root, "downloads")
    temp <- file.path(root, "temp")
    composites <- file.path(root, "composites")

    dir.create(downloads)
    dir.create(temp)
    dir.create(composites)

    scene_2025 <- file.path(
        downloads,
        "S2A_31UCT_20250716_0_L2A"
    )

    scene_2026 <- file.path(
        downloads,
        "S2A_31UCT_20260716_0_L2A"
    )

    dir.create(scene_2025)
    dir.create(scene_2026)

    file_2025 <- file.path(
        scene_2025,
        "red.tif"
    )

    file_2026 <- file.path(
        scene_2026,
        "red.tif"
    )

    writeLines("2025", file_2025)
    writeLines("2026", file_2026)

    testthat::local_mocked_bindings(
        cache_downloads = function() downloads,
        cache_temp = function() temp,
        cache_composites = function() composites
    )

    result <- sbr_cache_prune(
        max_age = NULL,
        year = 2025,
        temp = FALSE,
        dry_run = TRUE
    )

    expect_equal(
        result,
        file_2025
    )

    expect_true(
        file.exists(file_2025)
    )

    expect_true(
        file.exists(file_2026)
    )
})

test_that("sbr_cache_prune combines asset and year filters", {

    root <- withr::local_tempdir()

    downloads <- file.path(root, "downloads")
    temp <- file.path(root, "temp")
    composites <- file.path(root, "composites")

    dir.create(downloads)
    dir.create(temp)
    dir.create(composites)

    scene_2025 <- file.path(
        downloads,
        "S2A_31UCT_20250716_0_L2A"
    )

    scene_2026 <- file.path(
        downloads,
        "S2A_31UCT_20260716_0_L2A"
    )

    dir.create(scene_2025)
    dir.create(scene_2026)

    visual_2025 <- file.path(
        scene_2025,
        "visual.tif"
    )

    red_2025 <- file.path(
        scene_2025,
        "red.tif"
    )

    visual_2026 <- file.path(
        scene_2026,
        "visual.tif"
    )

    writeLines("visual 2025", visual_2025)
    writeLines("red 2025", red_2025)
    writeLines("visual 2026", visual_2026)

    testthat::local_mocked_bindings(
        cache_downloads = function() downloads,
        cache_temp = function() temp,
        cache_composites = function() composites
    )

    result <- sbr_cache_prune(
        max_age = NULL,
        asset = "visual",
        year = 2025,
        temp = FALSE,
        dry_run = TRUE
    )

    expect_equal(
        result,
        visual_2025
    )

    expect_true(
        file.exists(visual_2025)
    )

    expect_true(
        file.exists(red_2025)
    )

    expect_true(
        file.exists(visual_2026)
    )
})

test_that("sbr_cache_prune removes only matching files", {

    root <- withr::local_tempdir()

    downloads <- file.path(root, "downloads")
    temp <- file.path(root, "temp")
    composites <- file.path(root, "composites")

    dir.create(downloads)
    dir.create(temp)
    dir.create(composites)

    scene <- file.path(
        downloads,
        "S2A_31UCT_20260716_0_L2A"
    )

    dir.create(scene)

    visual <- file.path(scene, "visual.tif")
    red <- file.path(scene, "red.tif")

    writeLines("visual", visual)
    writeLines("red", red)

    testthat::local_mocked_bindings(
        cache_downloads = function() downloads,
        cache_temp = function() temp,
        cache_composites = function() composites
    )

    result <- sbr_cache_prune(
        max_age = NULL,
        asset = "visual",
        temp = FALSE,
        dry_run = FALSE
    )

    expect_equal(
        result,
        visual
    )

    expect_false(
        file.exists(visual)
    )

    expect_true(
        file.exists(red)
    )

    # Directory must remain because red.tif is still present.
    expect_true(
        dir.exists(scene)
    )
})

