
test_that("filter_patches_by_area removes small patches", {

    patches <- terra::rast(
        nrows = 10,
        ncols = 10,
        xmin = 0,
        xmax = 100,
        ymin = 0,
        ymax = 100,
        crs = "EPSG:32631"
    )

    values <- rep(NA_real_, 100)
    values[1:2] <- 34
    values[11:30] <- 28
    values[41:70] <- 31

    terra::values(patches) <- values

    result <- filter_patches_by_area(
        patches,
        min_area_ha = 0.1
    )

    ids <- terra::freq(result)$value

    expect_setequal(ids, c(28, 31))
    expect_false(34 %in% ids)
})

test_that("filter_patches_by_area preserves original IDs", {

    patches <- terra::rast(
        nrows = 5,
        ncols = 5,
        xmin = 0,
        xmax = 50,
        ymin = 0,
        ymax = 50,
        crs = "EPSG:32631"
    )

    terra::values(patches) <- c(
        rep(28, 10),
        rep(31, 10),
        rep(34, 5)
    )

    result <- filter_patches_by_area(
        patches,
        min_area_ha = 0.08
    )

    expect_setequal(
        terra::freq(result)$value,
        c(28, 31)
    )
})

test_that("zero threshold leaves patches unchanged", {

    patches <- terra::rast(
        nrows = 2,
        ncols = 2,
        xmin = 0,
        xmax = 20,
        ymin = 0,
        ymax = 20,
        crs = "EPSG:32631"
    )

    terra::values(patches) <- c(1, 2, 3, NA)

    result <- filter_patches_by_area(patches)

    expect_equal(
        terra::values(result),
        terra::values(patches)
    )
})

test_that("invalid minimum areas are rejected", {

    patches <- terra::rast(
        nrows = 2,
        ncols = 2
    )

    expect_error(
        filter_patches_by_area(patches, -1),
        "non-negative"
    )

    expect_error(
        filter_patches_by_area(patches, NA_real_),
        "non-negative"
    )
})
