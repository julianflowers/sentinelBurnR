test_that("mask_scl preserves class", {

    r <- terra::rast(
        nrows = 4,
        ncols = 4,
        vals = 1:16
    )

    s <- terra::rast(
        nrows = 4,
        ncols = 4,
        vals = c(
            4,4,8,8,
            4,5,8,8,
            6,7,9,9,
            4,4,4,4
        )
    )

    out <- mask_scl(r, prepare_scl_mask(s, r))

    expect_s4_class(
        out,
        "SpatRaster"
    )

})

test_that("mask_scl masks cloud pixels", {

    r <- terra::rast(
        nrows = 2,
        ncols = 2,
        vals = 1:4
    )

    s <- terra::rast(
        nrows = 2,
        ncols = 2,
        vals = c(
            4,
            8,
            5,
            9
        )
    )

    out <- mask_scl(r, prepare_scl_mask(s, r))

    v <- terra::values(out)

    expect_equal(
        v[1],
        1
    )

    expect_true(
        is.na(v[2])
    )

    expect_equal(
        v[3],
        3
    )

    expect_true(
        is.na(v[4])
    )

    expect_equal(
        terra::values(out)[1, 1],
        terra::values(r)[1, 1]
    )

})

test_that("mask_scl requires an already prepared mask", {

    image <- terra::rast(nrows = 4, ncols = 4, vals = 1:16)
    scl <- terra::rast(nrows = 2, ncols = 2, vals = c(4, 8, 5, 9))

    expect_error(
        mask_scl(image, scl),
        "must already match"
    )
})

test_that("SCL masks are prepared once per tile and grid", {

    make_stack <- function(nrows, ncols, values = 1) {
        x <- terra::rast(nrows = nrows, ncols = ncols, nlyrs = 2)
        terra::values(x) <- values
        x
    }

    scl <- list(T1 = make_stack(2, 2, rep(c(4, 8, 5, 9), 2)))
    ten_m <- make_stack(4, 4)
    twenty_m <- make_stack(2, 2)

    bands <- list(
        red = list(T1 = ten_m),
        green = list(T1 = ten_m),
        blue = list(T1 = ten_m),
        nir08 = list(T1 = ten_m),
        swir22 = list(T1 = twenty_m)
    )

    masks <- prepare_scl_masks(scl, bands)

    expect_equal(attr(masks, "n_prepared"), 2L)
    expect_true(terra::compareGeom(masks$red$T1, ten_m, lyrs = FALSE))
    expect_true(terra::compareGeom(masks$swir22$T1, twenty_m, lyrs = FALSE))
    expect_equal(
        terra::values(masks$red$T1),
        terra::values(masks$green$T1)
    )
})

test_that("cached masking is numerically equivalent to per-band preparation", {

    make_stack <- function(nrows, ncols, values) {
        x <- terra::rast(nrows = nrows, ncols = ncols, nlyrs = 2)
        terra::values(x) <- values
        x
    }

    scl <- list(T1 = make_stack(2, 2, rep(c(4, 8, 5, 9), 2)))
    bands <- list(
        red = list(T1 = make_stack(4, 4, seq_len(32))),
        green = list(T1 = make_stack(4, 4, seq_len(32) + 100)),
        blue = list(T1 = make_stack(4, 4, seq_len(32) + 200)),
        nir08 = list(T1 = make_stack(4, 4, seq_len(32) + 300)),
        swir22 = list(T1 = make_stack(2, 2, seq_len(8) + 400))
    )

    cached <- prepare_scl_masks(scl, bands)

    for (asset in names(bands)) {
        image <- bands[[asset]]$T1
        legacy <- mask_scl(image, prepare_scl_mask(scl$T1, image))
        refactored <- mask_scl(image, cached[[asset]]$T1)

        expect_equal(
            terra::values(refactored),
            terra::values(legacy),
            tolerance = 0
        )
    }
})
