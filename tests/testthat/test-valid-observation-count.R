test_ts <- tibble::tibble(
    patch = rep(
        c("SC3_1", "SC3_4", "Forest_6", "Forest_19"),
        each = 5
    ),
    date = rep(
        as.Date(c(
            "2024-03-08",
            "2024-04-17",
            "2024-05-02",
            "2024-06-21",
            "2024-06-26"
        )),
        times = 4
    ),
    median_ndvi = c(
        # SC3_1: persistent decline
        0.75, 0.74, 0.40, 0.39, 0.38,

        # SC3_4: persistent decline
        0.76, 0.45, 0.42, 0.43, 0.44,

        # Forest_6: transient cloud artefact
        0.80, 0.79, 0.15, 0.81, 0.82,

        # Forest_19: transient cloud artefact
        0.82, 0.81, 0.18, 0.83, 0.84
    ),
    coverage_pct = rep(100, 20)
)

test_that("valid observations count unique dates across tiles", {

    r <- terra::rast(
        nrows = 2, ncols = 3,
        xmin = 0, xmax = 60,
        ymin = 0, ymax = 40,
        crs = "EPSG:32631"
    )

    # Date 1: two overlapping tiles
    a <- r
    terra::values(a) <- c(1, 0, 1, 1, 0, 1)

    b <- r
    terra::values(b) <- c(0, 1, 1, 0, 1, 1)

    date1 <- terra::mosaic(a, b, fun = "max")

    # Date 2: coverage extends beyond date 1
    date2 <- terra::rast(
        nrows = 2, ncols = 4,
        xmin = 0, xmax = 80,
        ymin = 0, ymax = 40,
        crs = "EPSG:32631"
    )
    terra::values(date2) <- 1

    # Union-extent template
    template <- terra::extend(date1, terra::ext(date2))

    date1 <- terra::extend(date1, template)
    date2 <- terra::extend(date2, template)

    counts <- terra::app(
        terra::rast(list(date1, date2)),
        sum,
        na.rm = TRUE
    )

    expect_equal(
        as.vector(terra::values(counts)),
        c(2, 2, 2, 1, 2, 2, 2, 1)
    )

})


test_that("persistent disturbances are detected", {

    result <- detect_patch_disturbance(test_ts)

    persistent <- result |>
        dplyr::filter(
            patch %in% c("SC3_1", "SC3_4")
        )

    expect_true(all(
        persistent$classification == "persistent"
    ))

    expect_true(all(
        persistent$persistence == 1
    ))

    expect_equal(nrow(persistent), 2)
})

test_that("transient anomalies are rejected", {

    result <- detect_patch_disturbance(test_ts)

    transient <- result |>
        dplyr::filter(
            patch %in% c("Forest_6", "Forest_19")
        )

    expect_true(all(
        transient$classification == "transient"
    ))

    expect_true(all(
        transient$persistence == 0
    ))
})
