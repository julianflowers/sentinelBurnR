

test_that("disturbance area summary reports correct areas", {

    patches <- terra::rast(
        nrows = 5,
        ncols = 10,
        xmin = 0,
        xmax = 100,
        ymin = 0,
        ymax = 50,
        crs = "EPSG:32631"
    )

    terra::values(patches) <- c(
        rep(28, 20),
        rep(31, 25),
        rep(34, 5)
    )

    disturbances <- tibble::tibble(
        patch = c(28, 31, 34),
        classification = c(
            "persistent",
            "transient",
            "persistent"
        ),
        disturbance_date = as.Date(c(
            "2024-05-02",
            "2024-05-20",
            "2024-05-02"
        ))
    )

    result <- summarise_disturbance_area(
        disturbances,
        patches
    )

    persistent <- dplyr::filter(
        result,
        classification == "persistent"
    )

    expect_equal(persistent$patches, 2L)
    expected_areas <- terra::zonal(
        terra::cellSize(patches, unit = "ha"),
        patches,
        fun = "sum",
        na.rm = TRUE
    )

    persistent_areas <- expected_areas[
        expected_areas[[1]] %in% c(28, 34),
        2
    ]

    expect_equal(
        persistent$total_area_ha,
        sum(persistent_areas),
        tolerance = 1e-6
    )

    expect_equal(
        persistent$median_patch_ha,
        median(persistent_areas),
        tolerance = 1e-6
    )
})

test_that("disturbance area can be grouped by date", {

    patches <- terra::rast(
        nrows = 3,
        ncols = 10,
        xmin = 0,
        xmax = 100,
        ymin = 0,
        ymax = 30,
        crs = "EPSG:32631"
    )

    terra::values(patches) <- rep(1:3, each = 10)

    disturbances <- tibble::tibble(
        patch = 1:3,
        classification = rep("transient", 3),
        disturbance_date = as.Date(c(
            "2024-04-17",
            "2024-05-20",
            "2024-05-20"
        ))
    )

    result <- summarise_disturbance_area(
        disturbances,
        patches,
        by_date = TRUE
    )

    expect_equal(nrow(result), 2L)

    may <- dplyr::filter(
        result,
        disturbance_date == as.Date("2024-05-20")
    )

    expected_areas <- terra::zonal(
        terra::cellSize(patches, unit = "ha"),
        patches,
        fun = "sum",
        na.rm = TRUE
    )

    may_areas <- expected_areas[
        expected_areas[[1]] %in% c(2, 3),
        2
    ]

    expect_equal(
        may$total_area_ha,
        sum(may_areas),
        tolerance = 1e-6
    )
})

test_that("missing required columns produce an error", {

    patches <- terra::rast(
        nrows = 2,
        ncols = 2
    )

    disturbances <- tibble::tibble(
        patch = 1
    )

    expect_error(
        summarise_disturbance_area(
            disturbances,
            patches
        ),
        "Missing required columns"
    )

    expect_error(
        summarise_disturbance_area(
            tibble::tibble(
                patch = 1,
                classification = "persistent"
            ),
            patches,
            by_date = TRUE
        ),
        "disturbance_date"
    )
})

