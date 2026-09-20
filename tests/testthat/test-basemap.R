test_that("XYZ tile coordinates are calculated correctly", {

    bbox <- c(
        left = 1.550397,
        bottom = 52.249090,
        right = 1.669747,
        top = 52.322371
    )

    zoom <- 14L

    xmin <- xyz_x(
        bbox[["left"]],
        zoom
    )

    xmax <- xyz_x(
        bbox[["right"]],
        zoom
    )

    ymin <- xyz_y(
        bbox[["top"]],
        zoom
    )

    ymax <- xyz_y(
        bbox[["bottom"]],
        zoom
    )

    expect_true(xmin <= xmax)
    expect_true(ymin <= ymax)

    expect_type(xmin, "double")
    expect_type(ymin, "double")
})

test_that("basemap cache key depends on XYZ tile range", {

    f <- basemap_cache_file(
        type = "satellite",
        zoom = 14,
        xmin = 8262,
        xmax = 8267,
        ymin = 5381,
        ymax = 5386
    )

    expect_match(
        basename(f),
        "^satellite_z14_x8262-8267_y5381-5386\\.rds$"
    )
})

test_that("nearby extents sharing tiles produce the same cache key", {

    zoom <- 14L

    bbox1 <- c(
        left = 1.550397,
        bottom = 52.249090,
        right = 1.669747,
        top = 52.322371
    )

    bbox2 <- bbox1 + c(
        0.000001,
        0.000001,
        -0.000001,
        -0.000001
    )

    tile_range <- function(bbox) {

        c(
            xmin = xyz_x(bbox[["left"]], zoom),
            xmax = xyz_x(bbox[["right"]], zoom),
            ymin = xyz_y(bbox[["top"]], zoom),
            ymax = xyz_y(bbox[["bottom"]], zoom)
        )
    }

    expect_equal(
        tile_range(bbox1),
        tile_range(bbox2)
    )
})

