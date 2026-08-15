test_that("burn_area sums area of burned cells", {

    x <- terra::rast(
        xmin = 400000,
        xmax = 400020,
        ymin = 5800000,
        ymax = 5800020,
        resolution = 10,
        crs = "EPSG:32631"
    )

    terra::values(x) <- c(
        1, 1,
        0, 1
    )

    result <- burn_area(
        x,
        unit = "ha"
    )

    cell_area <- terra::cellSize(
        x,
        unit = "ha"
    )

    expected <- sum(
        terra::values(cell_area)[c(1, 2, 4)]
    )

    expect_equal(
        result,
        expected
    )
})
