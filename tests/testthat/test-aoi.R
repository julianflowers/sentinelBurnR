test_that("read_aoi accepts a SpatVector", {

    aoi <- terra::vect(
        terra::ext(
            650000,
            651000,
            270000,
            271000
        ),
        crs = "EPSG:27700"
    )

    result <- read_aoi(aoi)

    expect_s4_class(
        result,
        "SpatVector"
    )

    expect_equal(
        nrow(result),
        1
    )
})


test_that("read_aoi accepts an sf object", {

    polygon <- sf::st_polygon(
        list(
            matrix(
                c(
                    0, 0,
                    1, 0,
                    1, 1,
                    0, 1,
                    0, 0
                ),
                ncol = 2,
                byrow = TRUE
            )
        )
    )

    aoi <- sf::st_sf(
        id = 1,
        geometry = sf::st_sfc(
            polygon,
            crs = 4326
        )
    )

    result <- read_aoi(aoi)

    expect_s4_class(
        result,
        "SpatVector"
    )

    expect_equal(
        nrow(result),
        1
    )
})


test_that("read_aoi reads a vector file", {

    aoi <- terra::vect(
        terra::ext(
            650000,
            651000,
            270000,
            271000
        ),
        crs = "EPSG:27700"
    )

    filename <- tempfile(
        fileext = ".gpkg"
    )

    terra::writeVector(
        aoi,
        filename,
        overwrite = TRUE
    )

    result <- read_aoi(filename)

    expect_s4_class(
        result,
        "SpatVector"
    )

    expect_equal(
        nrow(result),
        1
    )
})


test_that("read_aoi dissolves multiple features", {

    first <- terra::vect(
        terra::ext(
            0,
            1,
            0,
            1
        ),
        crs = "EPSG:4326"
    )

    second <- terra::vect(
        terra::ext(
            2,
            3,
            0,
            1
        ),
        crs = "EPSG:4326"
    )

    aoi <- rbind(
        first,
        second
    )

    result <- read_aoi(
        aoi,
        dissolve = TRUE
    )

    expect_equal(
        nrow(result),
        1
    )
})


test_that("read_aoi preserves features when dissolve is FALSE", {

    first <- terra::vect(
        terra::ext(
            0,
            1,
            0,
            1
        ),
        crs = "EPSG:4326"
    )

    second <- terra::vect(
        terra::ext(
            2,
            3,
            0,
            1
        ),
        crs = "EPSG:4326"
    )

    aoi <- rbind(
        first,
        second
    )

    result <- read_aoi(
        aoi,
        dissolve = FALSE
    )

    expect_equal(
        nrow(result),
        2
    )
})


test_that("read_aoi can reproject an AOI", {

    aoi <- terra::vect(
        terra::ext(
            1.5,
            1.7,
            52.1,
            52.3
        ),
        crs = "EPSG:4326"
    )

    result <- read_aoi(
        aoi,
        target_crs = "EPSG:27700"
    )

    expect_true(
        terra::same.crs(
            result,
            "EPSG:27700"
        )
    )
})


test_that("read_aoi rejects missing files", {

    expect_error(
        read_aoi("this-file-does-not-exist.gpkg"),
        "does not exist"
    )
})


test_that("read_aoi rejects objects without a CRS", {

    aoi <- terra::vect(
        terra::ext(
            0,
            1,
            0,
            1
        )
    )

    expect_error(
        read_aoi(aoi),
        "coordinate reference system"
    )
})


test_that("read_aoi rejects unsupported inputs", {

    expect_error(
        read_aoi(42),
        "must be a vector filename"
    )
})
