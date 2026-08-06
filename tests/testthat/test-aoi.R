test_that("read_aoi accepts a SpatVector", {

    result <- read_aoi(
        make_test_aoi()
    )

    expect_s3_class(
        result,
        "sbr_aoi"
    )

    expect_s4_class(
        geometry(result),
        "SpatVector"
    )

    expect_equal(
        nrow(
            geometry(result)
        ),
        1
    )

})



test_that("read_aoi accepts an sf object", {

    result <- read_aoi(
        make_test_sf()
    )

    expect_s3_class(
        result,
        "sbr_aoi"
    )

    expect_s4_class(
        geometry(result),
        "SpatVector"
    )

    expect_equal(
        nrow(
            geometry(result)
        ),
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
        nrow(geometry(result)),
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
        nrow(geometry(result)),
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
            geometry(result),
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

test_that("read_aoi reads a vector file", {

    filename <- tempfile(
        fileext = ".gpkg"
    )

    terra::writeVector(
        make_test_aoi(),
        filename,
        overwrite = TRUE
    )

    result <- read_aoi(filename)

    expect_s3_class(
        result,
        "sbr_aoi"
    )

    expect_s4_class(
        geometry(result),
        "SpatVector"
    )

})
