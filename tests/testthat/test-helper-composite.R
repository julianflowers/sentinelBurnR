make_test_composite <- function() {

    r <- terra::rast(
        nrows = 2,
        ncols = 2,
        xmin = 0,
        xmax = 20,
        ymin = 0,
        ymax = 20,
        crs = "EPSG:27700"
    )

    red <- r
    nir08 <- r
    swir22 <- r

    terra::values(red) <- c(
        0.2, 0.3,
        0.4, 0.5
    )

    terra::values(nir08) <- c(
        0.8, 0.7,
        0.6, 0.5
    )

    terra::values(swir22) <- c(
        0.1, 0.2,
        0.3, 0.4
    )

    comp <- c(
        red,
        nir08,
        swir22
    )

    names(comp) <- c(
        "red",
        "nir08",
        "swir22"
    )

    comp
}

