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
    green <- r
    blue <- r
    nir08 <- r
    swir16 <- r
    swir22 <- r

    terra::values(red) <- c(
        0.2, 0.3,
        0.4, 0.5
    )

    terra::values(green) <- c(
        0.15, 0.25,
        0.35, 0.45
    )

    terra::values(blue) <- c(
        0.1, 0.2,
        0.3, 0.4
    )

    terra::values(nir08) <- c(
        0.8, 0.7,
        0.6, 0.5
    )

    terra::values(swir16) <- c(
        0.3, 0.35,
        0.4, 0.45
    )

    terra::values(swir22) <- c(
        0.1, 0.2,
        0.3, 0.4
    )

    comp <- c(
        red,
        green,
        blue,
        nir08,
        swir16,
        swir22
    )

    names(comp) <- c(
        "red",
        "green",
        "blue",
        "nir08",
        "swir16",
        "swir22"
    )

    comp
}
