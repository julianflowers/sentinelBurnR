test_that("read_boundary returns a SpatVector", {

    p <- terra::vect(
        matrix(
            c(
                0,0,
                1,0,
                1,1,
                0,1,
                0,0
            ),
            ncol = 2,
            byrow = TRUE
        ),
        type = "polygons"
    )

    expect_s4_class(
        read_boundary(p),
        "SpatVector"
    )

})

test_that("read_boundary converts sf", {

    skip_if_not_installed("sf")

    p <- terra::vect(
        matrix(
            c(
                0,0,
                1,0,
                1,1,
                0,1,
                0,0
            ),
            ncol = 2,
            byrow = TRUE
        ),
        type = "polygons"
    )

    s <- sf::st_as_sf(p)

    expect_s4_class(
        read_boundary(s),
        "SpatVector"
    )

})
