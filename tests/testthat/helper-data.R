library(terra)
library(sf)
# ------------------------------------------------------------------
# Create a simple test AOI
# ------------------------------------------------------------------

make_test_aoi <- function() {

    terra::vect(
        terra::ext(
            650000,
            651000,
            270000,
            271000
        ),
        crs = "EPSG:27700"
    )

}

# ------------------------------------------------------------------
# Create a simple sf polygon
# ------------------------------------------------------------------

make_test_sf <- function() {

    poly <-

        sf::st_polygon(

            list(

                matrix(

                    c(

                        0,0,

                        1,0,

                        1,1,

                        0,1,

                        0,0

                    ),

                    ncol=2,

                    byrow=TRUE

                )

            )

        )

    sf::st_sf(

        id=1,

        geometry=

            sf::st_sfc(

                poly,

                crs=4326

            )

    )

}

make_test_boundary <- function() {

    terra::vect(

        matrix(

            c(
                0, 0,
                100, 0,
                100, 100,
                0, 100,
                0, 0
            ),

            ncol = 2,
            byrow = TRUE

        ),

        type = "polygons",
        crs = "EPSG:3857"

    )

}



