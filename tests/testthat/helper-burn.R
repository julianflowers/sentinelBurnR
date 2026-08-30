make_test_burn <- function() {

    r <- terra::rast(
        ncols = 10,
        nrows = 10,
        xmin = 0,
        xmax = 100,
        ymin = 0,
        ymax = 100,
        crs = "EPSG:3857"
    )

    dnbr <- r
    terra::values(dnbr) <- 0

    ##
    ## Burn centre 4 × 4 pixels
    ##

    burn_cells <- terra::cells(
        dnbr,
        ext(30, 70, 30, 70)
    )

    terra::values(dnbr)[burn_cells] <- 0.6

    burned <- dnbr > 0.27

    severity <- r
    terra::values(severity) <- 3
    terra::values(severity)[burn_cells] <- 7

    structure(

        list(

            dnbr = dnbr,

            burned = burned,

            severity = severity,

            threshold = 0.27,

            area_ha = sum(terra::values(burned)) *
                prod(terra::res(burned)) / 10000,

            provenance = list(

                pre = list(
                    summary = list(
                        start = as.Date("2026-05-01"),
                        end = as.Date("2026-05-31"),
                        n_acquisitions = 3,
                        satellites = "sentinel-2a",
                        mean_cloud = 5,
                        max_cloud = 10
                    )
                ),

                post = list(
                    summary = list(
                        start = as.Date("2026-06-01"),
                        end = as.Date("2026-06-30"),
                        n_acquisitions = 3,
                        satellites = "sentinel-2a",
                        mean_cloud = 4,
                        max_cloud = 8
                    )
                ),

                processing = list(
                    threshold = 0.27
                )
            )

        ),

        class = "sbr_burn"

    )

}
