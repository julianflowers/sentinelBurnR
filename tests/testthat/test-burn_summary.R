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
                prod(terra::res(burned)) / 10000

        ),

        class = "sbr_burn"

    )

}



test_that("burn_summary returns correct area", {

    burn <- make_test_burn()

    s <- burn_summary(
        burn,
        make_test_boundary()
    )

    expect_equal(
        s$area_ha,
        1,
        tolerance = 1e-6
    )

    expect_equal(
        s$burned_ha,
        0.16
    )

    expect_equal(
        s$burn_pct,
        16
    )


    expect_equal(
        s$burn_pct,
        16
    )

    expect_equal(
        s$mean_dnbr,
        0.096,
        tolerance = 1e-6
    )

})

