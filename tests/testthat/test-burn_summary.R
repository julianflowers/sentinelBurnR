



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

