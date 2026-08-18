test_that("analyse_burn requires sbr_collections", {

    expect_error(
        analyse_burn(
            pre = list(),
            post = list()
        ),
        "`pre` must be an sbr_collection"
    )
})


test_that("sbr_burn print method works", {

    x <- structure(
        list(
            area_ha = 64.2,
            threshold = 0.27
        ),
        class = "sbr_burn"
    )

    expect_output(
        print(x),
        "64.2 ha"
    )

    expect_output(
        print(x),
        "0.27"
    )
})
