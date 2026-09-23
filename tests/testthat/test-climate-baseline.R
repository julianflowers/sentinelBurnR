test_that("baseline_year_months handles windows within a year", {

    result <- baseline_year_months(
        date = as.Date("2026-08-25"),
        years = c(1991, 1992),
        window_days = 90
    )

    expected <- data.frame(
        year = c(
            rep(1991L, 4),
            rep(1992L, 4)
        ),
        month = rep(5:8, 2)
    )

    expect_equal(
        result,
        expected
    )
})


test_that("baseline_year_months handles windows crossing year boundary", {

    result <- baseline_year_months(
        date = as.Date("2026-01-15"),
        years = c(2020, 2021),
        window_days = 90
    )

    expected <- data.frame(
        year = c(
            2019L, 2019L, 2019L, 2020L,
            2020L, 2020L, 2020L, 2021L
        ),
        month = c(
            10L, 11L, 12L, 1L,
            10L, 11L, 12L, 1L
        )
    )

    expect_equal(
        result,
        expected
    )
})
