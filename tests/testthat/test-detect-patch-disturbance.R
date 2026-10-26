library(testthat)

test_that("detect_patch_disturbance classifies temporal patterns", {

    dates <- as.Date(c(
        "2024-03-08",
        "2024-04-17",
        "2024-05-02",
        "2024-05-05",
        "2024-05-20",
        "2024-06-21",
        "2024-06-26"
    ))

    make_patch <- function(id, ndvi) {
        tibble::tibble(
            patch = id,
            date = dates,
            median_ndvi = ndvi,
            coverage_pct = 100
        )
    }

    observations <- dplyr::bind_rows(

        # Early felling, persistent vegetation loss
        make_patch(
            "SC3_4",
            c(0.758, 0.472, 0.416, 0.394,
              0.420, 0.459, 0.478)
        ),

        # Later felling, persistent vegetation loss
        make_patch(
            "SC3_1",
            c(0.746, 0.724, 0.449, 0.434,
              0.400, 0.345, 0.345)
        ),

        # Cloud artefact followed by recovery
        make_patch(
            "Forest_6",
            c(0.747, 0.770, 0.781, 0.817,
              0.203, 0.875, 0.833)
        ),

        make_patch(
            "Forest_19",
            c(0.683, 0.750, 0.791, 0.799,
              0.163, 0.876, 0.837)
        )
    )

    result <- detect_patch_disturbance(observations)

    expect_equal(
        result$classification[
            match(c("SC3_1", "SC3_4"), result$patch)
        ],
        c("persistent", "persistent")
    )

    expect_equal(
        result$classification[
            match(c("Forest_6", "Forest_19"), result$patch)
        ],
        c("transient", "transient")
    )

    expect_equal(
        result$persistence[
            match(c("SC3_1", "SC3_4"), result$patch)
        ],
        c(1, 1)
    )

    expect_equal(
        result$persistence[
            match(c("Forest_6", "Forest_19"), result$patch)
        ],
        c(0, 0)
    )
})

test_that("insufficient observations are identified", {

    observations <- tibble::tibble(
        patch = "A",
        date = as.Date(c(
            "2024-03-08",
            "2024-05-02"
        )),
        median_ndvi = c(0.75, 0.40),
        coverage_pct = c(100, 100)
    )

    result <- detect_patch_disturbance(observations)

    expect_equal(
        result$classification,
        "insufficient_evidence"
    )
})

test_that("stable vegetation is not classified as disturbed", {

    observations <- tibble::tibble(
        patch = "B",
        date = as.Date(c(
            "2024-03-08",
            "2024-05-02",
            "2024-06-21"
        )),
        median_ndvi = c(0.75, 0.77, 0.73),
        coverage_pct = c(100, 100, 100)
    )

    result <- detect_patch_disturbance(observations)

    expect_equal(
        result$classification,
        "no_disturbance"
    )
})

test_that("low-coverage observations are excluded", {

    observations <- tibble::tibble(
        patch = "C",
        date = as.Date(c(
            "2024-03-08",
            "2024-05-02",
            "2024-05-20",
            "2024-06-21"
        )),
        median_ndvi = c(0.75, 0.76, 0.10, 0.78),
        coverage_pct = c(100, 100, 20, 100)
    )

    result <- detect_patch_disturbance(observations)

    expect_equal(
        result$classification,
        "no_disturbance"
    )
})

test_that("genuine disturbance is detected after a cloud artefact", {

    observations <- tibble::tibble(
        patch = "D",
        date = as.Date(c(
            "2024-03-08",
            "2024-04-17",
            "2024-05-02",
            "2024-05-05",
            "2024-06-21",
            "2024-06-26"
        )),
        median_ndvi = c(
            0.75,  # Intact forest
            0.15,  # Cloud artefact
            0.76,  # Recovery
            0.43,  # Genuine felling
            0.40,  # Persistent
            0.42   # Persistent
        ),
        coverage_pct = rep(100, 6)
    )

    result <- detect_patch_disturbance(observations)

    expect_equal(
        result$classification,
        "persistent"
    )

    expect_equal(
        result$disturbance_date,
        as.Date("2024-05-05")
    )
})
