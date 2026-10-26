#' Detect persistent vegetation disturbance in patch time series
#'
#' Identifies candidate disturbance events from declines in median NDVI
#' and evaluates whether the decline persists in subsequent observations.
#'
#' @param x Data frame containing patch, date, median_ndvi and coverage_pct.
#' @param min_decline Minimum absolute NDVI decline defining a candidate.
#' @param min_confirmations Minimum number of subsequent confirming observations.
#' @param min_persistence Minimum proportion of subsequent observations
#'   confirming the disturbance.
#' @param min_coverage Minimum percentage of valid pixels per observation.
#'
#' @return A tibble containing one selected event per patch.
#' @export
detect_patch_disturbance <- function(
        x,
        min_decline = 0.25,
        min_confirmations = 2,
        min_persistence = 0.75,
        min_coverage = 80
) {

    required <- c(
        "patch",
        "date",
        "median_ndvi",
        "coverage_pct"
    )

    missing <- setdiff(required, names(x))

    if (length(missing) > 0) {
        stop(
            "Missing required columns: ",
            paste(missing, collapse = ", "),
            call. = FALSE
        )
    }

    x |>
        dplyr::arrange(
            .data$patch,
            .data$date
        ) |>
        dplyr::group_by(.data$patch) |>
        dplyr::group_modify(function(data, key) {

            # Keep observations with adequate pixel coverage
            valid <- data |>
                dplyr::filter(
                    !is.na(.data$median_ndvi),
                    !is.na(.data$coverage_pct),
                    .data$coverage_pct >= min_coverage
                ) |>
                dplyr::arrange(.data$date)

            # Standard output when no event can be evaluated
            empty <- tibble::tibble(
                previous_date = as.Date(NA),
                disturbance_date = as.Date(NA),
                ndvi_decline = NA_real_,
                subsequent_observations = 0L,
                confirmed_observations = 0L,
                persistence = NA_real_,
                classification = "insufficient_evidence"
            )

            if (nrow(valid) < 2) {
                return(empty)
            }

            # Calculate consecutive NDVI changes
            changes <- valid |>
                dplyr::mutate(
                    previous_date = dplyr::lag(.data$date),
                    previous_ndvi = dplyr::lag(.data$median_ndvi),
                    change = .data$median_ndvi -
                        .data$previous_ndvi
                ) |>
                dplyr::filter(!is.na(.data$change))

            # Identify every substantial decline
            candidates <- changes |>
                dplyr::filter(
                    .data$change <= -min_decline
                )

            if (nrow(candidates) == 0) {
                empty$classification <- "no_disturbance"
                return(empty)
            }

            # Evaluate persistence of each candidate
            evaluated <- purrr::map_dfr(
                seq_len(nrow(candidates)),
                function(i) {

                    event <- candidates[i, ]

                    subsequent <- valid |>
                        dplyr::filter(
                            .data$date > event$date
                        )

                    n_subsequent <- nrow(subsequent)

                    # Compare subsequent NDVI with pre-event NDVI
                    confirmed <- subsequent$median_ndvi <=
                        event$previous_ndvi - min_decline

                    # A recovery breaks the disturbance sequence.
                    # Only count consecutive confirmations before the first recovery.
                    n_confirmed <- if (length(confirmed) == 0) {
                        0L
                    } else {
                        sum(cumprod(as.integer(confirmed)))
                    }

                    persistence <- if (n_subsequent > 0) {
                        n_confirmed / n_subsequent
                    } else {
                        NA_real_
                    }

                    classification <- if (
                        n_subsequent < min_confirmations
                    ) {
                        "insufficient_evidence"
                    } else if (
                        n_confirmed >= min_confirmations &&
                        persistence >= min_persistence
                    ) {
                        "persistent"
                    } else {
                        "transient"
                    }

                    tibble::tibble(
                        previous_date = event$previous_date,
                        disturbance_date = event$date,
                        ndvi_decline = event$change,
                        subsequent_observations = n_subsequent,
                        confirmed_observations = n_confirmed,
                        persistence = persistence,
                        classification = classification
                    )
                }
            )

            # Prefer the earliest persistent disturbance
            persistent <- evaluated |>
                dplyr::filter(
                    .data$classification == "persistent"
                )

            if (nrow(persistent) > 0) {

                persistent |>
                    dplyr::slice_min(
                        .data$disturbance_date,
                        n = 1,
                        with_ties = FALSE
                    )

            } else {

                # No persistent event: report largest decline
                evaluated |>
                    dplyr::slice_min(
                        .data$ndvi_decline,
                        n = 1,
                        with_ties = FALSE
                    )
            }

        }) |>
        dplyr::ungroup()
}
