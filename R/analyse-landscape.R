#' Analyse landscape structure
#'
#' Summarises categorical land cover and spatial relationships between
#' supplied landscape features, including interface proximity, target area
#' within distance bands, and the occurrence of mapped transport features
#' along interfaces.
#'
#' `analyse_landscape()` describes landscape structure rather than calculating
#' a wildfire risk score. Interpretation of features such as roads and tracks
#' depends on their physical characteristics and management context.
#'
#' @param landcover An `sf` object containing categorical land-cover polygons.
#' @param category Character string giving the land-cover category column.
#' @param habitat Optional `sf` object containing habitat polygons.
#' @param transport Optional `sf` object containing transport polygons.
#' @param interfaces Optional named list of interfaces. Each element must
#'   contain `source` and `target` `sf` objects and may contain
#'   `source_label` and `target_label`.
#' @param distances Numeric vector of distances in metres used to describe
#'   source-target proximity.
#' @param boundary Optional analysis boundary.
#'
#' @return An object of class `sbr_landscape`.
#'
#' @export

analyse_landscape <- function(
        landcover,
        category = "cover",
        habitat = NULL,
        transport = NULL,
        boundary = NULL,
        interfaces = NULL,
        distances = c(0, 10, 25, 50, 100)
) {

    if (!inherits(landcover, "sf")) {
        stop(
            "`landcover` must be an sf object.",
            call. = FALSE
        )
    }

    if (!is.null(habitat) &&
        !inherits(habitat, "sf")) {
        stop(
            "`habitat` must be an sf object.",
            call. = FALSE
        )
    }

    if (!is.null(transport) &&
        !inherits(transport, "sf")) {
        stop(
            "`transport` must be an sf object.",
            call. = FALSE
        )
    }

    summary <- summarise_landcover(
        landcover,
        category
    )

    interface_summary <- NULL
    interface_bands <- NULL

    if (!is.null(interfaces)) {

        interface_summary <- purrr::imap_dfr(
            interfaces,
            \(interface, name) {

                labels <- interface_labels(interface)

                calculate_interfaces(
                    source = interface[["source"]],
                    target = interface[["target"]],
                    distances = distances
                ) |>
                    dplyr::mutate(
                        interface = name,
                        source = labels$source,
                        target = labels$target,
                        .before = 1
                    )
            }
        )

        interface_bands <- purrr::imap_dfr(
            interfaces,
            \(interface, name) {

                labels <- interface_labels(interface)

                calculate_interface_bands(
                    source = interface[["source"]],
                    target = interface[["target"]],
                    distances = distances
                ) |>
                    dplyr::mutate(
                        interface = name,
                        source = labels$source,
                        target = labels$target,
                        .before = 1
                    )
            }
        )
    }

    transport_summary <- NULL
    interface_transport <- NULL

    if (!is.null(transport) && !is.null(interfaces)) {

        interface_transport <- purrr::imap_dfr(
            interfaces,
            \(interface, name) {

                labels <- interface_labels(interface)

                purrr::map_dfr(
                    distances,
                    \(distance) {

                        calculate_interface_transport(
                            source = interface[["source"]],
                            target = interface[["target"]],
                            transport = transport,
                            distance = distance,
                            category = "description"
                        ) |>
                            dplyr::mutate(
                                interface = name,
                                source = labels$source,
                                target = labels$target,
                                .before = 1
                            )
                    }
                )
            }
        )
    }

    structure(
        list(
            landcover = landcover,
            habitat = habitat,
            transport = transport,
            boundary = boundary,
            summary = summary,
            category = category,
            interface_summary = interface_summary,
            interfaces = interfaces,
            interface_transport = interface_transport,
            distances = distances,
            transport_summary = transport_summary,
            interface_bands = interface_bands
        ),
        class = "sbr_landscape"
    )
}

summarise_landcover <- function(
        landcover,
        category
) {

    if (!category %in% names(landcover)) {
        stop(
            "`category` not found in `landcover`.",
            call. = FALSE
        )
    }

    landcover |>
        dplyr::mutate(
            area_ha = as.numeric(
                sf::st_area(geometry)
        ) / 10000
        ) |>
        sf::st_drop_geometry() |>
        dplyr::group_by(
            .data[[category]]
        ) |>
        dplyr::summarise(
            n = dplyr::n(),
            area_ha = sum(
                .data$area_ha,
                na.rm = TRUE
            ),
            .groups = "drop"
        ) |>
        dplyr::arrange(
            dplyr::desc(.data$area_ha)
        )
}

calculate_interface <- function(
        source,
        target,
        distance = 10
) {

    if (!inherits(source, "sf")) {
        stop("`source` must be an sf object.", call. = FALSE)
    }

    if (!inherits(target, "sf")) {
        stop("`target` must be an sf object.", call. = FALSE)
    }

    if (!is.numeric(distance) ||
        length(distance) != 1 ||
        is.na(distance) ||
        distance < 0) {
        stop(
            "`distance` must be a single non-negative number.",
            call. = FALSE
        )
    }

    if (sf::st_crs(source) != sf::st_crs(target)) {
        target <- sf::st_transform(
            target,
            sf::st_crs(source)
        )
    }

    source <- sf::st_make_valid(source)
    target <- sf::st_make_valid(target)

    source_boundary <- source |>
        sf::st_union() |>
        sf::st_boundary()

    target_zone <- target |>
        sf::st_union() |>
        sf::st_buffer(dist = distance)

    interface <- sf::st_intersection(
        source_boundary,
        target_zone
    )

    length_m <- if (length(interface) == 0) {
        0
    } else {
        interface <- sf::st_union(interface)

        as.numeric(
            sf::st_length(interface)
        )
    }

    list(
        distance_m = distance,
        length_m = length_m,
        length_km = length_m / 1000,
        geometry = interface
    )
}

calculate_interfaces <- function(
        source,
        target,
        distances = c(0, 10, 25, 50, 100)
) {

    if (!is.numeric(distances) ||
        anyNA(distances) ||
        any(distances < 0)) {
        stop(
            "`distances` must contain non-negative numbers.",
            call. = FALSE
        )
    }

    purrr::map_dfr(
        distances,
        \(distance) {

            x <- calculate_interface(
                source = source,
                target = target,
                distance = distance
            )

            tibble::tibble(
                distance_m = x$distance_m,
                length_m = x$length_m,
                length_km = x$length_km
            )
        }
    )
}

summarise_transport <- function(
        transport,
        category = "description"
) {

    if (!inherits(transport, "sf")) {
        stop("`transport` must be an sf object.", call. = FALSE)
    }

    if (!category %in% names(transport)) {
        stop(
            "`category` not found in `transport`.",
            call. = FALSE
        )
    }

    transport |>
        dplyr::mutate(
            area_ha =
                as.numeric(sf::st_area(geometry)) / 10000
        ) |>
        sf::st_drop_geometry() |>
        dplyr::group_by(.data[[category]]) |>
        dplyr::summarise(
            n = dplyr::n(),
            area_ha = sum(.data$area_ha, na.rm = TRUE),
            .groups = "drop"
        ) |>
        dplyr::arrange(
            dplyr::desc(.data$area_ha)
        )
}

calculate_interface_transport <- function(
        source,
        target,
        transport,
        distance = 10,
        category = "description"
) {

    if (!inherits(source, "sf")) {
        stop("`source` must be an sf object.", call. = FALSE)
    }

    if (!inherits(target, "sf")) {
        stop("`target` must be an sf object.", call. = FALSE)
    }

    if (!inherits(transport, "sf")) {
        stop("`transport` must be an sf object.", call. = FALSE)
    }

    if (!category %in% names(transport)) {
        stop("`category` not found in `transport`.", call. = FALSE)
    }

    if (!is.numeric(distance) ||
        length(distance) != 1 ||
        is.na(distance) ||
        distance < 0) {

        stop(
            "`distance` must be a single non-negative number.",
            call. = FALSE
        )
    }

    if (sf::st_crs(target) != sf::st_crs(source)) {
        target <- sf::st_transform(
            target,
            sf::st_crs(source)
        )
    }

    if (sf::st_crs(transport) != sf::st_crs(source)) {
        transport <- sf::st_transform(
            transport,
            sf::st_crs(source)
        )
    }

    source <- sf::st_make_valid(source)
    target <- sf::st_make_valid(target)
    transport <- sf::st_make_valid(transport)

    source_boundary <- source |>
        sf::st_union() |>
        sf::st_boundary()

    target_zone <- target |>
        sf::st_union() |>
        sf::st_buffer(dist = distance)

    purrr::map_dfr(
        unique(transport[[category]]),
        \(type) {

            transport_zone <- transport |>
                dplyr::filter(
                    .data[[category]] == type
                ) |>
                sf::st_union()

            # Fixed part of source boundary associated with
            # this transport category
            transport_boundary <- sf::st_intersection(
                source_boundary,
                transport_zone
            )

            # Of that boundary, how much lies within
            # `distance` of the target?
            x <- sf::st_intersection(
                transport_boundary,
                target_zone
            )

            length_m <- if (length(x) == 0) {
                0
            } else {
                x <- sf::st_union(x)

                as.numeric(
                    sf::st_length(x)
                )
            }

            tibble::tibble(
                category = type,
                distance_m = distance,
                length_m = length_m,
                length_km = length_m / 1000
            )
        }
    )
}

calculate_interface_bands <- function(
        source,
        target,
        distances = c(0, 10, 25, 50, 100)
) {

    if (!inherits(source, "sf")) {
        stop("`source` must be an sf object.", call. = FALSE)
    }

    if (!inherits(target, "sf")) {
        stop("`target` must be an sf object.", call. = FALSE)
    }

    if (!is.numeric(distances) ||
        anyNA(distances) ||
        any(distances < 0)) {

        stop(
            "`distances` must contain non-negative numbers.",
            call. = FALSE
        )
    }

    distances <- sort(unique(distances))

    if (length(distances) < 2) {
        stop(
            "`distances` must contain at least two values.",
            call. = FALSE
        )
    }

    if (sf::st_crs(target) != sf::st_crs(source)) {
        target <- sf::st_transform(
            target,
            sf::st_crs(source)
        )
    }

    source <- source |>
        sf::st_make_valid() |>
        sf::st_union()

    target <- target |>
        sf::st_make_valid() |>
        sf::st_union()

    purrr::map2_dfr(
        distances[-length(distances)],
        distances[-1],
        \(lower, upper) {

            outer <- sf::st_buffer(
                source,
                dist = upper
            )

            if (lower == 0) {

                band <- outer

            } else {

                inner <- sf::st_buffer(
                    source,
                    dist = lower
                )

                band <- sf::st_difference(
                    outer,
                    inner
                )
            }

            target_band <- sf::st_intersection(
                target,
                band
            )

            area_ha <- if (
                length(target_band) == 0 ||
                all(sf::st_is_empty(target_band))
            ) {
                0
            } else {
                sum(
                    as.numeric(sf::st_area(target_band)),
                    na.rm = TRUE
                ) / 10000
            }

            tibble::tibble(
                lower_m = lower,
                upper_m = upper,
                area_ha = area_ha
            )
        }
    )
}

interface_label <- function(x, default) {
    if (is.null(x)) default else x
}

#' Print a landscape analysis
#'
#' @param x An `sbr_landscape` object.
#' @param ... Additional arguments, currently unused.
#'
#' @return `x`, invisibly.
#' @export
print.sbr_landscape <- function(x, ...) {

    cat("<sbr_landscape>\n")

    cat(
        "Land-cover classes:",
        nrow(x$summary),
        "\n"
    )

    if (!is.null(x$transport_summary)) {
        cat(
            "Transport classes:",
            nrow(x$transport_summary),
            "\n"
        )
    }

    if (!is.null(x$interface_summary)) {

        cat(
            "\nSource boundary near target:\n",
            dplyr::n_distinct(
                x$interface_summary$interface
            ),
            "\n"
        )

        cat("\nTarget area by distance from source:\n")

        print(
            x$interface_summary,
            row.names = FALSE
        )
    }

    if (!is.null(x$interface_bands)) {

        cat("\nTransport coinciding with source boundary near target:\n")

        print(
            x$interface_bands,
            row.names = FALSE
        )
    }

    if (!is.null(x$interface_transport)) {

        cat("\nTransport at interface:\n")

        print(
            x$interface_transport,
            row.names = FALSE
        )
    }

    invisible(x)
}

interface_labels <- function(interface) {

    if (!is.list(interface)) {
        stop(
            "Each interface must be a list.",
            call. = FALSE
        )
    }

    list(
        source = interface_label(
            interface[["source_label"]],
            "source"
        ),
        target = interface_label(
            interface[["target_label"]],
            "target"
        )
    )
}
