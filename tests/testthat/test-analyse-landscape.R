test_that("analyse_landscape returns an sbr_landscape", {

    landcover <- sf::st_sf(
        cover = c("Coniferous woodland", "Heath"),
        geometry = sf::st_sfc(
            sf::st_polygon(list(rbind(
                c(0, 0), c(100, 0),
                c(100, 100), c(0, 100),
                c(0, 0)
            ))),
            sf::st_polygon(list(rbind(
                c(100, 0), c(200, 0),
                c(200, 100), c(100, 100),
                c(100, 0)
            ))),
            crs = 27700
        )
    )

    x <- analyse_landscape(landcover, category = "cover")

    expect_s3_class(x, "sbr_landscape")
    expect_s3_class(x$landcover, "sf")
    expect_null(x$habitat)
    expect_null(x$transport)
})

test_that("summarise_landcover calculates area by category", {

    landcover <- sf::st_sf(
        cover = c(
            "Coniferous woodland",
            "Heath"
        ),
        geometry = sf::st_sfc(
            sf::st_polygon(list(rbind(
                c(0, 0),
                c(100, 0),
                c(100, 100),
                c(0, 100),
                c(0, 0)
            ))),
            sf::st_polygon(list(rbind(
                c(100, 0),
                c(200, 0),
                c(200, 100),
                c(100, 100),
                c(100, 0)
            ))),
            crs = 27700
        )
    )

    x <- summarise_landcover(
        landcover,
        category = "cover"
    )

    expect_equal(nrow(x), 2)

    expect_equal(
        x$area_ha,
        c(1, 1)
    )

    expect_equal(
        x$n,
        c(1, 1)
    )
})

test_that("summarise_landcover checks category", {

    landcover <- sf::st_sf(
        cover = "Heath",
        geometry = sf::st_sfc(
            sf::st_point(c(0, 0)),
            crs = 27700
        )
    )

    expect_error(
        summarise_landcover(
            landcover,
            "missing"
        ),
        "not found"
    )
})

test_that("calculate_interface measures a shared polygon interface", {

    source <- sf::st_sf(
        geometry = sf::st_sfc(
            sf::st_polygon(list(rbind(
                c(0, 0),
                c(100, 0),
                c(100, 100),
                c(0, 100),
                c(0, 0)
            ))),
            crs = 27700
        )
    )

    target <- sf::st_sf(
        geometry = sf::st_sfc(
            sf::st_polygon(list(rbind(
                c(100, 0),
                c(200, 0),
                c(200, 100),
                c(100, 100),
                c(100, 0)
            ))),
            crs = 27700
        )
    )

    x <- calculate_interface(
        source,
        target,
        distance = 0
    )

    expect_equal(
        x$length_m,
        100,
        tolerance = 0.001
    )

    expect_equal(
        x$length_km,
        0.1,
        tolerance = 0.000001
    )

    expect_equal(
        x$distance_m,
        0
    )
})

test_that("calculate_interface returns zero for separated polygons", {

    source <- sf::st_sf(
        geometry = sf::st_sfc(
            sf::st_polygon(list(rbind(
                c(0, 0),
                c(100, 0),
                c(100, 100),
                c(0, 100),
                c(0, 0)
            ))),
            crs = 27700
        )
    )

    target <- sf::st_sf(
        geometry = sf::st_sfc(
            sf::st_polygon(list(rbind(
                c(200, 0),
                c(300, 0),
                c(300, 100),
                c(200, 100),
                c(200, 0)
            ))),
            crs = 27700
        )
    )

    x <- calculate_interface(
        source,
        target,
        distance = 0
    )

    expect_equal(x$length_m, 0)
})

test_that("calculate_interfaces calculates multiple distances", {

    source <- sf::st_sf(
        geometry = sf::st_sfc(
            sf::st_polygon(list(rbind(
                c(0, 0),
                c(100, 0),
                c(100, 100),
                c(0, 100),
                c(0, 0)
            ))),
            crs = 27700
        )
    )

    target <- sf::st_sf(
        geometry = sf::st_sfc(
            sf::st_polygon(list(rbind(
                c(100, 0),
                c(200, 0),
                c(200, 100),
                c(100, 100),
                c(100, 0)
            ))),
            crs = 27700
        )
    )

    x <- calculate_interfaces(
        source,
        target,
        distances = c(0, 10, 25)
    )

    expect_equal(
        x$distance_m,
        c(0, 10, 25)
    )

    expect_equal(
        x$length_m[[1]],
        100,
        tolerance = 0.001
    )

    expect_true(
        all(diff(x$length_m) >= 0)
    )
})

test_that("analyse_landscape calculates supplied interfaces", {

    landcover <- sf::st_sf(
        cover = c("Woodland", "Heath"),
        geometry = sf::st_sfc(
            sf::st_polygon(list(rbind(
                c(0, 0), c(100, 0),
                c(100, 100), c(0, 100),
                c(0, 0)
            ))),
            sf::st_polygon(list(rbind(
                c(100, 0), c(200, 0),
                c(200, 100), c(100, 100),
                c(100, 0)
            ))),
            crs = 27700
        )
    )

    source <- landcover[1, ]
    target <- landcover[2, ]

    x <- analyse_landscape(
        landcover,
        category = "cover",
        interfaces = list(
            woodland_heath = list(
                source = source,
                target = target
            )
        ),
        distances = c(0, 10)
    )

    expect_s3_class(
        x,
        "sbr_landscape"
    )

    expect_equal(
        x$interface_summary$interface,
        c("woodland_heath", "woodland_heath")
    )

    expect_equal(
        x$interface_summary$distance_m,
        c(0, 10)
    )

    expect_equal(
        x$interface_summary$length_m[[1]],
        100,
        tolerance = 0.001
    )
})

test_that("landscape demo data load", {

    landcover <- demo_data("landcover")
    habitat <- demo_data("habitat")
    transport <- demo_data("transport")

    expect_s3_class(landcover, "sf")
    expect_s3_class(habitat, "sf")
    expect_s3_class(transport, "sf")

    expect_gt(nrow(landcover), 0)
    expect_gt(nrow(habitat), 0)
    expect_gt(nrow(transport), 0)

    expect_true("oslandcovertierb" %in% names(landcover))
    expect_true("mainhabs" %in% names(habitat))
    expect_true("description" %in% names(transport))
})

test_that("summarise_transport summarises transport polygons", {

    transport <- demo_data("transport")

    x <- summarise_transport(transport)

    expect_s3_class(x, "data.frame")

    expect_true(
        all(c(
            "description",
            "n",
            "area_ha"
        ) %in% names(x))
    )

    expect_true(
        all(
            c("Road", "Track") %in%
                x$description
        )
    )

    expect_true(all(x$area_ha > 0))
})
test_that("calculate_interface_transport measures transport at interface", {

    landcover <- demo_data("landcover")
    habitat <- demo_data("habitat")
    transport <- demo_data("transport")

    heath <- habitat |>
        dplyr::filter(
            stringr::str_detect(
                mainhabs,
                "Lowland heathland"
            )
        )

    conifer <- landcover |>
        dplyr::filter(
            purrr::map_lgl(
                oslandcovertierb,
                \(x) any(
                    x %in% c(
                        "Coniferous Trees",
                        "Scattered Coniferous Trees"
                    )
                )
            )
        )

    x <- calculate_interface_transport(
        source = heath,
        target = conifer,
        transport = transport,
        distance = 10
    )

    expect_s3_class(x, "data.frame")

    expect_true(
        all(
            c(
                "category",
                "distance_m",
                "length_m",
                "length_km"
            ) %in% names(x)
        )
    )

    expect_setequal(
        x$category,
        unique(transport$description)
    )

    expect_true(all(x$length_m >= 0))
})

test_that("calculate_interface_transport measures transport crossing interface", {

    source <- sf::st_sf(
        geometry = sf::st_sfc(
            sf::st_polygon(list(
                matrix(
                    c(
                        0,   0,
                        100, 0,
                        100, 100,
                        0,   100,
                        0,   0
                    ),
                    ncol = 2,
                    byrow = TRUE
                )
            )),
            crs = 27700
        )
    )

    target <- sf::st_sf(
        geometry = sf::st_sfc(
            sf::st_polygon(list(
                matrix(
                    c(
                        100, 0,
                        200, 0,
                        200, 100,
                        100, 100,
                        100, 0
                    ),
                    ncol = 2,
                    byrow = TRUE
                )
            )),
            crs = 27700
        )
    )

    # Track crosses the shared boundary between y = 40 and 60
    transport <- sf::st_sf(
        description = "Track",
        geometry = sf::st_sfc(
            sf::st_polygon(list(
                matrix(
                    c(
                        90,  40,
                        110, 40,
                        110, 60,
                        90,  60,
                        90,  40
                    ),
                    ncol = 2,
                    byrow = TRUE
                )
            )),
            crs = 27700
        )
    )

    x <- calculate_interface_transport(
        source = source,
        target = target,
        transport = transport,
        distance = 0
    )

    expect_equal(x$category, "Track")
    expect_equal(x$distance_m, 0)
    expect_equal(x$length_m, 20)
    expect_equal(x$length_km, 0.02)
})

test_that("calculate_interface_transport works with landscape demo data", {

    landcover <- demo_data("landcover")
    habitat <- demo_data("habitat")
    transport <- demo_data("transport")

    heath <- habitat |>
        dplyr::filter(
            stringr::str_detect(
                mainhabs,
                "Lowland heathland"
            )
        )

    conifer <- landcover |>
        dplyr::filter(
            purrr::map_lgl(
                oslandcovertierb,
                \(x) any(
                    x %in% c(
                        "Coniferous Trees",
                        "Scattered Coniferous Trees"
                    )
                )
            )
        )

    x <- calculate_interface_transport(
        heath,
        conifer,
        transport,
        distance = 10
    )

    expect_setequal(
        x$category,
        unique(transport$description)
    )

    expect_true(all(x$length_m >= 0))
})

test_that("analyse_landscape returns transport results", {

    # ... construct/run landscape as in existing test ...

    expect_true(
        all(
            c(
                "transport_summary",
                "interface_transport"
            ) %in% names(landscape)
        )
    )

    expect_s3_class(
        landscape$transport_summary,
        "data.frame"
    )

    expect_s3_class(
        landscape$interface_transport,
        "data.frame"
    )
})

test_that("interface transport lengths are cumulative with distance", {

    x <- landscape$interface_transport |>
        dplyr::group_by(interface, category) |>
        dplyr::summarise(
            cumulative = all(diff(length_m) >= -1e-6),
            .groups = "drop"
        )

    expect_true(all(x$cumulative))
})

test_that("calculate_interface_bands calculates exclusive area bands", {

    source <- sf::st_sf(
        geometry = sf::st_sfc(
            sf::st_polygon(list(
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
                )
            )),
            crs = 27700
        )
    )

    target <- sf::st_sf(
        geometry = sf::st_sfc(
            sf::st_polygon(list(
                matrix(
                    c(
                        100, 0,
                        200, 0,
                        200, 100,
                        100, 100,
                        100, 0
                    ),
                    ncol = 2,
                    byrow = TRUE
                )
            )),
            crs = 27700
        )
    )

    x <- calculate_interface_bands(
        source,
        target,
        distances = c(0, 10, 25, 50)
    )

    expect_equal(
        x$area_ha,
        c(0.1, 0.15, 0.25),
        tolerance = 1e-6
    )
})

test_that("interface bands partition total target area", {

    source <- sf::st_sf(
        geometry = sf::st_sfc(
            sf::st_polygon(list(
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
                )
            )),
            crs = 27700
        )
    )

    target <- sf::st_sf(
        geometry = sf::st_sfc(
            sf::st_polygon(list(
                matrix(
                    c(
                        100, 0,
                        200, 0,
                        200, 100,
                        100, 100,
                        100, 0
                    ),
                    ncol = 2,
                    byrow = TRUE
                )
            )),
            crs = 27700
        )
    )

    bands <- calculate_interface_bands(
        source,
        target,
        distances = c(0, 10, 25, 50, 100)
    )

    target_100 <- sf::st_intersection(
        sf::st_union(target),
        sf::st_buffer(
            sf::st_union(source),
            100
        )
    )

    expected <- as.numeric(
        sf::st_area(target_100)
    ) / 10000

    expect_equal(
        sum(bands$area_ha),
        expected,
        tolerance = 1e-8
    )
})

test_that("sbr_landscape prints", {

    expect_output(
        print(landscape),
        "<sbr_landscape>",
        fixed = TRUE
    )

    expect_invisible(
        print(landscape)
    )
})

test_that("analyse_landscape preserves interface labels", {

    source <- sf::st_sf(
        cover = "Woodland",
        geometry = sf::st_sfc(
            sf::st_polygon(list(
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
                )
            )),
            crs = 27700
        )
    )

    target <- sf::st_sf(
        geometry = sf::st_sfc(
            sf::st_polygon(list(
                matrix(
                    c(
                        100, 0,
                        200, 0,
                        200, 100,
                        100, 100,
                        100, 0
                    ),
                    ncol = 2,
                    byrow = TRUE
                )
            )),
            crs = 27700
        )
    )

    x <- analyse_landscape(
        landcover = source,
        category = "cover",
        interfaces = list(
            woodland_heath = list(
                source = source,
                target = target,
                source_label = "Woodland",
                target_label = "Heathland"
            )
        ),
        distances = c(0, 10)
    )

    expect_true(
        all(x$interface_summary$source == "Woodland")
    )

    expect_true(
        all(x$interface_summary$target == "Heathland")
    )
})
