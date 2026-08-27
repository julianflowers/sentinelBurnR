library(sf)
library(httr2)
needs(lwgeom)

get_dimensions <- function(x) {

    # Minimum rotated rectangle
    rect <- sf::st_minimum_rotated_rectangle(
        sf::st_geometry(x)
    )

    # Coordinates of rectangle
    xy <- sf::st_coordinates(rect)

    # Consecutive corner-to-corner distances
    d <- sqrt(
        diff(xy[, "X"])^2 +
            diff(xy[, "Y"])^2
    )

    # Rectangle has two pairs of equal sides
    sides <- sort(unique(round(d, 6)))

    c(
        width_m  = min(sides),
        length_m = max(sides)
    )
}

get_dimensions(land_clipped[1,])

dunwich_centroid <- st_sfc(
    st_point(c(646315, 271425)),
    crs = 27700
)

buffer <- st_buffer(dunwich_centroid, 2500)

bbox <- st_bbox(buffer)

bbox_string <- paste(
    bbox["xmin"],
    bbox["ymin"],
    bbox["xmax"],
    bbox["ymax"],
    sep = ","
)

bbox_string

get_ngd_features <- function(
        collection,
        bbox,
        api_key,
        limit = 100
) {

    base_url <- paste0(
        "https://api.os.uk/features/ngd/ofa/v1/collections/",
        collection,
        "/items"
    )

    all_features <- list()
    offset <- 0

    repeat {

        message("Downloading features ", offset + 1, "–", offset + limit)

        response <- request(base_url) |>
            req_url_query(
                key = api_key,
                bbox = bbox,
                `bbox-crs` = "http://www.opengis.net/def/crs/EPSG/0/27700",
                crs = "http://www.opengis.net/def/crs/EPSG/0/27700",
                limit = limit,
                offset = offset
            ) |>
            req_perform()

        json <- resp_body_json(response)

        features <- json$features

        n <- length(features)

        message("  returned ", n, " features")

        if (n == 0) {
            break
        }

        all_features <- c(all_features, features)

        if (n < limit) {
            break
        }

        offset <- offset + limit
    }

    message("Total features: ", length(all_features))

    geojson <- list(
        type = "FeatureCollection",
        features = all_features
    )

    tmp <- tempfile(fileext = ".geojson")

    jsonlite::write_json(
        geojson,
        tmp,
        auto_unbox = TRUE,
        null = "null"
    )

    st_read(tmp, quiet = TRUE)
}

land_clipped <- download_ngd(
    collection = "lnd-fts-land-1",
    area = buffer,
    key = Sys.getenv("OSDATAHUB"),
    clip = TRUE
)

land_clipped$landcover_b <- vapply(
    land$oslandcovertierb,
    function(x) {
        if (length(x) == 0 || is.null(x)) {
            NA_character_
        } else {
            paste(x, collapse = "; ")
        }
    },
    character(1)
)

ggplot(land_clipped) +
    geom_sf(aes(fill = landcover_b, colour = NA)) +
    theme_minimal()

library(dplyr)

land <- land |>
    mutate(
        cover_group = case_when(

            # Woodland
            grepl("^Coniferous Trees", landcover_b) ~ "Coniferous woodland",
            grepl("^Non-Coniferous Trees", landcover_b) ~ "Broadleaved woodland",

            # Open habitats
            grepl("^Heath", landcover_b) ~ "Heath",
            grepl("^Rough Grassland", landcover_b) ~ "Rough grassland",
            grepl("^Scrub", landcover_b) ~ "Scrub",

            # Wetland
            grepl("^Saltmarsh", landcover_b) ~ "Saltmarsh",
            grepl("^Marsh", landcover_b) ~ "Marsh",

            # Coastal / bare
            grepl("^Sand", landcover_b) ~ "Sand",
            grepl("^Shingle", landcover_b) ~ "Shingle",
            grepl("^Mud", landcover_b) ~ "Mud",
            grepl("^Bare Earth", landcover_b) ~ "Bare earth / grass",

            # Human
            grepl("^Residential Garden", landcover_b) ~ "Residential garden",
            grepl("^Made |^Under Construction|^Excavated",
                  landcover_b) ~ "Built / disturbed",

            # Other tree categories
            grepl("^Scattered Coniferous Trees", landcover_b) ~
                "Scattered trees",
            grepl("^Scattered Non-Coniferous Trees", landcover_b) ~
                "Scattered trees",

            grepl("^Orchard", landcover_b) ~ "Orchard",

            TRUE ~ "Other"
        )
    )

library(dplyr)

land_clipped <- land_clipped |>
  mutate(
    cover_group = case_when(

      # Woodland
      grepl("^Coniferous Trees", landcover_b) ~ "Coniferous woodland",
      grepl("^Non-Coniferous Trees", landcover_b) ~ "Broadleaved woodland",

      # Open habitats
      grepl("^Heath", landcover_b) ~ "Heath",
      grepl("^Rough Grassland", landcover_b) ~ "Rough grassland",
      grepl("^Scrub", landcover_b) ~ "Scrub",

      # Wetland
      grepl("^Saltmarsh", landcover_b) ~ "Saltmarsh",
      grepl("^Marsh", landcover_b) ~ "Marsh",

      # Coastal / bare
      grepl("^Sand", landcover_b) ~ "Sand",
      grepl("^Shingle", landcover_b) ~ "Shingle",
      grepl("^Mud", landcover_b) ~ "Mud",
      grepl("^Bare Earth", landcover_b) ~ "Bare earth / grass",

      # Human
      grepl("^Residential Garden", landcover_b) ~ "Residential garden",
      grepl("^Made |^Under Construction|^Excavated",
            landcover_b) ~ "Built / disturbed",

      # Other tree categories
      grepl("^Scattered Coniferous Trees", landcover_b) ~
        "Scattered trees",
      grepl("^Scattered Non-Coniferous Trees", landcover_b) ~
        "Scattered trees",

      grepl("^Orchard", landcover_b) ~ "Orchard",

      TRUE ~ "Other"
    )
  )

cover_cols <- c(
    "Bare earth / grass"   = "#C8B58B",
    "Broadleaved woodland" = "#68A357",
    "Built / disturbed"    = "#A6A6A6",
    "Coniferous woodland"  = "#27632A",
    "Heath"                = "#A6789D",
    "Marsh"                = "#73A6A2",
    "Residential garden"   = "#B7D68C",
    "Rough grassland"      = "#C9C76B",
    "Sand"                 = "#E5D39B",
    "Scattered trees"      = "#9ABD78",
    "Scrub"                = "#718C52",
    "Shingle"              = "#C6B99D"
)

ggplot(land_clipped) +
    geom_sf(
        aes(fill = cover_group),
        colour = NA
    ) +
    geom_sf(
        data = buffer,
        fill = NA,
        colour = "black",
        linewidth = 0.4
    ) +
    scale_fill_manual(
        values = cover_cols,
        name = "Land cover"
    ) +
    coord_sf(crs = 27700) +
    labs(
        title = "Land cover around Dunwich Forest",
        subtitle = "OS NGD Land Cover – 4 km radius"
    ) +
    theme_minimal() +
    theme(
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = "right"
    )

land_clipped |>
    count(cover_group) |>
    arrange(-n) |>
    st_drop_geometry()

dims <- t(
    vapply(
        seq_len(nrow(land_clipped)),
        function(i) get_dimensions(land_clipped[i, ]),
        numeric(2)
    )
)

land_clipped$width_m  <- round(dims[, "width_m"], 1)
land_clipped$length_m <- round(dims[, "length_m"], 1)

library(mapview)

land_map <- land_clipped |> select(cover_group, area, ends_with("m"))


m <- mapview::mapview(
    land_map,
    zcol = "cover_group",
    col.regions = unname(cover_cols),
    popup = TRUE,
    layer.name = "Land cover",
    alpha.regions = 0.6,
    lwd = 0
)

m

cover_cols <- c(
  "Bare earth / grass"   = "#C8B58B",
  "Broadleaved woodland" = "#68A357",
  "Built / disturbed"    = "#A6A6A6",
  "Coniferous woodland"  = "#27632A",
  "Heath"                = "#A6789D",
  "Marsh"                = "#73A6A2",
  "Residential garden"   = "#B7D68C",
  "Rough grassland"      = "#C9C76B",
  "Sand"                 = "#E5D39B",
  "Scattered trees"      = "#9ABD78",
  "Scrub"                = "#718C52",
  "Shingle"              = "#C6B99D"
)

library(sf)
parishes <- st_read(
    "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/PARNCP_MAY_2026_EW_BFC/FeatureServer/0/query?f=json&where=1%3D1&outFields=*&geometryType=esriGeometryPolygon&geometry=%7B%22spatialReference%22%3A%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%2C%22falseM%22%3A-100000%2C%22falseX%22%3A-20037700%2C%22falseY%22%3A-30241100%2C%22falseZ%22%3A-100000%2C%22mTolerance%22%3A0.001%2C%22mUnits%22%3A10000%2C%22xyTolerance%22%3A0.001%2C%22xyUnits%22%3A10000%2C%22zTolerance%22%3A0.001%2C%22zUnits%22%3A10000%7D%2C%22rings%22%3A%5B%5B%5B164072.51900624373%2C6805232.775183248%5D%2C%5B157172.41760842616%2C6806920.207597845%5D%2C%5B152880.6510695415%2C6813078.58048034%5D%2C%5B148067.2990026337%2C6838793.428802989%5D%2C%5B147524.51595776132%2C6850686.222023766%5D%2C%5B127859.0888983233%2C6858499.1189860655%5D%2C%5B124131.68228801075%2C6863356.769855521%5D%2C%5B124131.68228801075%2C6867497.874577162%5D%2C%5B126584.32237752275%2C6871442.040676173%5D%2C%5B144145.75173369353%2C6881772.624937729%5D%2C%5B138831.19181367318%2C6913678.986889215%5D%2C%5B134561.11655844393%2C6919849.556887686%5D%2C%5B135232.41686940828%2C6924948.588985568%5D%2C%5B139484.47345651605%2C6928958.967094745%5D%2C%5B154216.91118777738%2C6931899.260197351%5D%2C%5B170556.12545443175%2C6940840.147678819%5D%2C%5B187105.97337830742%2C6945202.521723979%5D%2C%5B229171.30018315313%2C6935502.812455883%5D%2C%5B235625.75271688018%2C6927693.768372338%5D%2C%5B245630.19508028816%2C6885803.977641426%5D%2C%5B246365.51375513274%2C6866600.849287319%5D%2C%5B242348.26918650378%2C6856149.069601801%5D%2C%5B230149.28808210487%2C6835006.114020901%5D%2C%5B221812.79884222557%2C6827324.1168357115%5D%2C%5B209130.17189749004%2C6824275.436331211%5D%2C%5B205186.0057984784%2C6826728.0764207225%5D%2C%5B200725.15565148642%2C6833484.999011192%5D%2C%5B192088.6980659123%2C6814993.050773415%5D%2C%5B164072.51900624373%2C6805232.775183248%5D%5D%5D%2C%22type%22%3A%22esriGeometryPolygon%22%7D"
)

localP <- parishes |>
    filter(str_detect(PARNCP26NM, "Westleton|Dunwich|Darsham|Walberswick|Blyth"))


m +
    mapview(localP,
            alpha.regions = 0,
            color = "red",
            lwd = 3,
            popup = TRUE,
            layer.name = "Parishes"
    )




land_clipped <- land_clipped |>
    mutate(area = st_area(geometry))

names(land_clipped)
