# Dunwich 2026 pre-fire analysis
#
# Exploratory analysis of Sentinel-2 vegetation condition before the
# 2026 Dunwich fire.
#
# This script is a case study / validation analysis and is not part
# of the sentinelBurnR package API.

library(sentinelBurnR)
library(terra)
library(sf)
library(httr2)
library(jsonlite)

# -------------------------------------------------------------------------
# 1. AOI
# -------------------------------------------------------------------------

# Load/define the Dunwich analysis AOI here.
#
# aoi <- ...


# -------------------------------------------------------------------------
# 2. Sentinel-2 time-series search
# -------------------------------------------------------------------------

search <- search_s2(
    aoi = aoi,
    start = as.Date("2026-05-01"),
    end = as.Date("2026-08-30")
)

ts_search <- select_timeseries(
    search,
    interval = 10,
    cloud_cover = 30
)

# Check selected acquisitions
ts_search


# -------------------------------------------------------------------------
# 3. Download moisture and vegetation bands
# -------------------------------------------------------------------------

collection_moisture <- download_s2(
    ts_search,
    assets = c("nir08", "swir16", "scl"),
    workers = 4
)

collection_vegetation <- download_s2(
    ts_search,
    assets = c("red", "nir08", "scl"),
    workers = 4
)


# -------------------------------------------------------------------------
# 4. AOI-level time series
# -------------------------------------------------------------------------

ndmi_ts <- sentinel_timeseries(
    collection_moisture,
    index = "ndmi"
)

msi_ts <- sentinel_timeseries(
    collection_moisture,
    index = "msi"
)

ndvi_ts <- sentinel_timeseries(
    collection_vegetation,
    index = "ndvi"
)

plot_timeseries(ndmi_ts, index = "NDMI")
plot_timeseries(msi_ts, index = "MSI")
plot_timeseries(ndvi_ts, index = "NDVI")


# -------------------------------------------------------------------------
# 5. Pre-fire antecedent conditions
# -------------------------------------------------------------------------

assessment_date <- as.Date("2026-07-29")

antecedent <- antecedent_conditions(
    ndvi = ndvi_ts,
    ndmi = ndmi_ts,
    date = assessment_date,
    window = 45
)

antecedent


# -------------------------------------------------------------------------
# 6. Spatial composites
# -------------------------------------------------------------------------

moisture_composites <- build_timeseries_composites(
    collection_moisture,
    assets = c("nir08", "swir16")
)

vegetation_composites <- build_timeseries_composites(
    collection_vegetation,
    assets = c("red", "nir08")
)


# -------------------------------------------------------------------------
# 7. Antecedent NDMI trend
# -------------------------------------------------------------------------

ndmi_trend <- index_trend(
    moisture_composites,
    index = "ndmi",
    start = as.Date("2026-06-24"),
    end = assessment_date,
    min_obs = 3
)

plot(
    ndmi_trend,
    main = "NDMI trend: 24 June – 29 July 2026"
)

terra::global(
    ndmi_trend,
    fun = quantile,
    probs = c(.01, .05, .10, .25, .50, .75, .90, .95, .99),
    na.rm = TRUE
)


# -------------------------------------------------------------------------
# 8. Absolute pre-fire condition
# -------------------------------------------------------------------------

ndmi_july29 <- calc_ndmi(
    moisture_composites[["2026-07-29"]]
)

names(ndmi_july29) <- "ndmi"

ndmi_july29 <- terra::resample(
    ndmi_july29,
    ndmi_trend,
    method = "bilinear"
)

ndvi_july29 <- calc_ndvi(
    vegetation_composites[["2026-07-29"]]
)

names(ndvi_july29) <- "ndvi"

ndvi_july29 <- terra::resample(
    ndvi_july29,
    ndmi_trend,
    method = "bilinear"
)


# -------------------------------------------------------------------------
# 9. Burn outcome
# -------------------------------------------------------------------------

# `burn_raster` should come from the independently derived burn analysis.
#
# For example:
#
# burn <- analyse_burn(...)
# burn_raster <- burn$burn

burn_for_analysis <- terra::resample(
    burn_raster,
    ndmi_trend,
    method = "near"
)

burn_for_analysis <- terra::mask(
    burn_for_analysis,
    ndmi_trend
)


# -------------------------------------------------------------------------
# 10. OS NGD land cover
# -------------------------------------------------------------------------

download_ngd <- function(
        collection,
        area,
        key = Sys.getenv("OS_API_KEY"),
        limit = 100,
        clip = FALSE,
        quiet = FALSE
) {

    if (!inherits(area, c("sf", "sfc"))) {
        stop("`area` must be an sf or sfc object.")
    }

    if (!nzchar(key)) {
        stop("No OS API key supplied.")
    }

    if (limit > 100) {
        stop("OS NGD API has a maximum limit of 100.")
    }

    area_bng <- sf::st_transform(area, 27700)
    bb <- sf::st_bbox(area_bng)

    bbox_string <- paste(
        bb["xmin"], bb["ymin"],
        bb["xmax"], bb["ymax"],
        sep = ","
    )

    base_url <- paste0(
        "https://api.os.uk/features/ngd/ofa/v1/collections/",
        collection,
        "/items"
    )

    pages <- list()
    offset <- 0

    repeat {

        if (!quiet) {
            message(
                "Requesting features ",
                offset + 1,
                "–",
                offset + limit,
                "..."
            )
        }

        response <- httr2::request(base_url) |>
            httr2::req_url_query(
                key = key,
                bbox = bbox_string,
                `bbox-crs` =
                    "http://www.opengis.net/def/crs/EPSG/0/27700",
                crs =
                    "http://www.opengis.net/def/crs/EPSG/0/27700",
                limit = limit,
                offset = offset
            ) |>
            httr2::req_retry(max_tries = 5) |>
            httr2::req_perform()

        geojson <- httr2::resp_body_string(response)

        parsed <- jsonlite::fromJSON(
            geojson,
            simplifyVector = FALSE
        )

        n_page <- length(parsed$features)

        if (!quiet) {
            message("  received ", n_page)
        }

        if (n_page == 0) {
            break
        }

        tmp <- tempfile(fileext = ".geojson")
        writeLines(geojson, tmp, useBytes = TRUE)

        page <- sf::st_read(tmp, quiet = TRUE)
        unlink(tmp)

        sf::st_crs(page) <- 27700

        pages[[length(pages) + 1]] <- page

        if (n_page < limit) {
            break
        }

        offset <- offset + limit
    }

    if (length(pages) == 0) {
        return(
            sf::st_sf(
                geometry = sf::st_sfc(crs = 27700)
            )
        )
    }

    x <- do.call(rbind, pages)

    if (!quiet) {
        message("Downloaded ", nrow(x), " features in total.")
    }

    hits <- lengths(
        sf::st_intersects(
            x,
            sf::st_union(area_bng)
        )
    ) > 0

    x <- x[hits, ]

    if (clip) {
        x <- sf::st_intersection(
            x,
            sf::st_union(area_bng)
        )
    }

    x
}


land_aoi <- download_ngd(
    collection = "lnd-fts-land-1",
    area = aoi,
    clip = TRUE
)


# -------------------------------------------------------------------------
# 11. Flatten NGD land-cover field
# -------------------------------------------------------------------------

land_aoi$landcover_b <- vapply(
    land_aoi$oslandcovertierb,
    function(x) {

        if (length(x) == 0 || all(is.na(x))) {
            return(NA_character_)
        }

        paste(
            as.character(x),
            collapse = "; "
        )
    },
    character(1)
)


# -------------------------------------------------------------------------
# 12. Construct mutually exclusive fuel classes
# -------------------------------------------------------------------------

has_cover <- function(x, cover) {

    vapply(
        strsplit(x, "; ", fixed = TRUE),
        function(z) cover %in% z,
        logical(1)
    )
}

lc <- land_aoi$landcover_b

is_conifer <- has_cover(lc, "Coniferous Trees")
is_broadleaf <- has_cover(lc, "Non-Coniferous Trees")
is_scattered <- has_cover(lc, "Scattered Non-Coniferous Trees")
is_scattered_conifer <- has_cover(lc, "Scattered Coniferous Trees")
is_heath <- has_cover(lc, "Heath")
is_grass <- has_cover(lc, "Rough Grassland")
is_scrub <- has_cover(lc, "Scrub")
is_bare <- has_cover(lc, "Bare Earth Or Grass")
is_sand <- has_cover(lc, "Sand")
is_shingle <- has_cover(lc, "Shingle")
is_marsh <- has_cover(lc, "Marsh")
is_made_sealed <- has_cover(lc, "Made Sealed")
is_made_unsealed <- has_cover(lc, "Made Unsealed")
is_garden <- has_cover(lc, "Residential Garden")

fuel_class <- rep(NA_character_, length(lc))

fuel_class[is_conifer] <-
    "Coniferous/mixed woodland"

fuel_class[
    is.na(fuel_class) &
        is_broadleaf
] <- "Broadleaved woodland/scrub"

fuel_class[
    is.na(fuel_class) &
        (is_scattered | is_scattered_conifer)
] <- "Tree-influenced heath/grass/scrub"

fuel_class[
    is.na(fuel_class) &
        (is_heath | is_grass | is_scrub)
] <- "Open heath/grass/scrub"

fuel_class[
    is.na(fuel_class) &
        is_bare
] <- "Bare earth/grass"

fuel_class[
    is.na(fuel_class) &
        (is_sand | is_shingle)
] <- "Sand/shingle"

fuel_class[
    is.na(fuel_class) &
        (is_made_sealed | is_made_unsealed | is_garden)
] <- "Built/garden"

fuel_class[
    is.na(fuel_class) &
        is_marsh
] <- "Marsh"

land_aoi$fuel_class <- fuel_class


# -------------------------------------------------------------------------
# 13. Rasterise fuel classes
# -------------------------------------------------------------------------

fuel_lookup <- data.frame(
    fuel_class = sort(unique(na.omit(land_aoi$fuel_class))),
    stringsAsFactors = FALSE
)

fuel_lookup$fuel_id <- seq_len(nrow(fuel_lookup))

land_aoi$fuel_id <- fuel_lookup$fuel_id[
    match(
        land_aoi$fuel_class,
        fuel_lookup$fuel_class
    )
]

land_utm <- terra::project(
    terra::vect(land_aoi),
    terra::crs(ndmi_trend)
)

fuel_raster <- terra::rasterize(
    land_utm,
    ndmi_trend,
    field = "fuel_id"
)

names(fuel_raster) <- "fuel_id"


# -------------------------------------------------------------------------
# 14. Burned vs unburned pre-fire condition
# -------------------------------------------------------------------------

predictors <- c(
    ndvi_july29,
    ndmi_july29,
    ndmi_trend,
    burn_for_analysis,
    fuel_raster
)

names(predictors) <- c(
    "ndvi",
    "ndmi",
    "ndmi_trend",
    "burned",
    "fuel_id"
)

dat <- as.data.frame(
    predictors,
    na.rm = TRUE
)

dat$fuel_class <- fuel_lookup$fuel_class[
    match(
        dat$fuel_id,
        fuel_lookup$fuel_id
    )
]

focus_classes <- c(
    "Broadleaved woodland/scrub",
    "Open heath/grass/scrub",
    "Tree-influenced heath/grass/scrub"
)

focus_dat <- dat[
    dat$fuel_class %in% focus_classes,
]


# -------------------------------------------------------------------------
# 15. Relative NDMI quintiles within fuel class
# -------------------------------------------------------------------------

focus_dat$ndmi_quintile <- ave(
    focus_dat$ndmi,
    focus_dat$fuel_class,
    FUN = function(x) {

        cut(
            x,
            breaks = quantile(
                x,
                probs = seq(0, 1, 0.2),
                na.rm = TRUE
            ),
            include.lowest = TRUE,
            labels = FALSE
        )
    }
)

moisture_response <- aggregate(
    burned ~ fuel_class + ndmi_quintile,
    data = focus_dat,
    FUN = function(x) {
        c(
            n = length(x),
            burned = sum(x),
            burn_rate = mean(x)
        )
    }
)

moisture_response


# -------------------------------------------------------------------------
# 16. Does NDVI add information after relative moisture?
# -------------------------------------------------------------------------

focus_dat$ndvi_group <- ave(
    focus_dat$ndvi,
    focus_dat$fuel_class,
    FUN = function(x) {

        ifelse(
            x <= median(x, na.rm = TRUE),
            "Lower NDVI",
            "Higher NDVI"
        )
    }
)

ndvi_moisture_response <- aggregate(
    burned ~
        fuel_class +
        ndmi_quintile +
        ndvi_group,
    data = focus_dat,
    FUN = function(x) {
        c(
            n = length(x),
            burned = sum(x),
            burn_rate = mean(x)
        )
    }
)

ndvi_moisture_response


# -------------------------------------------------------------------------
# 17. Fuel-class-normalised moisture map
# -------------------------------------------------------------------------

moisture_quintile <- terra::rast(ndmi_trend)

terra::values(moisture_quintile) <- NA

names(moisture_quintile) <- "moisture_quintile"

for (fc in focus_classes) {

    id <- fuel_lookup$fuel_id[
        fuel_lookup$fuel_class == fc
    ]

    habitat_mask <- fuel_raster == id

    x <- terra::mask(
        ndmi_july29,
        habitat_mask,
        maskvalues = 0
    )

    vals <- terra::values(
        x,
        na.rm = TRUE
    )

    breaks <- unique(
        quantile(
            vals,
            probs = seq(0, 1, 0.2),
            na.rm = TRUE
        )
    )

    if (length(breaks) < 2) {
        next
    }

    q <- terra::classify(
        x,
        cbind(
            head(breaks, -1),
            tail(breaks, -1),
            seq_len(length(breaks) - 1)
        ),
        include.lowest = TRUE
    )

    moisture_quintile <- terra::cover(
        moisture_quintile,
        q
    )
}

terra::freq(moisture_quintile)

plot(
    moisture_quintile,
    breaks = 0.5:5.5,
    main = paste(
        "Relative pre-fire vegetation moisture",
        "29 July 2026",
        sep = "\n"
    )
)


# -------------------------------------------------------------------------
# 18. Overlay subsequent burn
# -------------------------------------------------------------------------

burn_poly <- terra::as.polygons(
    burn_for_analysis,
    dissolve = TRUE,
    na.rm = TRUE
)

burn_poly <- burn_poly[
    burn_poly[[1]] == 1,
]

plot(
    moisture_quintile,
    breaks = 0.5:5.5,
    main = paste(
        "Pre-fire relative moisture",
        "and subsequent burn",
        sep = "\n"
    )
)

lines(
    burn_poly,
    lwd = 2
)


# -------------------------------------------------------------------------
# Interpretation
# -------------------------------------------------------------------------
#
# Current exploratory findings:
#
# * Absolute pre-fire NDMI is associated with subsequent burning within
#   the main vegetation/fuel classes.
#
# * NDMI trend is substantially confounded by land-cover composition and
#   does not consistently distinguish burned from unburned pixels within
#   fuel classes.
#
# * NDVI does not show a consistent additional relationship once fuel
#   class and relative moisture state are considered.
#
# * There is no evidence here for a universal NDMI fire-risk threshold.
#
# * Relative moisture condition appears more informative: the wettest
#   portion of each fuel class generally experienced substantially less
#   subsequent burning.
#
# These results are from one fire and are exploratory. Pixel observations
# are spatially autocorrelated and should not be treated as independent
# samples for conventional significance testing.
#
# Next step:
#
# Build a historical seasonal NDMI baseline and calculate pre-fire
# moisture anomaly relative to expected condition.
