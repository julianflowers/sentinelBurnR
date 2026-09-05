
years <- 2018:2025

aoi <- read_aoi(
    system.file(
        "extdata",
        "dunwich.gpkg",
        package = "sentinelBurnR"
    )
)

boundary <- read_boundary(system.file(
    "extdata",
    "dunwich.gpkg",
    package = "sentinelBurnR"
))

historical_searches <- lapply(
    years,
    function(year) {

        message("Processing ", year)

        start <- as.Date(sprintf("%d-07-09", year))
        end <- as.Date(sprintf("%d-08-18", year))

        x <- search_s2(
            aoi = aoi,
            start = start,
            end = end
        )

        tryCatch(
            select_timeseries(
                x,
                interval = 5,
                max_cloud = 30
            ),
            error = function(e) {
                message(
                    "  ", year, ": ",
                    conditionMessage(e)
                )
                NULL
            }
        )
    }
)

names(historical_searches) <- years
historical_collections <- vector(
    "list",
    length(years)
)

names(historical_collections) <- years

for (year in years) {

    year <- as.character(year)

    search <- historical_searches[[year]]

    if (is.null(search)) {
        message(year, ": no selected acquisitions")
        next
    }

    message(
        year,
        ": downloading ",
        length(search$items$features),
        " items"
    )

    historical_collections[[year]] <- download_s2(
        search,
        assets = c(
            "nir08",
            "swir16",
            "scl"
        ),
        workers = 4
    )
}

annual_ndmi <- list()

for (year in names(historical_collections)) {

    collection <- historical_collections[[year]]

    if (is.null(collection)) {
        next
    }

    message("Building ", year)

    composites <- build_timeseries_composites(
        collection,
        assets = c("nir08", "swir16")
    )

    ndmi <- lapply(
        composites,
        calc_ndmi
    )

    # Align observations within the year to the first raster
    template <- ndmi[[1]]

    ndmi <- lapply(
        ndmi,
        function(x) {
            if (!terra::compareGeom(
                x,
                template,
                stopOnError = FALSE
            )) {
                x <- terra::resample(
                    x,
                    template,
                    method = "bilinear"
                )
            }

            x
        }
    )

    # Seasonal median for this year
    stack <- terra::rast(ndmi)

    annual_ndmi[[year]] <- terra::app(
        stack,
        median,
        na.rm = TRUE
    )

    names(annual_ndmi[[year]]) <- paste0(
        "ndmi_",
        year
    )
}

template <- annual_ndmi[[1]]

annual_ndmi_aligned <- lapply(
    annual_ndmi,
    function(x) {

        if (!terra::compareGeom(
            x,
            template,
            stopOnError = FALSE
        )) {
            x <- terra::resample(
                x,
                template,
                method = "bilinear"
            )
        }

        x
    }
)

historical_stack <- terra::rast(
    annual_ndmi_aligned
)

historical_n <- terra::app(
    historical_stack,
    function(x) sum(!is.na(x))
)

names(historical_n) <- "historical_years"

plot(
    historical_n,
    main = "Historical late-summer NDMI coverage"
)



