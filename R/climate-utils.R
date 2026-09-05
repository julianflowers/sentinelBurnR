#==========================================================
# Split a date range into year/month combinations
#==========================================================

split_months <- function(
        start,
        end
) {

    start <- as.Date(start)
    end <- as.Date(end)

    if (start > end) {

        stop(
            "`start` must be before `end`.",
            call. = FALSE
        )

    }

    months <- seq(

        as.Date(format(start, "%Y-%m-01")),

        as.Date(format(end, "%Y-%m-01")),

        by = "month"

    )

    data.frame(

        year = as.integer(
            format(months, "%Y")
        ),

        month = as.integer(
            format(months, "%m")
        ),

        stringsAsFactors = FALSE

    )

}


#==========================================================
# Climate cache filename
#==========================================================

climate_cache_file <- function(
        source,
        year,
        month,
        variable = "total_precipitation",
        statistic = "daily_sum",
        cache = cache_climate()
) {

    stopifnot(
        length(source) == 1,
        length(year) == 1,
        length(month) == 1
    )

    dir <- file.path(
        cache,
        source,
        variable,
        sprintf("%04d", year)
    )

    dir.create(
        dir,
        recursive = TRUE,
        showWarnings = FALSE
    )

    file.path(
        dir,
        sprintf(
            "%04d_%02d_%s.nc",
            year,
            month,
            statistic
        )
    )

}

#==========================================================
# Expand a bounding box to a minimum size
#==========================================================

expand_bbox <- function(
        bbox,
        min_width = 0.5,
        min_height = 0.5
) {

    stopifnot(length(bbox) == 4)

    north <- bbox[1]
    west  <- bbox[2]
    south <- bbox[3]
    east  <- bbox[4]

    width <- east - west
    height <- north - south

    if (width < min_width) {

        pad <- (min_width - width) / 2

        west <- west - pad
        east <- east + pad

    }

    if (height < min_height) {

        pad <- (min_height - height) / 2

        south <- south - pad
        north <- north + pad

    }

    c(
        north,
        west,
        south,
        east
    )

}
