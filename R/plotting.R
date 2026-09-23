#----plot rgb -----------------------------------
#' Plot an RGB composite
#'
#' @param x A composite containing red, green and blue bands.
#' @param title Plot title.
#' @param subtitle Optional subtitle.
#' @param rgb_stretch Apply stretch - linear, histogram or none
#' @param boundary Optional boundary to overlay on the plot.
#' @return A ggplot object.
#'
#' @export
plot_rgb <- function(

    x,

    title = "Sentinel-2 RGB composite",

    subtitle = NULL,

    rgb_stretch = c(
        "lin",
        "hist",
        "none"),

    boundary = NULL


) {

    if (!inherits(x, "SpatRaster")) {
        stop(
            "`x` must be a SpatRaster.",
            call. = FALSE
        )
    }

    rgb_stretch <- match.arg(rgb_stretch)

    rgb <- x[[c(
        "red",
        "green",
        "blue"
    )]]

    p <- ggplot2::ggplot()

    if (rgb_stretch == "none") {

        p <- p +
            tidyterra::geom_spatraster_rgb(
                data = rgb
            )

    } else {

        p <- p +
            tidyterra::geom_spatraster_rgb(
                data = rgb,
                stretch = rgb_stretch
            )

    }

    p <- p +
        ggplot2::coord_sf(
            expand = FALSE
        ) +
        ggplot2::labs(
            title = title,
            subtitle = subtitle,

        ) +
        theme_sbr_map()

    p <- overlay_boundary(
        p,
        boundary,
        rgb
    )

    p
}

#------ theme sbr ------------------------------------
#' Theme for spatial plots
#'
#' @keywords internal

theme_sbr_map <- function() {



    ggplot2::theme_minimal(
        base_size = 12

    ) +

        ggplot2::theme(

            panel.grid = ggplot2::element_blank(),

            axis.title = ggplot2::element_blank(),

            axis.text = ggplot2::element_blank(),

            axis.ticks = ggplot2::element_blank(),

            plot.title = ggplot2::element_text(
                face = "bold",
                size = 14
            ),

            plot.subtitle = ggplot2::element_text(
                size = 11
            ),

            plot.caption = ggplot2::element_text(

                size = 9,

                colour = "grey40",

                hjust = 0

            ),

            legend.position = "right",

            legend.title = ggplot2::element_text(
                face = "bold"
            )

        )
}

#' Theme for charts
#'
#' @keywords internal

theme_sbr_plot <- function() {

    ggplot2::theme_minimal(
        base_size = 12
    ) +

        ggplot2::theme(

            panel.grid.minor =
                ggplot2::element_blank(),

            panel.grid.major.x =
                ggplot2::element_blank(),

            panel.grid.major.y =
                ggplot2::element_line(
                    colour = "grey85"
                ),

            plot.caption = ggplot2::element_text(

                size = 9,

                colour = "grey40",

                hjust = 0

            ),

            axis.title =
                ggplot2::element_text(),

            axis.text =
                ggplot2::element_text(),

            axis.ticks =
                ggplot2::element_line(),

            plot.title =
                ggplot2::element_text(
                    face = "bold",
                    size = 14
                ),

            plot.subtitle =
                ggplot2::element_text(
                    size = 11
                ),

            legend.position = "right",

            legend.title =
                ggplot2::element_text(
                    face = "bold"
                )

        )

}

#-----plot index ----------------------------------------
#' Plot a continuous raster index
#'
#' @param x A `SpatRaster` containing the index to plot.
#' @param index Character name of the spectral index.
#' @param title Optional plot title.
#' @param subtitle Optional plot subtitle.
#' @param basemap Optional basemap default is FALSE
#' @param basemap_type haracter. Basemap type to use. Supported values include
#'   Stadia map styles such as `"stamen_toner_background"` and
#'   `"alidade_smooth"`, and `"satellite"` for satellite imagery.
#' @param basemap_zoom Integer or `NULL`. Basemap zoom level. If `NULL`, an
#'   appropriate zoom is selected for standard Stadia maps. Satellite imagery
#'   uses the default satellite zoom.
#' @param index_alpha Numeric between 0 and 1. Opacity of the index raster when
#'   plotted over a basemap. Default is `0.75`.
#' @param boundary Optional spatial boundary to overlay on the plot.
#' @param caption Optional plot caption.
#' @examples
#' \dontrun{
#' plot_index(
#'     x,
#'     index = "ndvi",
#'     basemap = TRUE,
#'     basemap_type = "satellite",
#'     basemap_zoom = 14,
#'     index_alpha = 0.65
#' )
#' }
#' @export
plot_index <- function(
        x,
        index,
        title = NULL,
        subtitle = NULL,
        boundary = NULL,
        caption = NULL,
        basemap = FALSE,
        basemap_type = "stamen_toner_background",
        basemap_zoom = NULL,
        index_alpha = 0.75
) {

    if (!inherits(x, "SpatRaster")) {
        stop(
            "`x` must be a terra::SpatRaster.",
            call. = FALSE
        )
    }

    if (terra::nlyr(x) != 1L) {
        stop(
            "`x` must contain exactly one raster layer.",
            call. = FALSE
        )
    }

    index <- tolower(index)

    info <- index_info[[index]]

    if (is.null(info)) {
        stop(
            "Unknown index: ",
            index,
            call. = FALSE
        )
    }

    if (is.null(title)) {
        title <- info$title
    }

    n_colours <- if (!is.null(info$palette_values)) {
        length(info$palette_values)
    } else {
        256
    }

    palette <- palette_lookup(
        info$palette,
        n = n_colours
    )

    scale_args <- list(
        colours = palette,
        na.value = "transparent",
        name = info$name,
        limits = info$limits,
        oob = scales::squish
    )

    if (!is.null(info$palette_values)) {
        scale_args$values <- scales::rescale(
            info$palette_values,
            from = info$limits
        )
    }

    fill_scale <- do.call(
        ggplot2::scale_fill_gradientn,
        scale_args
    )

    # No basemap --------------------------
    if (!basemap) {

    p <- ggplot2::ggplot() +

            tidyterra::geom_spatraster(

                data = x

            ) +

            fill_scale +

            ggplot2::labs(

                title = title,

                subtitle = subtitle,

                caption = caption



            ) +

            ggplot2::coord_sf(

                expand = FALSE

            ) +

            theme_sbr_map()

    p <- overlay_boundary(
        p,
        boundary,
        x
    )

    p

}

    # --------------------------------------------------
    # Basemap
    # --------------------------------------------------

    if (isTRUE(basemap)) {
    map <- get_sbr_basemap(
        x,
        type = basemap_type,
        zoom = basemap_zoom
    )


    # ggmap uses lon/lat coordinates
    x_plot <- terra::project(
        x,
        "EPSG:4326"
    )

    p <- ggmap::ggmap(
        map
    ) +

        tidyterra::geom_spatraster(
            data = x_plot,
            alpha = index_alpha
        ) +

        fill_scale +

        ggplot2::labs(
            title = title,
            subtitle = subtitle,
            caption = caption
        ) +

        theme_sbr_map()
}

    # Boundary must also be lon/lat in this branch

    if (!is.null(boundary)) {

        if (inherits(boundary, "SpatVector")) {
            boundary_plot <- terra::project(
                boundary,
                "EPSG:4326"
            )

            boundary_plot <- sf::st_as_sf(
                boundary_plot
            )
        } else if (inherits(boundary, "sf")) {
            boundary_plot <- sf::st_transform(
                boundary,
                4326
            )
        } else {
            stop(
                "`boundary` must be an sf or SpatVector object.",
                call. = FALSE
            )
        }

        p <- p +
            ggplot2::geom_sf(
                data = boundary_plot,
                fill = NA,
                colour = "black",
                linewidth = 0.5,
                inherit.aes = FALSE
            )
    }

    p
}


# rainfall ----------------------------------------------------------------


#' @importFrom graphics par
#' @importFrom rlang .data
#' @export
plot.sbr_rainfall <- function(
        x,
        title = "Daily rainfall",
        subtitle = NULL,
        ...
) {

    stopifnot(
        inherits(x, "sbr_rainfall")
    )

    ggplot2::ggplot(
        x,
        ggplot2::aes(
            x = .data$date,
            y = .data$precipitation_mm
        )
    ) +

        ggplot2::geom_col(
            fill = "#4C78A8",
            width = 0.9
        ) +

        ggplot2::labs(
            title = title,
            subtitle = subtitle,
            x = NULL,
            y = "Rainfall (mm)"
        ) +

        theme_sbr_plot()

}

# plot severity -----------------------------------------------------------

#' Plot Burn severity
#'
#' @param x A single-layer dNBR SpatRaster.
#' @param title Plot title.
#' @param subtitle Plot subtitle
#' @param caption Optional plot caption
#' @param boundary Optional boundary to overlay on the plot.
#' @return A ggplot object.
#' @export

plot_severity <- function(
        x,
        title = "Burn severity",
        subtitle = NULL,
        caption = NULL,
        boundary = NULL
) {

    ids <- sort(
        unique(
            terra::values(x)
        )
    )

    ids <- ids[!is.na(ids)]

    if (
        length(ids) == 0L ||
        any(!ids %in% seq_along(dnbr_labels))
    ) {
        stop(
            "`x` must be a classified burn-severity raster ",
            "with class values from 1 to ",
            length(dnbr_labels),
            ".",
            call. = FALSE
        )
    }

    x <- terra::as.factor(x)

    levels(x) <- data.frame(
        ID = ids,
        severity = dnbr_labels[ids]
    )


    p <- ggplot2::ggplot() +

        tidyterra::geom_spatraster(
            data = x
        ) +

        ggplot2::scale_fill_manual(
            values = burn_palette,
            drop = FALSE,
            name = "Burn severity"
        ) +

        ggplot2::coord_sf(expand = FALSE) +

        ggplot2::labs(
            title = title,
            subtitle = subtitle,
            caption = caption
        ) +

        ggplot2::theme_minimal()

    add_boundary(
        p,
        boundary = boundary
    )
}

# overlay boundary --------------------------------------------------------

overlay_boundary <- function(
        p,
        boundary,
        raster
) {

    if (is.null(boundary)) {
        return(p)
    }

    boundary <- read_boundary(boundary)

    if (!terra::same.crs(
        boundary,
        raster
    )) {

        boundary <- terra::project(
            boundary,
            terra::crs(raster)
        )
    }

    p +
        tidyterra::geom_spatvector(
            data = boundary,
            fill = NA,
            linewidth = 0.6
        )
}

add_boundary <- function(
        p,
        boundary
) {

    if (is.null(boundary)){
        return(p)

}
if (inherits(boundary, "SpatVector")) {
    boundary <- sf::st_as_sf(boundary)
}

    p +

        ggplot2::geom_sf(
            data = boundary,
            fill = NA
        )

}

add_caption <- function(
        p,
        caption
) {

    if (is.null(caption))
        return(p)

    p +

        ggplot2::labs(
            caption = caption
        )

}


# print sbr_drought -------------------------------------------------------
#' @export
print.sbr_drought <- function(x, ...) {

    s <- x$summary

    cat("<sbr_drought>\n")
    cat(
        "Current date:      ",
        format(s$current_date),
        "\n",
        sep = ""
    )

    cat(
        "Baseline:          ",
        s$baseline_start,
        "-",
        s$baseline_end,
        " (",
        s$baseline_years,
        " years)\n",
        sep = ""
    )

    cat(
        "Seasonal window:   \u00b0C",
        s$window_days,
        " days\n",
        sep = ""
    )

    cat(
        "Valid coverage:    ",
        sprintf("%.1f%%", 100 * s$valid_coverage),
        "\n",
        sep = ""
    )

    cat(
        "Median anomaly:    ",
        sprintf("%.3f", s$anomaly_median),
        "\n",
        sep = ""
    )

    cat(
        "Pixels below -2SD: ",
        sprintf(
            "%.1f%%",
            100 * s$proportion_below_minus_2sd
        ),
        "\n",
        sep = ""
    )

    invisible(x)
}


# plot drought ------------------------------------------------------------

#' Plot drought analysis
#'
#' Plot vegetation moisture anomalies from an `sbr_drought`
#' analysis.
#'
#' @param x An object of class `sbr_drought`.
#' @param index Drought product to plot. Either `"anomaly"` or
#'   `"standardised"`.
#' @param boundary Optional boundary to overlay.
#' @param title Optional plot title.
#' @param subtitle Optional plot subtitle.
#' @param caption Optional plot caption.
#'
#' @return A ggplot object.
#'
#' @export
plot_drought <- function(
        x,
        index = c("anomaly", "standardised"),
        boundary = NULL,
        title = NULL,
        subtitle = NULL,
        caption = NULL
) {

    if (!inherits(x, "sbr_drought")) {
        stop("`x` must be an sbr_drought object.")
    }

    index <- match.arg(index)

    s <- x$summary

    if (index == "anomaly") {

        r <- x$anomaly

        if (is.null(title)) {
            title <- "Vegetation moisture anomaly"
        }

        if (is.null(subtitle)) {
            subtitle <- paste0(
                "NDMI anomaly on ",
                format(s$current_date),
                " relative to ",
                s$baseline_start,
                "\u2013",
                s$baseline_end,
                " seasonal baseline"
            )
        }


    } else {

        r <- x$standardised

        if (is.null(title)) {
            title <- "Standardised vegetation moisture anomaly"
        }

        if (is.null(subtitle)) {
            subtitle <- paste0(
                format(s$current_date),
                " relative to ",
                s$baseline_start,
                "\u2013",
                s$baseline_end,
                " seasonal baseline"
            )
        }

    }

    plot_index(
        r,
        index = names(r)[1],
        title = title,
        subtitle = subtitle,
        boundary = boundary,
        caption = caption    )
}


# basemap -----------------------------------------------------------------



get_sbr_basemap <- function(
        x,
        type = "stamen_toner_background",
        zoom = NULL,
        cache = TRUE,
        verbose = FALSE
) {

    if (!inherits(x, "SpatRaster")) {
        stop(
            "`x` must be a terra::SpatRaster.",
            call. = FALSE
        )
    }

    # Convert raster extent to lon/lat
    extent_poly <- terra::as.polygons(
        terra::ext(x),
        crs = terra::crs(x)
    )

    extent_ll <- terra::project(
        extent_poly,
        "EPSG:4326"
    )

    e <- terra::ext(extent_ll)

    bbox <- c(
        left   = unname(e$xmin),
        bottom = unname(e$ymin),
        right  = unname(e$xmax),
        top    = unname(e$ymax)
    )

    if (identical(type, "satellite")) {

        return(
            get_sbr_satellite(
                bbox = bbox,
                zoom = zoom,
                cache = cache,
                verbose = verbose
            )
        )
    }

    stadia_types <- c(
        "stamen_terrain",
        "stamen_toner",
        "stamen_toner_lite",
        "stamen_watercolor",
        "alidade_smooth",
        "alidade_smooth_dark",
        "outdoors",
        "stamen_terrain_background",
        "stamen_toner_background",
        "stamen_terrain_labels",
        "stamen_terrain_lines",
        "stamen_toner_labels",
        "stamen_toner_lines"
    )

    if (!type %in% stadia_types) {
        stop(
            "Unknown basemap type: ",
            type,
            call. = FALSE
        )
    }

    args <- list(
        bbox = bbox,
        maptype = type
    )

    if (!is.null(zoom)) {
        args$zoom <- zoom
    }

    key <- Sys.getenv("STADIA_MAPS_API_KEY")

    if (!nzchar(key)) {
        stop(
            paste0(
                "A Stadia Maps API key is required for basemaps.\n",
                "Set it in the environment variable ",
                "`STADIA_MAPS_API_KEY`."
            ),
            call. = FALSE
        )
    }

    ggmap::register_stadiamaps(
        key = key,
        write = FALSE
    )

    do.call(
        ggmap::get_stadiamap,
        args
    )
}


# sbr basemap cache -------------------------------------------------------
sbr_basemap_cache_dir <- function() {

    dir <- tools::R_user_dir(
        "sentinelBurnR",
        which = "cache"
    )

    dir <- file.path(
        dir,
        "basemaps"
    )

    if (!dir.exists(dir)) {
        dir.create(
            dir,
            recursive = TRUE,
            showWarnings = FALSE
        )
    }

    dir
}


# basemap cache file ------------------------------------------------------

basemap_cache_file <- function(
        type,
        zoom,
        xmin,
        xmax,
        ymin,
        ymax
) {

    key <- paste0(
        type,
        "_z", zoom,
        "_x", xmin, "-", xmax,
        "_y", ymin, "-", ymax
    )

    file.path(
        sbr_basemap_cache_dir(),
        paste0(key, ".rds")
    )
}

xyz_x <- function(lon, zoom) {
    floor(
        (lon + 180) / 360 * 2^zoom
    )
}


xyz_y <- function(lat, zoom) {

    lat_rad <- lat * pi / 180

    floor(
        (
            1 -
                log(
                    tan(lat_rad) +
                        1 / cos(lat_rad)
                ) / pi
        ) / 2 * 2^zoom
    )
}


# get sbr satellite -------------------------------------------------------

get_sbr_satellite <- function(
        bbox,
        zoom = 14,
        cache = TRUE,
        verbose = FALSE
) {

    key <- Sys.getenv("STADIA_MAPS_API_KEY")

    if (!nzchar(key)) {
        stop(
            paste0(
                "A Stadia Maps API key is required for satellite basemaps.\n",
                "Set `STADIA_MAPS_API_KEY` in your environment."
            ),
            call. = FALSE
        )
    }

    if (is.null(zoom)) {
        zoom <- 14L
    }

    zoom <- as.integer(zoom)

    if (zoom < 0L || zoom > 18L) {
        stop(
            "`zoom` must be between 0 and 18 for satellite imagery.",
            call. = FALSE
        )
    }


   # Required tile range------
    xmin <- xyz_x(
        bbox[["left"]],
        zoom
    )

    xmax <- xyz_x(
        bbox[["right"]],
        zoom
    )

    ymin <- xyz_y(
        bbox[["top"]],
        zoom
    )

    ymax <- xyz_y(
        bbox[["bottom"]],
        zoom
    )

    # Cache ----------------------
    cache_file <- basemap_cache_file(
        type = "satellite",
        zoom = zoom,
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax
    )

    if (
        isTRUE(cache) &&
        file.exists(cache_file)
    ) {

        if (isTRUE(verbose)) {
            message(
                "Using cached satellite basemap."
            )
        }

        return(
            readRDS(cache_file)
        )
    }


    # Required tiles -------------------
    xs <- seq.int(xmin, xmax)
    ys <- seq.int(ymin, ymax)


    # Download tiles -----------------------------------

    tiles <- vector(
        "list",
        length(xs) * length(ys)
    )

    k <- 1L

    for (y in ys) {

        for (x in xs) {

            url <- sprintf(
                paste0(
                    "https://tiles.stadiamaps.com/",
                    "data/satellite/%d/%d/%d.jpg",
                    "?api_key=%s"
                ),
                zoom,
                x,
                y,
                key
            )

            tmp <- tempfile(
                fileext = ".jpg"
            )

            utils::download.file(
                url,
                tmp,
                mode = "wb",
                quiet = TRUE
            )

            tiles[[k]] <- jpeg::readJPEG(
                tmp
            )

            unlink(tmp)

            k <- k + 1L
        }
    }


    # Stitch tiles -------------------------------------

    tile_height <- dim(tiles[[1]])[1]
    tile_width  <- dim(tiles[[1]])[2]

    image <- array(
        0,
        dim = c(
            tile_height * length(ys),
            tile_width * length(xs),
            3
        )
    )

    k <- 1L

    for (iy in seq_along(ys)) {

        for (ix in seq_along(xs)) {

            rows <- (
                (iy - 1L) * tile_height + 1L
            ):(
                iy * tile_height
            )

            cols <- (
                (ix - 1L) * tile_width + 1L
            ):(
                ix * tile_width
            )

            image[
                rows,
                cols,
            ] <- tiles[[k]]

            k <- k + 1L
        }
    }


    # XYZ tile edges -----------------------------------

    tile_x_to_lon <- function(x, z) {
        x / 2^z * 360 - 180
    }

    tile_y_to_lat <- function(y, z) {

        n <- pi - 2 * pi * y / 2^z

        180 / pi *
            atan(
                0.5 *
                    (exp(n) - exp(-n))
            )
    }


    left <- tile_x_to_lon(
        xmin,
        zoom
    )

    right <- tile_x_to_lon(
        xmax + 1L,
        zoom
    )

    top <- tile_y_to_lat(
        ymin,
        zoom
    )

    bottom <- tile_y_to_lat(
        ymax + 1L,
        zoom
    )


    # Convert to ggmap-compatible raster ---------------

    image <- grDevices::as.raster(image)

    class(image) <- c(
        "ggmap",
        "raster"
    )

    attr(
        image,
        "bb"
    ) <- data.frame(
        ll.lat = bottom,
        ll.lon = left,
        ur.lat = top,
        ur.lon = right,
        row.names = "bottom"
    )

    attr(
        image,
        "source"
    ) <- "stadia"

    attr(
        image,
        "maptype"
    ) <- "satellite"

    attr(
        image,
        "zoom"
    ) <- zoom


    if (isTRUE(cache)) {

        saveRDS(
            image,
            cache_file
        )

        if (isTRUE(verbose)) {
            message(
                "Satellite basemap cached."
            )
        }
    }

    image
}


# plot climate ------------------------------------------------------------
#' @export
plot_climate <- function(
        x,
        metric = c("rainfall", "temperature")
) {

    stopifnot(
        inherits(x, "sbr_climate")
    )

    metric <- match.arg(metric)

    if (metric == "rainfall") {

        p <- ggplot2::ggplot(
            x$summary,
            ggplot2::aes(
                x = factor(.data$window_days),
                y = .data$percent_of_normal
            )
        ) +
            ggplot2::geom_col(
                width = 0.65
            ) +
            ggplot2::geom_hline(
                yintercept = 100,
                linetype = "dashed"
            ) +
            ggplot2::labs(
                x = "Window (days)",
                y = "Rainfall (% of normal)",
                title = "Rainfall conditions",
                subtitle = sprintf(
                    "%s relative to %d\u2013%d baseline",
                    x$date,
                    min(x$baseline_years),
                    max(x$baseline_years)
                )
            ) +
            ggplot2::theme_minimal()

        return(p)
    }

    ggplot2::ggplot(
        x$temperature,
        ggplot2::aes(
            x = factor(.data$window_days),
            y = .data$mean_max_anomaly_c
        )
    ) +
        ggplot2::geom_col(
            width = 0.65
        ) +
        ggplot2::geom_hline(
            yintercept = 0,
            linetype = "dashed"
        ) +
        ggplot2::labs(
            x = "Window (days)",
            y = "Mean maximum temperature anomaly (\u00b0C)",
            title = "Temperature conditions",
            subtitle = sprintf(
                "%s relative to %d\u2013%d baseline",
                x$date,
                min(x$baseline_years),
                max(x$baseline_years)
            )
        ) +
        ggplot2::theme_minimal()
}

