#----- calculate dnbr-----------------------------

#' Calculate differenced Normalized Burn Ratio
#'
#' @param pre Pre-fire NBR raster.
#' @param post Post-fire NBR raster.
#'
#' @return A SpatRaster.
#' @export
calc_dnbr <- function(
        pre,
        post
) {

    stopifnot(
        inherits(pre, "SpatRaster"),
        inherits(post, "SpatRaster")
    )

    terra::compareGeom(
        pre,
        post,
        stopOnError = TRUE
    )

    dnbr <- pre - post

    names(dnbr) <- "dnbr"

    dnbr

}

# analyse burn ------------------------------------------------------------

#' Analyse burned area from pre- and post-fire Sentinel-2 collections
#'
#' Builds pre- and post-fire composites, calculates NBR and dNBR,
#' detects burned pixels, classifies burn severity, and estimates
#' burned area.
#'
#' @param pre Pre-fire `sbr_collection`.
#' @param post Post-fire `sbr_collection`.
#' @param threshold Numeric dNBR threshold used to identify burned
#'   pixels. Default is 0.27.
#' @param assets Sentinel-2 assets used to build the composites.
#'
#' @return An object of class `sbr_burn`.
#' @export
analyse_burn <- function(
        pre,
        post,
        threshold = 0.27,
        assets = s2_burn_assets,
        boundary = NULL
) {

    if (!inherits(pre, "sbr_collection")) {
        stop(
            "`pre` must be an sbr_collection.",
            call. = FALSE
        )
    }

    if (!inherits(post, "sbr_collection")) {
        stop(
            "`post` must be an sbr_collection.",
            call. = FALSE
        )
    }

    message("Building pre-fire composite...")

    pre_composite <- build_composite(
        pre,
        assets = assets
    )

    message("Building post-fire composite...")

    post_composite <- build_composite(
        post,
        assets = assets
    )

    message("Calculating NBR...")

    pre_nbr <- calc_nbr(
        pre_composite
    )

    post_nbr <- calc_nbr(
        post_composite
    )

    if (!terra::compareGeom(
        pre_nbr,
        post_nbr,
        stopOnError = FALSE
    )) {
        stop(
            "Pre- and post-fire rasters do not have matching geometry.",
            call. = FALSE
        )
    }

    message("Calculating dNBR...")

    dnbr <- calc_dnbr(
        pre_nbr,
        post_nbr
    )

    message("Detecting burned area...")

    burned <- detect_burn(
        dnbr,
        threshold = threshold
    )

    message("Classifying burn severity...")

    severity <- classify_burn_severity(
        dnbr
    )

    area_ha <- burn_area(
        burned,
        unit = "ha"
    )

    structure(
        list(
            pre_composite = pre_composite,
            post_composite = post_composite,
            pre_nbr = pre_nbr,
            post_nbr = post_nbr,
            dnbr = dnbr,
            burned = burned,
            severity = severity,
            area_ha = area_ha,
            threshold = threshold
        ),
        class = "sbr_burn"
    )
}

#' @export
print.sbr_burn <- function(
        x,
        ...
) {

    cat("sentinelBurnR burn analysis\n")
    cat("---------------------------\n")
    cat(
        sprintf(
            "Burn threshold : %.2f dNBR\n",
            x$threshold
        )
    )
    cat(
        sprintf(
            "Burned area    : %.1f ha\n",
            x$area_ha
        )
    )

    invisible(x)
}


# burn severity -----------------------------------------------------------
#' Classify burn severity from dNBR
#'
#' Classifies a dNBR raster into burn severity classes.
#'
#' @param x A single-layer dNBR SpatRaster.
#'
#' @return A categorical SpatRaster containing burn severity classes.
#'
#' @export
classify_burn_severity <- function(x) {

    if (!inherits(x, "SpatRaster")) {
        stop(
            "`x` must be a SpatRaster.",
            call. = FALSE
        )
    }

    if (terra::nlyr(x) != 1L) {
        stop(
            "`x` must contain one layer.",
            call. = FALSE
        )
    }

    rcl <- matrix(
        c(
            -Inf, -0.25, 1,
            -0.25, -0.10, 2,
            -0.10,  0.10, 3,
            0.10,  0.27, 4,
            0.27,  0.44, 5,
            0.44,  0.66, 6,
            0.66,  Inf, 7
        ),
        ncol = 3,
        byrow = TRUE
    )

    severity <- terra::classify(
        x,
        rcl
    )

    names(severity) <- "severity"

    severity
}


# detect burn -------------------------------------------------------------
#' Detect burned area from dNBR
#'
#' Creates a burned / not-burned raster from a dNBR raster.
#'
#' @param x A single-layer dNBR SpatRaster.
#' @param threshold Numeric dNBR threshold above which cells are
#'   classified as burned.
#'
#' @return A single-layer SpatRaster named `burned`.
#'
#' @export
detect_burn <- function(
        x,
        threshold = 0.27
) {

    if (!inherits(x, "SpatRaster")) {
        stop(
            "`x` must be a SpatRaster.",
            call. = FALSE
        )
    }

    if (terra::nlyr(x) != 1L) {
        stop(
            "`x` must contain one layer.",
            call. = FALSE
        )
    }

    if (!is.numeric(threshold) ||
        length(threshold) != 1L ||
        is.na(threshold)) {

        stop(
            "`threshold` must be a single numeric value.",
            call. = FALSE
        )
    }

    burned <- x >= threshold

    names(burned) <- "burned"

    burned
}


# burn area ---------------------------------------------------------------

#----- burn area -------------------------
#' Detect burned area from dNBR
#'
#' Creates a burned / not-burned raster from a dNBR raster.
#'
#' @param x A single-layer dNBR SpatRaster.
#' @param unit Area units to return (e.g. "ha", "m2", "km2").
#'
#' @return A single-layer SpatRaster named `burned`.
#'
#' @export
burn_area <- function(x, unit = "ha") {

    if (!inherits(x, "SpatRaster")) {
        stop(
            "`x` must be a SpatRaster.",
            call. = FALSE
        )
    }

    area <- terra::cellSize(
        x,
        unit = unit
    )

    area <- terra::mask(
        area,
        x,
        maskvalues = 0
    )

    as.numeric(
        terra::global(
            area,
            "sum",
            na.rm = TRUE
        )[1, 1]
    )
}




# summary sbr collection --------------------------------------------------



summary.sbr_collection <- function(object, ...) {

    files(object)

}

