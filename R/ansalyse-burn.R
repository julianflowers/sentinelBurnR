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
#'
#' @export
analyse_burn <- function(
        pre,
        post,
        threshold = 0.27,
        assets = s2_burn_assets
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
