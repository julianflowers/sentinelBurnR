#' Analyse vegetation condition
#'
#' Build a Sentinel-2 composite and calculate vegetation
#' condition indices.
#'
#' @param collection An `sbr_collection`.
#' @param assets Sentinel-2 assets used to build the composite.
#'
#' @return An object of class `sbr_vegetation`.
#'
#' @export
analyse_vegetation <- function(
        collection,
        assets = s2_vegetation_assets
) {

    if (!inherits(collection, "sbr_collection")) {
        stop(
            "`collection` must be an sbr_collection.",
            call. = FALSE
        )
    }

    message("Building vegetation composite...")

    composite <- build_composite(
        collection,
        assets = assets
    )

    message("Calculating NDVI...")

    ndvi <- calc_ndvi(
        composite
    )

    message("Calculating NDMI...")

    ndmi <- calc_ndmi(
        composite
    )

    message("Calculating MSI...")

    msi <- calc_msi(
        composite
    )

    structure(
        list(
            composite = composite,
            ndvi = ndvi,
            ndmi = ndmi,
            msi = msi,
            assets = assets
        ),
        class = "sbr_vegetation"
    )
    veg$provenance <- build_vegetation_provenance(
        collection,
        assets
    )

    veg

}
