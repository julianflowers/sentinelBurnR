#' Spectral index metadata
#'
#' Internal metadata defining plotting and display properties for
#' supported spectral indices.
#'
#' @format A named list.
#'
#' @keywords internal
index_info <- list(

    nbr = list(
        name = "NBR",
        title = "Normalized Burn Ratio",
        description = "Vegetation burn severity",
        palette = "burn",
        limits = c(-1, 1)
    ),

    dnbr = list(
        name = "dNBR",
        title = "Differenced Normalized Burn Ratio",
        description = "Change in burn severity",
        palette = "burn",
        limits = c(-0.5, 1)
    ),

    ndmi = list(
        name = "NDMI",
        title = "Normalized Difference Moisture Index",
        description = "Vegetation moisture content",
        palette = "moisture",
        limits = c(-1, 1)
    ),

    msi = list(
        name = "MSI",
        title = "Moisture Stress Index",
        description = "Vegetation moisture stress",
        palette = "stress",
        limits = c(0, 3)
    ),

    ndvi = list(
        name = "NDVI",
        title = "Normalized Difference Vegetation Index",
        description = "Vegetation ",
        palette = "vegetation",
        limits = c(-1, 1)
    )


)

