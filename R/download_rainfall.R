check_cds <- function() {

    uid <- Sys.getenv("CDSAPI_UID")
    key <- Sys.getenv("CDSAPI_KEY")

    if (uid == "" || key == "") {

        stop(
            "Copernicus CDS credentials not found.\n",
            "Please set CDSAPI_UID and CDSAPI_KEY.",
            call. = FALSE
        )

    }

    invisible(TRUE)

}

rainfall_cache_file <- function(
        year,
        month,
        cache
) {

    file.path(
        cache,
        sprintf(
            "%04d-%02d.nc",
            year,
            month
        )
    )

}

