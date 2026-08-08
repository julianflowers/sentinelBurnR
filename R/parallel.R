# Run a function sequentially or in parallel
#
# @param X Input vector.
# @param FUN Function to apply.
# @param workers Number of workers.
#' @keywords internal

parallel_apply <- function(
        X,
        FUN,
        workers = 1,
        ...
) {

    workers <- as.integer(workers)

    if (workers <= 1) {

        return(
            lapply(
                X,
                FUN,
                ...
            )
        )

    }

    if (.Platform$OS.type == "windows") {

        message(
            "Parallel downloads are currently disabled on Windows."
        )

        return(
            lapply(
                X,
                FUN,
                ...
            )
        )

    }

    parallel::mclapply(
        X,
        FUN,
        ...,
        mc.cores = workers
    )

}
