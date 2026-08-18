#' Run a function sequentially or in parallel
#' parallel_apply
#' @param X Input vector.
#' @param FUN Function to apply.
#' @param workers Number of workers.
#' @keywords internal
#' @export

parallel_apply <- function(
        X,
        FUN,
        workers = 1,
        ...
) {

    workers <- as.integer(workers)

    if (workers <= 1) {

        p <- progressr::progressor(
            steps = length(X)
        )

        return(

            lapply(

                X,

                function(x) {

                    result <- FUN(
                        x,
                        ...
                    )

                    p()

                    result

                }

            )

        )

    }

    old_plan <- future::plan()

    on.exit(

        future::plan(old_plan),

        add = TRUE

    )

    future::plan(

        future::multisession,

        workers = workers

    )

    p <- progressr::progressor(
        steps = length(X)
    )

    future.apply::future_lapply(

        X,

        function(x) {

            result <- FUN(
                x,
                ...
            )

            p()

            result

        },

        future.seed = TRUE,

        future.packages = "sentinelBurnR"

    )

}
