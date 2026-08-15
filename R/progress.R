new_progress <- function(total) {

    path <- tempfile(
        pattern = "sentinel-progress-",
        fileext = ".rds"
    )

    saveRDS(

        list(

            total = total,
            completed = 0,
            downloaded = 0,
            cached = 0,
            failed = 0,
            start = Sys.time()

        ),

        path

    )

    path

}

update_progress <- function(path, status) {

    x <- readRDS(path)

    x$completed <- x$completed + 1

    x[[status]] <- x[[status]] + 1

    saveRDS(
        x,
        path
    )

}

print_progress <- function(path) {

    x <- readRDS(path)

    elapsed <- as.numeric(

        difftime(

            Sys.time(),

            x$start,

            units = "secs"

        )

    )

    rate <- x$completed / max(elapsed, 1)

    eta <- (x$total - x$completed) / max(rate, 1e-6)

    cat(

        sprintf(

            "\rCompleted %d/%d | Downloaded %d | Cached %d | Failed %d | ETA %.1f min",

            x$completed,

            x$total,

            x$downloaded,

            x$cached,

            x$failed,

            eta / 60

        )

    )

    utils::flush.console()

}

