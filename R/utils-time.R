format_elapsed <- function(x) {

    x <- round(as.numeric(x))

    h <- x %/% 3600

    m <- (x %% 3600) %/% 60

    s <- x %% 60

    if (h > 0) {

        sprintf("%dh %02dm %02ds", h, m, s)

    } else if (m > 0) {

        sprintf("%dm %02ds", m, s)

    } else {

        sprintf("%ds", s)

    }

}
