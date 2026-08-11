#' Create a sentinelBurnR project
#'
#' @param name Project name.
#' @param root Root project directory. If NULL, uses the
#'   `sbr.project_dir` option.
#'
#' @return An sbr_project object.
#'
#' @export

create_project <- function(
        name,
        root = getOption("sbr.project_dir")
) {

    if (is.null(root)) {
        stop(
            "No project root configured. Set it with sbr_options().",
            call. = FALSE
        )
    }

    project_dir <- file.path(
        path.expand(root),
        name
    )

    dirs <- c(
        "raw",
        "composites",
        "indices",
        "outputs",
        "figures",
        "report",
        "logs"
    )

    for (d in dirs) {
        dir.create(
            file.path(project_dir, d),
            recursive = TRUE,
            showWarnings = FALSE
        )
    }

    structure(
        list(
            name = name,
            path = project_dir,
            raw = file.path(project_dir, "raw"),
            composites = file.path(project_dir, "composites"),
            indices = file.path(project_dir, "indices"),
            outputs = file.path(project_dir, "outputs"),
            figures = file.path(project_dir, "figures"),
            report = file.path(project_dir, "report"),
            logs = file.path(project_dir, "logs")
        ),
        class = "sbr_project"
    )
}


