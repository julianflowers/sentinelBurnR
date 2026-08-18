# Benchmark repeated per-band SCL preparation against the cached approach.
# Run from the package root after devtools::load_all().

make_stack <- function(nrows, ncols, nlyrs, values) {
    x <- terra::rast(nrows = nrows, ncols = ncols, nlyrs = nlyrs)
    terra::values(x) <- values
    x
}

nlyrs <- 6L
scl <- list(
    T1 = make_stack(
        500, 500, nlyrs,
        rep(c(4, 5, 6, 7, 8, 9), length.out = 500 * 500 * nlyrs)
    )
)
ten_m <- make_stack(1000, 1000, nlyrs, 1)
twenty_m <- make_stack(500, 500, nlyrs, 1)
bands <- list(
    red = list(T1 = ten_m),
    green = list(T1 = ten_m),
    blue = list(T1 = ten_m),
    nir08 = list(T1 = ten_m),
    swir22 = list(T1 = twenty_m)
)

run_before <- function() {
    masks <- lapply(
        bands,
        function(asset) prepare_scl_mask(scl$T1, asset$T1)
    )
    Map(function(asset, mask) mask_scl(asset$T1, mask), bands, masks)
}

run_after <- function() {
    masks <- prepare_scl_masks(scl, bands)
    Map(function(asset, mask) mask_scl(asset$T1, mask$T1), bands, masks)
}

benchmark_stage <- function(fun, iterations = 5L) {
    unname(replicate(iterations, system.time(fun())[["elapsed"]]))
}

before <- benchmark_stage(run_before)
after <- benchmark_stage(run_after)

print(data.frame(
    implementation = c("before", "after"),
    median_seconds = c(stats::median(before), stats::median(after)),
    min_seconds = c(min(before), min(after)),
    max_seconds = c(max(before), max(after)),
    scl_preparations = c(length(bands), attr(prepare_scl_masks(scl, bands), "n_prepared"))
))
