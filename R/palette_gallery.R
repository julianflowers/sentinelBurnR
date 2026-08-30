palette_gallery <- function() {

    palettes <- list(

        Burn = sbr_palette_burn(256),

        Moisture = sbr_palette_moisture(256),

        Stress = sbr_palette_stress(256),

        Vegetation = sbr_palette_vegetation(256)

    )

    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par))

    graphics::par(
        mar = c(1, 8, 2, 2)
    )

    n <- length(palettes)

    graphics::plot(
        NA,
        xlim = c(0, 1),
        ylim = c(0, n),
        xaxt = "n",
        yaxt = "n",
        xlab = "",
        ylab = "",
        bty = "n"
    )

    for (i in seq_along(palettes)) {

        cols <- palettes[[i]]

        x <- seq(
            0,
            1,
            length.out = length(cols) + 1
        )

        for (j in seq_along(cols)) {

            graphics::rect(
                x[j],
                n - i,
                x[j + 1],
                n - i + 0.8,
                col = cols[j],
                border = NA
            )

        }

        graphics::text(
            -0.02,
            n - i + 0.4,
            labels = names(palettes)[i],
            adj = 1,
            xpd = TRUE
        )

    }

    invisible(palettes)

}

sbr_palette_burn <- function(n = 256) {

    grDevices::colorRampPalette(

        c(
            "#2166AC",
            "#67A9CF",
            "#F7F7F7",
            "#FDAE61",
            "#B2182B"
        )

    )(n)

}


sbr_palette_moisture <- function(n = 256) {

    grDevices::colorRampPalette(

        c(
            "#8C510A",
            "#D8B365",
            "#F6E8C3",
            "#C7EAE5",
            "#01665E"
        )

    )(n)

}


sbr_palette_stress <- function(n = 256) {

    grDevices::colorRampPalette(

        c(
            "#2C7BB6",
            "#ABD9E9",
            "#FFFFBF",
            "#FDAE61",
            "#D7191C"
        )

    )(n)

}


sbr_palette_vegetation <- function(n = 256) {

    grDevices::colorRampPalette(

        c(
            "#F7FCF5",
            "#C7E9C0",
            "#74C476",
            "#238B45",
            "#00441B"
        )

    )(n)

}

palette_lookup <- function(name, n = 256) {

    switch(

        tolower(name),

        burn = sbr_palette_burn(n),

        moisture = sbr_palette_moisture(n),

        stress = sbr_palette_stress(n),

        vegetation = sbr_palette_vegetation(n),

        stop(
            "Unknown palette: ",
            name,
            call. = FALSE
        )

    )

}
