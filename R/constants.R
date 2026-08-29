# Sentinel-2 band lookup
#
# Maps user-friendly names to Sentinel-2 band identifiers.

s2_downloadable_assets <- c(
    "coastal",
    "blue",
    "green",
    "red",
    "rededge1",
    "rededge2",
    "rededge3",
    "nir",
    "nir08",
    "nir09",
    "swir16",
    "swir22",
    "scl",
    "visual",
    "thumbnail"
)

s2_bands <- c(
    coastal  = "B01",
    blue     = "B02",
    green    = "B03",
    red      = "B04",
    rededge1 = "B05",
    rededge2 = "B06",
    rededge3 = "B07",
    nir       = "B08",
    nir08     = "B8A",
    swir16    = "B11",
    swir22    = "B12",
    scl = "SCL"
)

s2_default_assets <- c(

    "red",

    "nir08",

    "swir22",

    "scl",

    "green",

    "blue",

    "swir16"

)

s2_scl_keep <- c(
    4L,  # vegetation
    5L,  # bare soil
    6L,  # water
    7L   # unclassified
)

s2_burn_assets <- c(
    "red",
    "nir08",
    "swir22",
    "blue",
    "green",
    "swir16"
)

s2_rgb_bands <- c(
    "red",
    "green",
    "blue"
)

sbr_palette_nbr <- grDevices::hcl.colors(
    256,
    palette = "Viridis"
)

sbr_palette_dnbr <- grDevices::hcl.colors(
    256,
    palette = "Blue-Red 3"
)

s2_scl_colours <- c(

    "#000000",  # 0 No data

    "#ff0000",  # 1 Saturated

    "#333333",  # 2 Dark area

    "#5a5a5a",  # 3 Cloud shadow

    "#00b050",  # 4 Vegetation

    "#c2b280",  # 5 Bare soil

    "#0070ff",  # 6 Water

    "#bdbdbd",  # 7 Unclassified

    "#d9d9d9",  # 8 Medium cloud

    "#ffffff",  # 9 High cloud

    "#b0ffff",  # 10 Cirrus

    "#ffb000"   # 11 Snow

)


# USGS burn thresholds ----------------------------------------------------

dnbr_thresholds <- c(
    -0.25,
    -0.10,
    0.10,
    0.27,
    0.44,
    0.66
)

dnbr_labels <- c(
    "Enhanced regrowth, high",
    "Enhanced regrowth, low",
    "Unburned",
    "Low severity",
    "Moderate-low severity",
    "Moderate-high severity",
    "High severity"
)

# burn palette ------------------------------------------------------------


burn_palette <- c(

    "#1a9850",

    "#66bd63",

    "#f7f7f7",

    "#fee08b",

    "#fdae61",

    "#f46d43",

    "#d73027"

)
