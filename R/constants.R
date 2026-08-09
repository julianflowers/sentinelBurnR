# Sentinel-2 band lookup
#
# Maps user-friendly names to Sentinel-2 band identifiers.

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
    swir22    = "B12"
)

s2_default_assets <- c(

    "red",

    "nir08",

    "swir22"

)

s2_burn_assets <- c(
    "red",
    "nir08",
    "swir22"
)
