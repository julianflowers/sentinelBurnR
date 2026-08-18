# sentinelBurnR

**Detect and map wildfire burn scars from Sentinel-2 imagery using a simple end-to-end workflow.**

`sentinelBurnR` automates the process of:

- searching for Sentinel-2 imagery
- downloading cloud-filtered scenes
- building cloud-masked composites
- calculating NBR and dNBR
- classifying burn severity
- estimating burned area
- producing publication-quality maps

---

## Installation

```r
# install.packages("pak")

pak::pak("julianflowers/sentinelBurnR")
```

---

## Quick start

```r

library(sentinelBurnR)

sbr_options(
    project_dir = "~/Projects/sentinelBurnR",
    temp_dir = "~/Library/Caches/sentinelBurnR/tmp"
)

aoi <- create_aoi(  
    xmin = 1.618117,
    ymin = 52.246700,
    xmax = 1.636083,
    ymax = 52.257699
)

pre_images <- search_s2(
    aoi,
    start = "2026-07-14",
    end = "2026-07-29"
)

post_images <- search_s2(
    aoi,
    start = "2026-07-29",
    end = "2026-08-16"
)

pre_collection <- download_s2( pre_images, limit = 10, max_cloud = 30, workers = 6)

post_collection <- download_s2(
    post_images, limit = 20,
    max_cloud = 30,
    workers = 6
    )

fire <- analyse_burn(
    pre_collection,
    post_collection
)

severity <- classify_dnbr(
    fire$dnbr
)

plot_severity(severity)

area_by_class(severity)
```

---

## Workflow

```
AOI
 │
 ▼
Search Sentinel-2
 │
 ▼
Download imagery
 │
 ▼
Cloud masking (SCL)
 │
 ▼
Temporal composites
 │
 ▼
NBR
 │
 ▼
dNBR
 │
 ▼
Burn severity
 │
 ▼
Area statistics & maps
```

---

## Features

- Sentinel-2 STAC search
- Parallel downloads
- Automatic cloud masking using Scene Classification Layer (SCL)
- Multi-scene compositing
- Multi-tile mosaicking
- NBR and dNBR calculation
- Burn severity classification
- Burned area estimation
- Publication-quality plotting
- Project and cache management

---

## Example outputs

*(We'll replace this section with real screenshots once we've generated them.)*

- RGB composite
- dNBR map
- Burn severity map
- Area summary

---

## Documentation

```r
vignette("getting-started", package = "sentinelBurnR")
```

---

## Development status

The package is under active development.

Current functionality includes:

- complete Sentinel-2 download workflow
- burn severity mapping
- plotting
- project management
- automated caching

Planned features include:

- validation against reference fire perimeters
- HTML reporting
- recovery monitoring
- additional burn indices
