## sentinelBurnR 0.0.1.9000

### Performance

* Improved Sentinel-2 compositing performance by caching prepared SCL
  masks by raster grid, avoiding repeated nearest-neighbour resampling
  across spectral bands.
* Scientific behaviour and resulting burn analysis are unchanged.

## New

- Project management
- Cache management
- SCL masking
- Multi-tile compositing
- NBR/dNBR calculation
- Burn area estimation
- Plotting functions
- Download summaries

## Internal

- Reorganised source tree
- Added S3 classes
- Improved package documentation
- Clean R CMD check
