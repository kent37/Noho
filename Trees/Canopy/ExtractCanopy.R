# Extract the Northampton-clipped NLCD Tree Canopy Cover rasters (1985-2023),
# the town outline, and the wards/precincts polygons into a single
# GeoPackage. This is what Trees/Canopy/Canopy_compare/app.R deploys with --
# small enough for shinyapps.io's free-tier 1 GB data limit, unlike the
# ~135 GB of source CONUS zip archives.
#
# Usage:
#   Rscript Trees/Canopy/ExtractCanopy.R

library(terra)
library(sf)
library(tidyverse)

source(here::here('Trees/Canopy/NLCD_helpers.R'))

out_path = here::here('Trees/Canopy/Canopy_compare/canopy_data.gpkg')
if (file.exists(out_path)) file.remove(out_path)

wards = read_sf(here::here(
  'Shapefiles/Wards_Precincts_2020_Northampton_20211022/Wards_Precincts_2020_Northampton.shp'))

lc_files = list.files(
  here::here('data/nlcd_tcc_CONUS_1985_2023_v2023-5_wgs84'),
  pattern = '^nlcd_tcc_CONUS_\\d{4}_v2023-5_wgs84\\.zip$',
  full.names = TRUE)
years = str_extract(basename(lc_files), '\\d{4}')

# Each year becomes its own single-band raster table (canopy_<year>). GDAL's
# GeoPackage driver only uses its compact RGBA/PNG tile storage for Byte
# data -- for any other datatype it switches to the "2D gridded coverage"
# extension, which keeps one real-valued band instead of silently expanding
# a single band into 4 (R=G=B=value, A=mask). Int16 easily covers the 0-99
# canopy percent range (plus the 254 non-processing marker, left as-is here
# so downstream code keeps applying the same 254->0 convention it already
# uses when reading straight from source rasters).
walk2(lc_files, years, function(path, year) {
  layer = read_layer(path)
  # Source rasters carry a color table for display as land-cover categories;
  # drop it so writeRaster treats this as a plain numeric band instead of
  # trying (and failing) to preserve a color table on a non-Byte datatype.
  coltab(layer) = NULL
  levels(layer) = NULL

  table = paste0('canopy_', year)
  gdal_opts = paste0('RASTER_TABLE=', table)
  if (file.exists(out_path)) gdal_opts = c('APPEND_SUBDATASET=YES', gdal_opts)
  writeRaster(layer, out_path, filetype = 'GPKG', datatype = 'INT2S',
              gdal = gdal_opts)
}, .progress = 'Extracting layers')

st_write(noho, out_path, layer = 'noho_outline',
         append = FALSE, delete_layer = TRUE, quiet = TRUE)
st_write(wards, out_path, layer = 'wards_precincts',
         append = FALSE, delete_layer = TRUE, quiet = TRUE)

cat('Wrote', out_path, sprintf('(%.1f MB)\n', file.info(out_path)$size / 1024^2))
