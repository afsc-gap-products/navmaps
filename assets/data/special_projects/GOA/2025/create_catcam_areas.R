##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Project:       Create Camera Area Polygons for GOA 2025 survey
## Author:        Zack Oyafuso (zack.oyafuso@noaa.gov)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Restart R Session before running
rm(list = ls())

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Import libraries
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(terra)
# library(akgfmaps)
# 
# goa_base <- akgfmaps::get_base_layers(select.region = "goa",
#                                       design.year = 2025,
#                                       set.crs = 4326)

coords <- list(matrix(c(
  -158.8,	54.52,
  -159.0858,	55.0274,
  -155.3,	55.9,
  -155.,	55.5,
  -158.8,	54.52
  # Closing the polygon
), ncol = 2, byrow = TRUE),

matrix(c(
  -154.64,	56.2752,
  -154.0907,	55.7512,
  -150.5,	57.1,
  -151.0,	57.6,
  -154.64,	56.2752
  # Closing the polygon
), ncol = 2, byrow = TRUE),

matrix(c(
  -151.3,	58.7,
  -148.3,	58.7,
  -148.3,	58.2,
  -151.3,	58.2,
  -151.3,	58.7
  # Closing the polygon
), ncol = 2, byrow = TRUE)

)

# Create a SpatVector polygon
poly <- terra::vect(list(coords), type = "polygons", crs = "EPSG:4326")

# Plot the polygon
# plot(poly, col="lightblue", border="black")
# plot(st_geometry(goa_base$survey.strata), add = TRUE)
# plot(st_geometry(goa_base$akland), col = "tan", add = TRUE)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Write Geopackage
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
terra::writeVector(x = poly, 
                   filename = "C:/Users/zack.oyafuso/Desktop/catcam_areas.gpkg")

