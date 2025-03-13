# Create three nautical mile buffer zones around Steller sea lion rookeries for bottom trawl operations
# Sean Rohan
# Last update: March 13, 2025
#
# GAP establishes three nautical mile buffer zones around all Steller sea lion rookery locations 
# and treats the areas as no-transit zones. As of this update, buffer zones are only required for the
# western DPS, which is defined as the population west of 140 degrees west.

library(navmaps)
library(akgfmaps)
library(readxl)
library(here)

buffer_meters <- 3 * 1852
alaska_aea <- "EPSG:3338"  # Alaska Albers Equal Area projection for outputs

# MML rookeries 2025
mml_rookeries <- 
  here::here("analysis", "ssl_rookeries", "AK_SSL_sites_abbrev.xlsx") |>
  readxl::read_xlsx() |>
  dplyr::filter(ROOK == 1) |>
  sf::st_as_sf(crs = "WGS84",
               coords = c("LON", "LAT"))

# Create points and write to assets ----
mml_rookery_points <- 
  mml_rookeries |>
  sf::st_transform(crs = alaska_aea)

mml_rookery_points |>
  sf::st_write(
    dsn = here::here(
      "assets", 
      "data", 
      "SSLrookeries", 
      paste0(
        "ssl_rookeries_", 
        gsub(pattern = "-", replacement = "", x = Sys.Date()), 
        ".shp")
      ),
    append = FALSE
    )

# Create NTZ polygons and write to assets ---
mml_rookeries_buffered <- 
  mml_rookeries |>
  sf::st_buffer(dist = buffer_meters) |>
  sf::st_transform(crs = alaska_aea)

mml_rookeries_buffered |>
  sf::st_write(
    dsn = here::here(
      "assets", 
      "data", 
      "SSLrookeries", 
      paste0(
        "ssl_no_transit_3nm_", 
        gsub(pattern = "-", replacement = "", x = Sys.Date()), 
        ".shp")
    ),
    append = FALSE
  )

mml_rookeries_buffered |>
  dplyr::group_by(PARENTSITE) |>
  summarise() |>
  sf::st_write(
    dsn = here::here(
      "assets", 
      "data", 
      "SSLrookeries", 
      paste0(
        "ssl_no_transit_parent_3nm_", 
        gsub(pattern = "-", replacement = "", x = Sys.Date()), 
        ".shp")
    ),
    append = FALSE
  )
