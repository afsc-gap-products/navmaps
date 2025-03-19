# Create three nautical mile buffer zones around all Steller sea lion rookeries in Alaska
# Sean Rohan
# Last update: March 19, 2025
#
# From 2017-2023, GAP used a three nautical mile buffer zone around all Steller sea lion rookeries 
# in Alaska where at least 50 pups had been counted in a single year since 1970. As of this update, 
# 3 nm no-transit zones are required for western DPS rookeries (west of 140 W) listed under 
# 50 CFR 224.103.

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
