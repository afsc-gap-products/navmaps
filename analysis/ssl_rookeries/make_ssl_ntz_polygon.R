# Create Steller sea lion 3 nautical mile no-transit polygon for surveys
# Sean Rohan
# Last update: March 12, 2025
#
# AFSC trawl surveys aim to use the most up to date rookery locations from MML to designate
# no-transit zones for survey operations. This script uses the rookery location information
# available on the 'Last update' date to identify  rookery locations (points) and add a 3 nautical 
# mile buffer to produce no-transit zone polygons for the survey.

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
      )
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
    )
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
    )
  )
