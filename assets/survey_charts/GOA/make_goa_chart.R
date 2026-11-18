library(navmaps)

# GOA Chart

crs_aea <- 3338

map_layers <- akgfmaps::get_base_layers(select.region = "goa", set.crs = crs_aea)

read_transform <- function(fpath, crs = 3338) {
  
  sf::st_read(fpath) |> |>
    sf::st_transform(crs)
  
}

hazards <- read_transform(here::here("output", "goa", "shapefiles", "hazards_buoys_20250121.gpkg"))

hazards <- sf::st_read(here::here("output", "goa", "shapefiles", "hazards_buoys_20250121.gpkg")) |>
  sf::st_transform(crs = "EPSG:3338")

allocation <- sf::st_read(here::here("output", "goa", "shapefiles", "goa_station_allocation.shp"))

towpath <- sf::st_read(here::here("output", "goa", "shapefiles", "goa_towpath.shp")) # color by performance

enc_wrecks <- sf::st_read(here::here("assets", "data", "wrecks", "enc_wrecks.shp"))

awois_wrecks <- sf::st_read(here::here("assets", "data", "wrecks", "awois_wrecks.shp"))

awois_obstructions <- sf::st_read(here::here("assets", "data", "wrecks", "awois_obstructions.shp"))

island_names <- sf::st_read(here::here("assets", "data", "placenames", "GOA_place_names_islands_large.shp"))

regions <- sf::st_read(here::here("assets", "data", "region_boundaries", "nmfs_boundaries.gpkg"))

trawlable_grid <- sf::st_read(here::here("output", "goa", "shapefiles", "goa_trawlable_grid.shp"))

habitat_nprw <- sf::st_read(here::here("assets", "data", "NPRW", "nprwch.shp"))

habitat_sea_otter <- sf::st_read(here::here("assets", "data", "otters", "SeaOtterFinalCH_Project.shp"))

habitat_humpback <- sf::st_read(here::here("assets", "data", "humpback", "WhaleHumpback_WesternNorthPacificDPS_20210421.shp"))

ssl_notransit <- sf::st_read(here::here("assets", "data", "SSLrookeries", "3nm_notransit.shp"))

akland <- map_layers$akland

darpa <- sf::st_read(here::here("assets", "data", "rockets", "DARPADownRange.shp"))

plot(test)
