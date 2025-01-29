# Layers for the GOA

# 1. Setup
library(navmaps)
region <- "goa" # Options are sebs, nbs, ai, goa

# 2. Load shapefiles using the akgfmaps package
map_layers <- akgfmaps::get_base_layers(select.region = region, split.land.at.180 = FALSE)

saveRDS(object = map_layers, file = here::here("assets", "data", paste0(region, "_map_layers.rds")))

channel <- get_connected(schema = "AFSC")

# 3. Get data
get_gps_data(region = region, channel = channel)

software_types <- c("timezero", "opencpn", "globe") 

for(ii in 1:length(software_types)) {
  
  set_software(software_types[ii])
  
  if(SOFTWARE == "globe") {
    map_layers <- readRDS(here::here("assets", "data", paste0(region, "_map_layers.rds")))
  }
  
  # 4. Historical towpath, tow start, and midpoint
  make_towpaths(
    region = region,
    overwrite_midpoint = ifelse(ii == 0, TRUE, FALSE),
    software_format = SOFTWARE
  )

  # 5. Trawlable/untrawlable station grid (lines) and marks
  trawlable_grid <- sf::st_read(here::here("assets", "data", "allocation", "goa_stations_2025.gpkg")) |>
    sf::st_transform(crs = "WGS84") |>
    sf::st_set_geometry("geometry") |>
    sf::st_wrap_dateline()

  trawlable_grid$fill <- 0

  trawlable_grid$color <- navmaps_pal(values = "cyan",
                                      software_format = SOFTWARE,
                                      file_type = FILE_TYPE_POLYGON)
  trawlable_grid$color[trawlable_grid$TRAWLABLE == 'Y'] <- navmaps_pal(values = "lightgreen",
                                                                       software_format = SOFTWARE,
                                                                       file_type = FILE_TYPE_POLYGON)
  trawlable_grid$color[trawlable_grid$TRAWLABLE == 'N'] <- navmaps_pal(values = "red",
                                                                       software_format = SOFTWARE,
                                                                       file_type = FILE_TYPE_POLYGON)

  trawlable_grid$description <- paste0("Trawlable: ", trawlable_grid$TRAWLABLE, "; Stratum: ", trawlable_grid$STRATUM)

  sf_to_nav_file(x = trawlable_grid,
                 file = here::here("output", region, "navigation", SOFTWARE, paste0(region, "_trawlwable_grid.", FILE_TYPE_POLYGON)),
                 name_col = "STATION",
                 description_col = "description",
                 color_col = "color",
                 fill_col = "fill",
                 software_format = SOFTWARE)

  trawlable_centroid <- sf::st_centroid(trawlable_grid)

  trawlable_centroid$color <- navmaps_pal(values = "cyan",
                                      software_format = SOFTWARE,
                                      file_type = FILE_TYPE_POINT)
  trawlable_centroid$color[trawlable_grid$TRAWLABLE == 'Y'] <- navmaps_pal(values = "lightgreen",
                                                                       software_format = SOFTWARE,
                                                                       file_type = FILE_TYPE_POINT)
  trawlable_centroid$color[trawlable_grid$TRAWLABLE == 'N'] <- navmaps_pal(values = "red",
                                                                       software_format = SOFTWARE,
                                                                       file_type = FILE_TYPE_POINT)

  trawlable_centroid$shape <- navmaps_sym_pal(values = "circle1",
                                              software_format = SOFTWARE,
                                              file_type = FILE_TYPE_POINT)

  sf_to_nav_file(x = trawlable_centroid,
                 file = here::here("output", region, "navigation", SOFTWARE, paste0(region, "_trawlwable_mark.", FILE_TYPE_POINT)),
                 name_col = "STATION",
                 description_col = "description",
                 color_col = "color",
                 shape_col = "shape",
                 software_format = SOFTWARE)



  # 6. Station marks
  grid_centers <- sf::st_centroid(map_layers$survey.grid)# Points at the center of each grid cell
  grid_centers$shape <- navmaps_sym_pal(values = "circle1",
                                        software_format = SOFTWARE,
                                        file_type = FILE_TYPE_POINT,
                                        color = "cyan")
  grid_centers$color <- navmaps_pal(values = "cyan", software_format = SOFTWARE, file_type = FILE_TYPE_POINT)

  sf_to_nav_file(
    x = grid_centers,
    file = here::here("output", region, "navigation", SOFTWARE, paste0(region, "_marks.", FILE_TYPE_POINT)),
    name_col = "STATION",
    description_col = "STATION",
    color_col = "color",
    shape_col = "shape",
    software_format = SOFTWARE
  )

  # 7. Station allocation (AI/GOA/slope only)
  allocation_df <- sf::st_read(here::here("assets", "data", "allocation", "goa_2025_station_allocation_520_aea.gpkg")) |>
    sf::st_drop_geometry()

  make_station_allocation(
    allocation_df = allocation_df,
    lon_col = "LONGITUDE",
    lat_col = "LATITUDE",
    region = region,
    station_col = "STATION",
    stratum_col = "STRATUM",
    vessel_col = "VESSEL",
    vessel_colors = c("176" = "yellow", "148" = "cyan"),
    vessel_symbols = c("176" = "triangle1", "148" = "circle1"),
    software_format = SOFTWARE
  )

  # 8. Survey stratum layer
  strata <- map_layers$survey.strata
  strata$color <- navmaps_pal(values = "yellow", software_format = SOFTWARE, file_type = FILE_TYPE_POLYGON)
  strata$fill <- 0
  strata$name <- paste0("Stratum ", strata$STRATUM)

  sf_to_nav_file(
    x = strata,
    file = here::here("output", region, "navigation", SOFTWARE, paste0(region, "_survey_strata.", FILE_TYPE_POLYGON)),
    name_col = "name",
    description_col = "STRATUM",
    color_col = "color",
    fill_col = "fill",
    software_format = SOFTWARE
  )

  # 9. SSL buffer zones
  ssl <- sf::st_read(here::here("assets", "data", "SSLrookeries", "3nm_No_Transit.shp"))
  ssl$color <- navmaps_pal(values = "red", software_format = SOFTWARE, file_type = FILE_TYPE_POLYGON)
  ssl$fill <- navmaps_pal(values = "red", software_format = SOFTWARE, file_type = FILE_TYPE_POLYGON)

  sf_to_nav_file(
    x = ssl,
    file = here::here("output", region, "navigation", SOFTWARE, paste0(region, "_ssl_no_transit.", FILE_TYPE_POLYGON)),
    name_col = "Name",
    description_col = "Name",
    color_col = "color",
    fill_col = "fill",
    software_format = SOFTWARE
  )

  # 10. Sea Otter Critical Habitat
  otters <- sf::st_read(here::here("assets", "data", "otters", "SeaOtterFinalCH_Project.shp")) |>
    sf::st_make_valid()

  otters$name <- "Otter Habitat"
  otters$color <- navmaps_pal(values = "red", software_format = SOFTWARE, file_type = FILE_TYPE_POLYGON)
  otters$fill <- 0

  sf_to_nav_file(x = otters,
                 file = here::here("output", region, "navigation", SOFTWARE, paste0("otter_habitat.", FILE_TYPE_POLYGON)),
                 name_col = "name",
                 description_col = "CH_Unit",
                 color_col = "color",
                 fill_col = "fill",
                 software_format = SOFTWARE)

  # 11. North Pacific Right Whale Critical Habitat
  nprw <- sf::st_read(here::here("assets", "data", "NPRW", "NPRWCH.shp")) |>
    sf::st_transform(crs = "EPSG:4326")
  nprw$name <- "NPRW Critical Habitat"
  nprw$description <- "NPRW Critical Habitat"
  nprw$color <- navmaps_pal(values = "red", software_format = SOFTWARE, file_type = FILE_TYPE_POLYGON)
  nprw$fill <- 0

  sf_to_nav_file(x = nprw,
                 file = here::here("output", region, "navigation", SOFTWARE, paste0("NPRW_Critical_Habitat.", FILE_TYPE_POLYGON)),
                 name_col = "name",
                 description_col = "description",
                 color_col = "color",
                 fill_col = "fill",
                 software_format = SOFTWARE)

  # 12. Humpback Whale Critical Habitat
  humpback <- sf::st_read(here::here("assets", "data", "humpback", "WhaleHumpback_WesternNorthPacificDPS_20210421.shp")) |>
    sf::st_transform(crs = "EPSG:4326")
  humpback$name <- "Humpback Critical Habitat"
  humpback$description <- "Humpback Critical Habitat"
  humpback$color <- navmaps_pal(values = "red", software_format = SOFTWARE, file_type = FILE_TYPE_POLYGON)
  humpback$fill <- 0

  sf_to_nav_file(x = humpback,
                 file = here::here("output", region, "navigation", SOFTWARE, paste0("Humpback_Critical_Habitat.", FILE_TYPE_POLYGON)),
                 name_col = "name",
                 description_col = "description",
                 color_col = "color",
                 fill_col = "fill",
                 software_format = SOFTWARE)

  # 13 Cook Inlet Beluga Whale Critical Habitat

  beluga <- sf::st_read(here::here("assets", "data", "cibeluga", "cib_critical_habitat.shp")) |>
    sf::st_transform(crs = "EPSG:4326")
  beluga$name <- "Beluga Critical Habitat"
  beluga$description <- "Beluga Critical Habitat"
  beluga$color <- navmaps_pal(values = "red", software_format = SOFTWARE, file_type = FILE_TYPE_POLYGON)
  beluga$fill <- 0

  sf_to_nav_file(x = beluga,
                 file = here::here("output", region, "navigation", SOFTWARE, paste0("CIBeluga_Critical_Habitat.", FILE_TYPE_POLYGON)),
                 name_col = "name",
                 description_col = "description",
                 color_col = "color",
                 fill_col = "fill",
                 software_format = SOFTWARE)
  
  # 14. Navigation hazards (moorings, shipwrecks, etc.)
  buoys <- sf::st_read(dsn = here::here("assets", "data", "buoys", "hazNav_1.geojson")) |>
    dplyr::filter(ATU == 17)
  
  buoys$shape <- navmaps_sym_pal(values = "warning",
                                 software_format = SOFTWARE,
                                 file_type = FILE_TYPE_POINT)
  buoys$color <- navmaps_pal(values = "darkorange",
                             software_format = SOFTWARE,
                             file_type = FILE_TYPE_POINT)
  
  sf_to_nav_file(x = buoys,
                 file = here::here("output", region, "navigation", SOFTWARE, paste0("hazards_buoys_20250121.", FILE_TYPE_POINT)),
                 name_col = "SUB_CATEGORY",
                 description_col = "DESCRIPTION",
                 color_col = "color",
                 shape_col = "shape",
                 software_format = SOFTWARE)
  
  buoys |>
    sf::st_transform(crs = "EPSG:3338") |>
    sf::st_write(delete_dsn = TRUE, here::here("output", region, "shapefiles", "hazards_buoys_20250121.gpkg"))
  
  # 15. Crab pot storage (requires 32-bit R to open .mdb)
  
  # Add an entry for every crab pot storage data set
  
  # crabpots <- sf::st_read("G:/GOA/GOA 2023/ArcMap/GIS/GOA_2023/Crab Pot Storage/ErlaN_Poly.shp") |>
  #   sf::st_transform(crs = "EPSG:4326")
  # 
  # crabpots$description <- "Crab pot storage"
  # crabpots$id <- "Pot storage"
  # crabpots$color <- navmaps_pal(values = "red", software_format = SOFTWARE, file_type = FILE_TYPE_POINT)
  # 
  # sf_to_nav_file(x = crabpots,
  #                file = here::here("output", region, "navigation", SOFTWARE, paste0("crabpots_2023.", FILE_TYPE_LINESTRING)),
  #                name_col = "id",
  #                description_col = "description",
  #                color_col = "color",
  #                software_format = SOFTWARE)
  
  # 16. Canadian border claim at Dixon Entrance
  dixon_entrance <- sf::st_read(here::here("assets", "data", "dixon_entrance", "canadaborder.shp"),
                                crs = "EPSG:3338") |>
    sf::st_transform(crs = "WGS84")
  
  dixon_entrance$description <- "Dixon Entrance disputed border"
  dixon_entrance$name <- "Dixon Entrance disputed border"
  dixon_entrance$fill <- 0
  dixon_entrance$color <- navmaps_pal(values = "darkorange", 
                             software_format = SOFTWARE, 
                             file_type = FILE_TYPE_POLYGON)
  
  sf_to_nav_file(x = dixon_entrance,
                 file = here::here("output", region, "navigation", SOFTWARE, paste0("disputed_border.", FILE_TYPE_POLYGON)),
                 name_col = "name",
                 description_col = "description",
                 color_col = "color",
                 fill_col = "fill",
                 software_format = SOFTWARE)
  
}

file.copy(from = here::here("output", region, "navigation"),
          to = paste0("G:/RACE_CHARTS/", region),
          recursive = TRUE)

file.copy(from = here::here("output", region, "shapefiles"),
          to = paste0("G:/RACE_CHARTS/", region),
          recursive = TRUE)

file.copy(from = here::here("output", region, "gps"),
          to = paste0("G:/RACE_CHARTS/", region),
          recursive = TRUE)
