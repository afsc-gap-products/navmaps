# Layers for the AI

# 1. Setup
library(navmaps)
region <- "ai" # Options are sebs, nbs, ai, goa

# 2. Load shapefiles using the akgfmaps package
map_layers <- akgfmaps::get_base_layers(select.region = region, split.land.at.180 = FALSE)

# Save AI survey grid and stratum shapefiles
sf::st_write(
  obj = sf::st_transform(map_layers$survey.grid, crs = "EPSG:3338"), 
  dsn = here::here("output", region, "shapefiles", paste0(region, "_station_grid.shp")),
  append = FALSE
)

sf::st_write(
  obj = sf::st_transform(map_layers$survey.strata, crs = "EPSG:3338"), 
  dsn = here::here("output", region, "shapefiles", paste0(region, "_survey_strata.shp")),
  append = FALSE
)

# channel <- get_connected(schema = "AFSC")
channel <- get_connected(schema = "AFSC_32") # 32 bit for Globe files

# 3. Get data
get_gps_data(region = region, channel = channel)

software_types <- c(
  "globe",
  "opencpn",
  "timezero"
  ) 

# Download latest USCG hazNav GeoJSON files (moorings, buoys)
download.file(
  url = "https://www.navcen.uscg.gov/sites/default/files/msi/hazNav_1.geojson",
  destfile = here::here("assets", "data", "buoys", "hazNav_1.geojson")
  )
download.file(
  url = "https://www.navcen.uscg.gov/sites/default/files/msi/hazNavLine_1.geojson",
  destfile = here::here("assets", "data", "buoys", "hazNavLine_1.geojson")
  )
download.file(
  url = "https://www.navcen.uscg.gov/sites/default/files/msi/hazNavPoly_1.geojson",
  destfile = here::here("assets", "data", "buoys", "hazNavPoly_1.geojson")
  )


for(ii in 1:length(software_types)) {
  
  set_software(software_types[ii])
  
  # 4. Historical towpath, tow start, and midpoint
  make_towpaths(
    region = region,
    overwrite_midpoint = FALSE,
    software_format = SOFTWARE
  )
  
  # 5. Station grid 
    # a. With trawlable/untrawlable (AI/GOA)
  make_trawlable(
    region = region,
    channel = channel,
    software_format = SOFTWARE
  )
  
  # b. Full grid
  survey_grid <- map_layers$survey.grid

  survey_grid$color <- navmaps_pal(values = "tan", software_format = SOFTWARE, file_type = FILE_TYPE_POLYGON)
  survey_grid$fill <- 0
  

  
  sf_to_nav_file(
    x = survey_grid,
    geometry = "LINESTRING",
    file = here::here("output", region, "navigation", SOFTWARE, paste0(region, "_station_grid.", FILE_TYPE_POLYGON)),
    name_col = "STATION",
    description_col = "STATION",
    color_col = "color",
    software_format = SOFTWARE
  )
  
  
  # 6. Station marks
  grid_centers <- navmaps::st_primary_centroid(map_layers$survey.grid) # Points at the center of each grid cell
  grid_centers$shape <- navmaps_sym_pal(values = "circle1", software_format = SOFTWARE, file_type = FILE_TYPE_POINT, color = "yellow")
  grid_centers$color <- navmaps_pal(values = "yellow", software_format = SOFTWARE, file_type = FILE_TYPE_POINT)

  sf_to_nav_file(
    x = grid_centers,
    file = here::here("output", region, "navigation", SOFTWARE, paste0(region, "_marks.", FILE_TYPE_POINT)),
    name_col = "STATION",
    description_col = "STRATUM",
    color_col = "color",
    shape_col = "shape",
    software_format = SOFTWARE
  )
  
  # 7. Station allocation
  allocation <- sf::st_read(here::here("assets", "data", "allocation", "ai_2026_station_allocation_400stn.gpkg")) |>
    tidyr::drop_na(LONGITUDE, LATITUDE, VESSEL) |>
    dplyr::mutate(VESSEL = factor(VESSEL)) |>
    sf::st_transform(crs = "WGS84")
  
  allocation[c("LONGITUDE", "LATITUDE")] <- sf::st_coordinates(allocation)
  
  allocation <- sf::st_drop_geometry(allocation) |> as.data.frame()
  
  make_station_allocation(
    allocation_df = allocation,
    lon_col = "LONGITUDE",
    lat_col = "LATITUDE",
    region = region,
    station_col = "STATION",
    stratum_col = "STRATUM",
    vessel_col = "VESSEL",
    # extra_cols = "STATION_TYPE",
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
  
  # # 9. SSL buffer zones
  ssl <- sf::st_read(here::here("assets", "data", "SSLrookeries", "3nm_notransit.shp"))
  ssl$NAME <- "SSL No-Transit"
  ssl$color <- navmaps_pal(values = "red", software_format = SOFTWARE, file_type = FILE_TYPE_POLYGON)
  ssl$fill <- navmaps_pal(values = "red", software_format = SOFTWARE, file_type = FILE_TYPE_POLYGON)

  sf_to_nav_file(
    x = ssl,
    file = here::here("output", region, "navigation", SOFTWARE, paste0(region, "_ssl_no_transit.", FILE_TYPE_POLYGON)),
    name_col = "NAME",
    description_col = "NAME",
    color_col = "color",
    fill_col = "fill",
    software_format = SOFTWARE
  )
  
  # 10. Sea Otter Critical Habitat
  otters <- sf::st_read(here::here("assets","data", "otters", "SeaOtterFinalCH_Project.shp")) |>
    sf::st_make_valid() |> dplyr::mutate(CH_Unit=NA)

  otters$name <- "Otter Habitat"
  otters$color <- navmaps_pal(values = "red", software_format = SOFTWARE, file_type = FILE_TYPE_POLYGON)
  otters$fill <- 0

  sf_to_nav_file(x = otters,
                 file = here::here("output", region, "navigation",SOFTWARE, paste0("otter_habitat.", FILE_TYPE_POLYGON)),
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
  
  # 14. Navigation hazards (moorings, shipwrecks, etc.)
  # From USCG Local Notice to Mariners (Arctic/District 17)
  # https://www.navcen.uscg.gov/msi
  buoys <- sf::st_read(dsn = here::here("assets", "data", "buoys", "hazNav_1.geojson")) |>
    dplyr::filter(ATU == 17)

  buoys$shape <- navmaps_sym_pal(values = "warning",
                                 software_format = SOFTWARE,
                                 file_type = FILE_TYPE_POINT)
  buoys$color <- navmaps_pal(values = "darkorange",
                             software_format = SOFTWARE,
                             file_type = FILE_TYPE_POINT)

  sf_to_nav_file(x = buoys,
                 file = here::here("output", region, "navigation", SOFTWARE, paste0("hazards_buoys_", gsub("[^0-9]", "", Sys.Date()), ".", FILE_TYPE_POINT)),
                 name_col = "SUB_CATEGORY",
                 description_col = "DESCRIPTION",
                 color_col = "color",
                 shape_col = "shape",
                 software_format = SOFTWARE)

  ggplot() +
  geom_sf(data=buoys) +
  geom_sf_text(data=buoys, mapping=aes(label=TYPE))

  buoys$shape <- navmaps_sym_pal(values = "warning",
                                 software_format = SOFTWARE,
                                 file_type = FILE_TYPE_POINT)
  buoys$color <- navmaps_pal(values = "darkorange",
                             software_format = SOFTWARE,
                             file_type = FILE_TYPE_POINT)

  sf::st_write(dplyr::select(buoys, -COORDINATES_JSON), dsn = here::here("output", region,  "shapefiles", paste0("hazards_buoys_", gsub("[^0-9]", "", Sys.Date()), ".gpkg")), append=FALSE)

  sf_to_nav_file(
    x = buoys,
    file = here::here("output", region,  "navigation", SOFTWARE, paste0("hazards_buoys_", gsub("[^0-9]", "", Sys.Date()), ".", FILE_TYPE_POINT)),
    name_col = "TYPE",
    description_col = "DESCRIPTION",
    color_col = "color",
    shape_col = "shape",
    software_format = SOFTWARE
  )
  # 
  # 14. Crab pot storage (requires 32-bit R to open .mdb)
  
  # Add an entry for every crab pot storage data set

  # crabpots <- sf::st_read(dsn = here::here("assets", "data", "crabpots", "2024", "edited_ED_crabpot_lines_2024.shp")) |>
  #   dplyr::bind_rows(
  #   sf::st_read(dsn = here::here("assets", "data", "crabpots", "2024", "edited_ai_AT_crabpot_lines_2024_full.shp")) |> 
  #     dplyr::mutate(id = as.character(id))  |>
  #     sf::st_cast(to = "MULTILINESTRING") |>
  #     sf::st_cast(to = "LINESTRING")) |>
  #   dplyr::bind_rows(sf::st_read(dsn = here::here("assets", "data", "crabpots", "2024", "edited_EN_crabpot_lines_2024.shp"))
  #         ) |>
  #   dplyr::bind_rows(sf::st_read(dsn = here::here("assets", "data", "crabpots", "2024", "edited_A1_crabpot_lines_2024.shp"))
  #                    ) |>
  #   dplyr::select(-length, -INDEX)
  # 
  # crabpots$description <- "Crab pot storage"
  # crabpots$color <- navmaps::navmaps_pal(values = "darkorange", 
  #                                        software_format = SOFTWARE, 
  #                                        file_type = FILE_TYPE_LINESTRING)
  # 
  # sf::st_transform(crabpots, crs = "EPSG:3338") |>
  # sf::st_write(crabpots, here::here("output", region, "shapefiles", "crabpot_lines_2024.shp"),
  #              append = FALSE)
  # 
  # sf_to_nav_file(x = crabpots,
  #                file = here::here("output", region, "navigation", SOFTWARE,  paste0("crabpot_lines_2024.", FILE_TYPE_LINESTRING)),
  #                name_col = "vessel",
  #                description_col = "description",
  #                color_col = "color",
  #                software_format = SOFTWARE)
  # 
  # crabpot_marks <- dplyr::bind_rows(
  #   lwgeom::st_startpoint(crabpots) |>
  #     sf::st_as_sf() |>
  #     dplyr::rename(geometry = x) |>
  #     dplyr::mutate(vessel = crabpots$vessel),
  #   lwgeom::st_endpoint(crabpots) |>
  #     sf::st_as_sf() |>
  #     dplyr::rename(geometry = x) |>
  #     dplyr::mutate(vessel = crabpots$vessel)
  # ) |>
  #   dplyr::mutate(color = navmaps::navmaps_pal(values = "darkorange", 
  #                                              software_format = SOFTWARE, 
  #                                              file_type = FILE_TYPE_POINT), 
  #                 shape = navmaps::navmaps_sym_pal("asterisk", 
  #                                                  software_format = SOFTWARE, 
  #                                                  file_type = FILE_TYPE_POINT), 
  #                 description = "Crab pot storage")
  # 
  # sf_to_nav_file(x = crabpot_marks,
  #                file = here::here("output", region, "navigation", SOFTWARE, paste0("crabpot_marks_2024.", FILE_TYPE_POINT)),
  #                name_col = "vessel",
  #                description_col = "description",
  #                shape_col = "shape",
  #                color_col = "color",
  #                software_format = SOFTWARE)
  # 
  # sf::st_transform(crabpot_marks, crs = "EPSG:3338") |>
  #   sf::st_write(here::here("output", region, "shapefiles", "crabpot_marks_2024.shp"),
  #                append = FALSE)
  

  # 15. Special projects
  
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

file.copy(from = here::here("assets", "data", "SSLrookeries"),
          to = paste0("G:/RACE_CHARTS/", region, "/shapefiles/"),
          recursive = TRUE)

