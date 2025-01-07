# Layers for the AI

# 1. Setup
library(navmaps)
region <- "ai" # Options are sebs, nbs, ai, goa


# 2. Load shapefiles using the akgfmaps package
map_layers <- akgfmaps::get_base_layers(select.region = region, split.land.at.180 = FALSE)
channel <- get_connected(schema = "AFSC")

# 3. Get data
get_gps_data(region = region, channel = channel)

software_types <- c("globe", "opencpn", "timezero") 

for(ii in 1:length(software_types)) {
  
  set_software(software_types[ii])
  
  4. Historical towpath, tow start, and midpoint
  make_towpaths(
    region = region,
    overwrite_midpoint = FALSE,
    software_format = SOFTWARE
  )
  
  # 5. Station grid 
    a. With trawlable/untrawlable (AI/GOA)
  make_trawlable(
    region = region,
    channel = channel,
    software_format = SOFTWARE
  )

  #   b. Without trawlable/untrawlable (EBS/NBS)
  survey_grid <- map_layers$survey.grid
  survey_grid$color <- navmaps_pal(values = "tan", software_format = SOFTWARE, file_type = FILE_TYPE_POLYGON)
  survey_grid$fill <- 0

  sf_to_nav_file(
    x = survey_grid,
    geometry = "LINESTRING",
    file = here::here("output", region, "navigation", SOFTWARE, paste0(region, "_station_grid.", FILE_TYPE_POLYGON)),
    name_col = "ID",
    description_col = "STRATUM",
    color_col = "color",
    software_format = SOFTWARE
  )
  
  # 6. Station marks
  grid_centers <- sf::st_centroid(map_layers$survey.grid)# Points at the center of each grid cell
  grid_centers$shape <- navmaps_sym_pal(values = "circle1", software_format = SOFTWARE, file_type = FILE_TYPE_POINT, color = "yellow")
  grid_centers$color <- navmaps_pal(values = "yellow", software_format = SOFTWARE, file_type = FILE_TYPE_POINT)

  sf_to_nav_file(
    x = grid_centers,
    file = here::here("output", region, "navigation", SOFTWARE, paste0(region, "_marks.", FILE_TYPE_POINT)),
    name_col = "ID",
    description_col = "ID",
    color_col = "color",
    shape_col = "shape",
    software_format = SOFTWARE
  )
  
  # 7. Station allocation
  allocation_df <- readxl::read_xlsx(here::here("assets", "data", "allocation", "ai_2024_station_allocation_320stn.xlsx")) |>
    tidyr::drop_na(LONGITUDE, LATITUDE, VESSEL) |>
    dplyr::mutate(VESSEL = factor(VESSEL)) |>
    as.data.frame()

  allocation_df$LONGITUDE
  allocation_df$LATITUDE

  make_station_allocation(
    allocation_df = allocation_df,
    lon_col = "LONGITUDE",
    lat_col = "LATITUDE",
    region = region,
    station_col = "STATIONID",
    stratum_col = "STRATUM",
    vessel_col = "VESSEL",
    extra_cols = "STATION_TYPE",
    vessel_colors = c("176" = "yellow", "148" = "cyan"),
    vessel_symbols = c("176" = "triangle1", "148" = "square1"),
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
  ssl <- sf::st_read(here::here("assets","data", "SSLrookeries", "3nm_No_Transit.shp"))
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
  humpback <- sf::st_read(here::here("assets","humpback", "WhaleHumpback_WesternNorthPacificDPS_20210421.shp")) |>
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


  
  
  # 13. Buoys
  buoys <- read.csv(file = here::here("assets", "data", "buoys", "Buoys_2024.csv")) |>
    dplyr::mutate(LONGITUDE = dms_string_to_dd(POSITION)[,1],
                  LATITUDE = dms_string_to_dd(POSITION)[,2]) |>
    sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = "EPSG:4326")

  # ggplot() +
  # geom_sf(data=buoys) +
  # geom_sf_text(data=buoys, mapping=aes(label=`TYPE.NAME`))

  buoys$shape <- navmaps_sym_pal(values = "warning",
                                 software_format = SOFTWARE,
                                 file_type = FILE_TYPE_POINT)
  buoys$color <- navmaps_pal(values = "darkorange",
                             software_format = SOFTWARE,
                             file_type = FILE_TYPE_POINT)
  buoys$description <- paste0("Top float: ", buoys$`TOP FLOAT DEPTH`, "; Depth: ", buoys$`WATER DEPTH`)

  sf::st_write(buoys, dsn = here::here("output", region,  "shapefiles", paste0("buoys_2024.shp")))

  sf_to_nav_file(x = buoys,
                 file = here::here("output", region,  "navigation", SOFTWARE, paste0("buoys_2024.", FILE_TYPE_POINT)),
                 name_col = "TYPE.NAME",
                 description_col = "description",
                 color_col = "color",
                 shape_col = "shape",
                 software_format = SOFTWARE)
  # 
  # 14. Crab pot storage (requires 32-bit R to open .mdb)
  
  # Add an entry for every crab pot storage data set

  crabpots <- sf::st_read(dsn = here::here("assets", "data", "crabpots", "2024", "edited_ED_crabpot_lines_2024.shp")) |>
    dplyr::bind_rows(
    sf::st_read(dsn = here::here("assets", "data", "crabpots", "2024", "edited_ai_AT_crabpot_lines_2024_full.shp")) |> 
      dplyr::mutate(id = as.character(id))  |>
      sf::st_cast(to = "MULTILINESTRING") |>
      sf::st_cast(to = "LINESTRING")) |>
    dplyr::bind_rows(sf::st_read(dsn = here::here("assets", "data", "crabpots", "2024", "edited_EN_crabpot_lines_2024.shp"))
          ) |>
    dplyr::bind_rows(sf::st_read(dsn = here::here("assets", "data", "crabpots", "2024", "edited_A1_crabpot_lines_2024.shp"))
                     ) |>
    dplyr::select(-length, -INDEX)
  
  crabpots$description <- "Crab pot storage"
  crabpots$color <- navmaps::navmaps_pal(values = "darkorange", 
                                         software_format = SOFTWARE, 
                                         file_type = FILE_TYPE_LINESTRING)
  
  sf::st_transform(crabpots, crs = "EPSG:3338") |>
  sf::st_write(crabpots, here::here("output", region, "shapefiles", "crabpot_lines_2024.shp"),
               append = FALSE)

  sf_to_nav_file(x = crabpots,
                 file = here::here("output", region, "navigation", SOFTWARE,  paste0("crabpot_lines_2024.", FILE_TYPE_LINESTRING)),
                 name_col = "vessel",
                 description_col = "description",
                 color_col = "color",
                 software_format = SOFTWARE)
  
  crabpot_marks <- dplyr::bind_rows(
    lwgeom::st_startpoint(crabpots) |>
      sf::st_as_sf() |>
      dplyr::rename(geometry = x) |>
      dplyr::mutate(vessel = crabpots$vessel),
    lwgeom::st_endpoint(crabpots) |>
      sf::st_as_sf() |>
      dplyr::rename(geometry = x) |>
      dplyr::mutate(vessel = crabpots$vessel)
  ) |>
    dplyr::mutate(color = navmaps::navmaps_pal(values = "darkorange", 
                                               software_format = SOFTWARE, 
                                               file_type = FILE_TYPE_POINT), 
                  shape = navmaps::navmaps_sym_pal("asterisk", 
                                                   software_format = SOFTWARE, 
                                                   file_type = FILE_TYPE_POINT), 
                  description = "Crab pot storage")
  
  sf_to_nav_file(x = crabpot_marks,
                 file = here::here("output", region, "navigation", SOFTWARE, paste0("crabpot_marks_2024.", FILE_TYPE_POINT)),
                 name_col = "vessel",
                 description_col = "description",
                 shape_col = "shape",
                 color_col = "color",
                 software_format = SOFTWARE)
  
  sf::st_transform(crabpot_marks, crs = "EPSG:3338") |>
    sf::st_write(here::here("output", region, "shapefiles", "crabpot_marks_2024.shp"),
                 append = FALSE)
  

  # 15. Special projects
  
}

