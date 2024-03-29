# Layers for the AI

# 1. Setup
library(navmaps)
region <- "ai" # Options are sebs, nbs, ai, goa


# 2. Load shapefiles using the akgfmaps package
map_layers <- akgfmaps::get_base_layers(select.region = region)
channel <- get_connected(schema = "AFSC")

# 3. Get data
get_gps_data(region = region, channel = channel)

software_types <- c("globe", "opencpn", "timezero") 

for(ii in 1:length(software_types)) {
  
  set_software(software_types[ii])
  
  # 4. Historical towpath, tow start, and midpoint
  make_towpaths(
    region = region, 
    overwrite_midpoint = FALSE, 
    software_format = SOFTWARE
  )
  
  # 5. Station grid 
  #   a. With trawlable/untrawlable (AI/GOA)
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
  
  # 7. Station allocation (AI/GOA/slope only)
  read.csv(here::here("data", "allocation", "AIallocation420.csv")) |> # Replace with path to station allocation file for the survey
    dplyr::select(-Symbol, -Color) |>
    tidyr::drop_na(Longitude, Latitude) |>
    make_station_allocation(
      lon_col = "Longitude",
      lat_col = "Latitude",
      region = region,
      station_col = "stationid",
      stratum_col = "stratum",
      vessel_col = "vessel",
      extra_cols = c("Priority", "stationid", "stratum"),
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
  ssl <- sf::st_read(here::here("data", "SSLrookeries", "3nm_No_Transit.shp"))
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
  otters <- sf::st_read(here::here("data", "otters", "SeaOtterFinalCH_Project.shp")) |>
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
  nprw <- sf::st_read(here::here("data", "NPRW", "NPRWCH.shp"))
  nprw$name <- "NPRW Critical Habitat"
  nprw$description <- "NPRW Critical Habitat"
  nprw$color <- navmaps_pal(values = "darkorange", software_format = SOFTWARE, file_type = FILE_TYPE_POINT)
  nprw$fill <- 0
  
  sf_to_nav_file(x = nprw,
                 file = here::here("output", region, "navigation", SOFTWARE, paste0("NPRW_Critical_Habitat.", FILE_TYPE_POLYGON)),
                 name_col = "name",
                 description_col = "description",
                 color_col = "color",
                 fill_col = "fill",
                 software_format = SOFTWARE)
  
  # 12. Buoys
  buoys <- readxl::read_xlsx(path = here::here("data", "buoys", "Buoys_2023_11_21.xlsx")) |>
    dplyr::mutate(LONGITUDE = dms_string_to_dd(POSITION)[,1],
                  LATITUDE = dms_string_to_dd(POSITION)[,2]) |>
    sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = "EPSG:4326")
  
  ggplot() + 
    geom_sf(data=buoys) #+
  # geom_sf_text(data=buoys, mapping=aes(label=`TYPE/NAME`))
  buoys$shape <- navmaps_sym_pal(values = "warning",
                                 software_format = SOFTWARE,
                                 file_type = FILE_TYPE_POINT)
  buoys$color <- navmaps_pal(values = "darkorange",
                             software_format = SOFTWARE,
                             file_type = FILE_TYPE_POINT)
  buoys$description <- paste0("Top float: ", buoys$`TOP FLOAT DEPTH`, "; Depth: ", buoys$`WATER DEPTH`)
  
  sf::st_write(buoys, dsn = here::here("output", region,  "shapefiles", paste0("buoys_2023_11_21.shp")))
  
  sf_to_nav_file(x = buoys,
                 file = here::here("output", region,  "navigation", SOFTWARE, paste0("buoys_2023_11_21.", FILE_TYPE_POINT)),
                 name_col = "TYPE/NAME",
                 description_col = "description",
                 color_col = "color",
                 shape_col = "shape",
                 software_format = SOFTWARE)
  
  # 13. Crab pot storage (requires 32-bit R to open .mdb)
  
  # Add an entry for every crab pot storage data set
  crabpots <- dplyr::bind_rows(
    globe_to_sf(dsn = here::here("data", "crabpots", "crabpots_AKTrojan_2022.mdb"),
                grouping_col = "DateTime"),
    globe_to_sf(dsn = here::here("data", "crabpots", "crabpots_EarlyDawnEast_2022.mdb"),
                grouping_col = NULL)
  )
  
  crabpots$description <- "Crab pot storage"
  crabpots$color <- navmaps_pal(values = "darkorange", software_format = SOFTWARE, file_type = FILE_TYPE_POINT)
  
  sf_to_nav_file(x = crabpots,
                 file = here::here("output", region, "navigation", paste0("crabpots_2022.", FILE_TYPE_LINESTRING)),
                 name_col = "id",
                 description_col = "description",
                 color_col = "color",
                 software_format = SOFTWARE)
  
  # 14. Special projects
  
}