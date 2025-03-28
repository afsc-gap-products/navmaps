# Example of creating layers for the EBS/NBS

# 1. Setup
library(navmaps)
region <- "nbs" # Options are sebs, nbs, ai, goa

# 2. Load shapefiles using the akgfmaps package
map_layers <- akgfmaps::get_base_layers(select.region = region, split.land.at.180 = FALSE)
channel <- get_connected(schema = "AFSC_32")

saveRDS(object = map_layers, file = here::here("assets", "data", paste0(region, "_map_layers.rds")))

# 3. Get data
get_gps_data(region = region, channel = channel)

software_types <- c("globe", "timezero", "opencpn") 

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
  
  # Survey grid without trawlable/untrawlable (EBS/NBS)
  survey_grid <- map_layers$survey.grid |>
    sf::st_make_valid() |> 
    sf::st_cast(to = "MULTILINESTRING") |> 
    sf::st_cast(to = "LINESTRING") |> 
    sf::st_cast("POLYGON")
  
  survey_grid$color <- navmaps_pal(values = "cyan", software_format = SOFTWARE, file_type = FILE_TYPE_POLYGON)
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
  grid_centers <- map_layers$survey.grid |>
    sf::st_make_valid() |> 
    sf::st_cast("POLYGON", group_or_split = TRUE) |>
    sf::st_centroid()
  
  spectacled_eider_sampling <- 
    read.csv(file = here::here("assets", "data", "special_projects", "NBS", "2025", "benthic_sampling_stations.csv")) |>
    dplyr::mutate(Priority = ifelse(Priority == "high", "Hi", "Md")) |>
    dplyr::mutate(STATION = Station.ID, station_label = paste0("Index + Grab (", Priority, ")")) |>
    dplyr::select(-Station.ID, -Priority)
  
  # Add spetacled eider sampling stations to centroids
  grid_centers <- dplyr::full_join(grid_centers, spectacled_eider_sampling) |>
    dplyr::mutate(station_label = ifelse(is.na(station_label), "Index Station", station_label))
  
  grid_centers$color <- "yellow"
  grid_centers$color[grid_centers$station_label == "Index + Grab (Hi)"] <- "magenta"
  grid_centers$color[grid_centers$station_label == "Index + Grab (Md)"] <- "cyan"
  
  grid_centers$shape <- navmaps_sym_pal(values = "circle1", 
                                        software_format = SOFTWARE, 
                                        file_type = FILE_TYPE_POINT, 
                                        color = grid_centers$color)
  
  grid_centers$color <- navmaps_pal(values = grid_centers$color, 
                                    software_format = SOFTWARE, 
                                    file_type = FILE_TYPE_POINT)
  
  sf_to_nav_file(
    x = grid_centers,
    file = here::here("output", region, "navigation", SOFTWARE, paste0(region, "_marks.", FILE_TYPE_POINT)),
    name_col = "STATION",
    description_col = "STATION",
    color_col = "color",
    shape_col = "shape",
    software_format = SOFTWARE
  )
  
  # 8. Survey stratum layer
  strata <- map_layers$survey.strata
  strata$color <- navmaps_pal(values = "yellow", 
                              software_format = SOFTWARE, 
                              file_type = FILE_TYPE_POLYGON)
  strata$fill <- 0
  strata$name <- paste0("Stratum ", strata$Stratum)
  
  sf_to_nav_file(
    x = strata,
    file = here::here("output", region, "navigation", SOFTWARE, paste0(region, "_survey_strata.", FILE_TYPE_POLYGON)), 
    name_col = "name",
    description_col = "STRATUM",
    color_col = "color",
    fill_col = "fill",
    software_format = SOFTWARE
  )
  
  # 9. Spectacled Eider Critical Habitat
  
  eider <- sf::st_read(here::here("assets", "data", "spectacled_eider", "FCH_Somateria_fischeri_20010206.shp")) |>
    dplyr::filter(Unit_ID == "3 - Norton Sound")
  
  sf::st_write(eider, here::here("output", region, "shapefiles", "SE_Only_Norton_Sound.shp"), append = FALSE)
  
  eider$color <- navmaps_pal(values = "red", 
                             software_format = SOFTWARE, 
                             file_type = FILE_TYPE_POLYGON)
  eider$fill <- 0
  eider$name <- "Spectacled Eider Critical Habitat"
  
  sf_to_nav_file(
    x = eider,
    file = here::here("output", region, "navigation", SOFTWARE, paste0("spectacled_eider_ch.", FILE_TYPE_LINESTRING)),
    name_col = "name",
    description_col = "Unit_ID",
    color_col = "color",
    fill_col = "fill",
    software_format = SOFTWARE
  )
  
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
