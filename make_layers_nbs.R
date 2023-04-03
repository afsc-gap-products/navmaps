# Example of creating layers for the EBS/NBS

# 1. Setup
library(navmaps)
region <- "nbs" # Options are sebs, nbs, ai, goa

# 2. Load shapefiles using the akgfmaps package
map_layers <- akgfmaps::get_base_layers(select.region = region)
channel <- get_connected(schema = "AFSC")

# 3. Get data
get_gps_data(region = region, channel = channel)

software_types <- c("globe", "opencpn", "timezero") 

for(ii in 1:length(software_types)) {
  set_software(software_types[ii]) # Options are globe, opencpn, timezero
  
  # 4. Historical towpath, tow start, and midpoint
  make_towpaths(
    region = region, 
    overwrite_midpoint = FALSE, 
    software_format = SOFTWARE
  )
  
  # Survey grid without trawlable/untrawlable (EBS/NBS)
  survey_grid <- map_layers$survey.grid |>
    sf::st_make_valid() |> 
    sf::st_cast("POLYGON", group_or_split = TRUE)
  survey_grid$color <- navmaps_pal(values = "cyan", software_format = SOFTWARE, file_type = FILE_TYPE_POLYGON)
  survey_grid$fill <- 0

  sf_to_nav_file(
    x = survey_grid,
    geometry = "LINESTRING",
    file = here::here("output", region, "navigation", paste0(region, "_station_grid.", FILE_TYPE_POLYGON)),
    name_col = "STATIONID",
    description_col = "STATIONID",
    color_col = "color", 
    software_format = SOFTWARE
  )
  
  # 6. Station marks
  grid_centers <- map_layers$survey.grid |>
    sf::st_make_valid() |> 
    sf::st_cast("POLYGON", group_or_split = TRUE) |>
    sf::st_centroid() # Points at the center of each grid cell
  grid_centers$shape <- navmaps_sym_pal(values = "circle1", 
                                        software_format = SOFTWARE, 
                                        file_type = FILE_TYPE_POINT, 
                                        color = "cyan")
  
  grid_centers$color <- navmaps_pal(values = "cyan", 
                                    software_format = SOFTWARE, 
                                    file_type = FILE_TYPE_POINT)
  
  sf_to_nav_file(
    x = grid_centers,
    file = here::here("output", region, "navigation", paste0(region, "_marks.", FILE_TYPE_POINT)),
    name_col = "STATIONID",
    description_col = "STATIONID",
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
    file = here::here("output", region, "navigation", paste0(region, "_survey_strata.", FILE_TYPE_POLYGON)), 
    name_col = "name",
    description_col = "Stratum",
    color_col = "color",
    fill_col = "fill",
    software_format = SOFTWARE
  )
  
  # 9. Spectacled Eider Critical Habitat
  
  eider <- sf::st_read(here::here("data", "spectacled_eider", "FCH_Somateria_fischeri_20010206.shp"))
  eider$color <- navmaps_pal(values = "red", 
                             software_format = SOFTWARE, 
                             file_type = FILE_TYPE_POLYGON)
  eider$fill <- 0
  eider$name <- "Spectacled Eider Critical Habitat"
  
  sf_to_nav_file(
    x = eider,
    file = here::here("output", region, "navigation", paste0("spectacled_eider_ch.", FILE_TYPE_POLYGON)),
    name_col = "name",
    description_col = "UNIT_ID",
    color_col = "color",
    fill_col = "fill",
    software_format = SOFTWARE
  )
  
}
