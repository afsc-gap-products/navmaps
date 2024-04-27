# Example of creating layers for the EBS/NBS

# 1. Setup
library(navmaps)
region <- "sebs" # Options are sebs, nbs, ai, goa

# 2. Load shapefiles using the akgfmaps package
map_layers <- akgfmaps::get_base_layers(select.region = region, 
                                        split.land.at.180 = FALSE)

channel <- get_connected(schema = "AFSC_32")

# 3. Get data
get_gps_data(region = region, channel = channel)

# Options are globe, opencpn, timezero
software_types <- c("globe", "timezero", "opencpn") 

for(ii in 1:length(software_types)) {
  set_software(software_types[ii]) 
  
  # 4. Historical towpath, tow start, and midpoint
  # make_towpaths(
  #   region = region,
  #   overwrite_midpoint = ifelse(ii == 1, TRUE, FALSE),
  #   software_format = SOFTWARE
  # )

  # 5. Station grid
  # survey_grid <- map_layers$survey.grid
  # survey_grid$color <- navmaps_pal(values = "tan", software_format = SOFTWARE, file_type = FILE_TYPE_POLYGON)
  # survey_grid$fill <- 0
  # 
  # sf_to_nav_file(
  #   x = survey_grid,
  #   geometry = "LINESTRING",
  #   file = here::here("output", region, "navigation", SOFTWARE, paste0(region, "_station_grid.", FILE_TYPE_POLYGON)),
  #   name_col = "STATIONID",
  #   description_col = "STATIONID",
  #   color_col = "color",
  #   software_format = SOFTWARE
  # )
  # 
  # # 6. Station marks
  # grid_centers <- sf::st_centroid(map_layers$survey.grid) # Points at the center of each grid cell
  # grid_centers$shape <- navmaps_sym_pal(values = "circle1",
  #                                       software_format = SOFTWARE,
  #                                       file_type = FILE_TYPE_POINT,
  #                                       color = "tan")
  # 
  # grid_centers$color <- navmaps_pal(values = "tan",
  #                                   software_format = SOFTWARE,
  #                                   file_type = FILE_TYPE_POINT)
  # 
  # sf_to_nav_file(
  #   x = grid_centers,
  #   file = here::here("output", region, "navigation", SOFTWARE, paste0(region, "_marks.", FILE_TYPE_POINT)),
  #   name_col = "STATIONID",
  #   description_col = "STATIONID",
  #   color_col = "color",
  #   shape_col = "shape",
  #   software_format = SOFTWARE
  # )
  # 
  # # 8. Survey stratum layer
  # strata <- map_layers$survey.strata
  # names(strata)[which(names(strata) != "geometry")] <- toupper(names(strata)[which(names(strata) != "geometry")])
  # strata$color <- navmaps_pal(values = "yellow", software_format = SOFTWARE, file_type = FILE_TYPE_POLYGON)
  # strata$fill <- 0
  # strata$name <- paste0("Stratum ", strata$STRATUM)
  # 
  # sf_to_nav_file(
  #   x = strata,
  #   file = here::here("output", region, "navigation", SOFTWARE, paste0(region, "_survey_strata.", FILE_TYPE_POLYGON)),
  #   name_col = "name",
  #   description_col = "STRATUM",
  #   color_col = "color",
  #   fill_col = "fill",
  #   software_format = SOFTWARE
  # )
  # 
  # # 9. SSL buffer zones
  # ssl <- sf::st_read(here::here("assets", "data", "SSLrookeries", "3nm_No_Transit.shp"))
  # ssl$color <- navmaps_pal(values = "red", software_format = SOFTWARE, file_type = FILE_TYPE_POLYGON)
  # ssl$fill <- navmaps_pal(values = "red", software_format = SOFTWARE, file_type = FILE_TYPE_POLYGON)
  # 
  # sf_to_nav_file(
  #   x = ssl,
  #   file = here::here("output", region, "navigation", SOFTWARE, paste0(region, "_ssl_no_transit.", FILE_TYPE_POLYGON)),
  #   name_col = "Name",
  #   description_col = "Name",
  #   color_col = "color",
  #   fill_col = "fill",
  #   software_format = SOFTWARE
  # )
  # 
  # # 10. Sea Otter Critical Habitat
  # otters <- sf::st_read(here::here("assets", "data", "otters", "SeaOtterFinalCH_Project.shp"))
  # otters$name <- "Otter Habitat"
  # otters$color <- navmaps_pal(values = "red", software_format = SOFTWARE, file_type = FILE_TYPE_POLYGON)
  # otters$fill <- 0
  # 
  # sf_to_nav_file(x = otters,
  #                file = here::here("output", region, "navigation", SOFTWARE, paste0("otter_habitat.", FILE_TYPE_POLYGON)),
  #                name_col = "name",
  #                description_col = "CH_Unit",
  #                color_col = "color",
  #                fill_col = "fill",
  #                software_format = SOFTWARE)
  # 
  # # 11. North Pacific Right Whale Critical Habitat
  # nprw <- sf::st_read(here::here("assets", "data", "NPRW", "NPRWCH.shp")) |>
  #   sf::st_transform(crs = "EPSG:4326")
  # nprw$name <- "NPRW Critical Habitat"
  # nprw$description <- "NPRW Critical Habitat"
  # nprw$color <- navmaps_pal(values = "red", software_format = SOFTWARE, file_type = FILE_TYPE_POLYGON)
  # nprw$fill <- 0
  # 
  # sf_to_nav_file(x = nprw,
  #                file = here::here("output", region, "navigation", SOFTWARE, paste0("NPRW_Critical_Habitat.", FILE_TYPE_POLYGON)),
  #                name_col = "name",
  #                description_col = "description",
  #                color_col = "color",
  #                fill_col = "fill",
  #                software_format = SOFTWARE)
  # 
  # # 12. Humpback Whale Critical Habitat
  # humpback <- sf::st_read(here::here("assets", "data", "humpback", "WhaleHumpback_WesternNorthPacificDPS_20210421.shp")) |>
  #   sf::st_transform(crs = "EPSG:4326")
  # humpback$name <- "Humpback Critical Habitat"
  # humpback$description <- "Humpback Critical Habitat"
  # humpback$color <- navmaps_pal(values = "red", software_format = SOFTWARE, file_type = FILE_TYPE_POLYGON)
  # humpback$fill <- 0
  # 
  # sf_to_nav_file(x = humpback,
  #                file = here::here("output", region, "navigation", SOFTWARE, paste0("Humpback_Critical_Habitat.", FILE_TYPE_POLYGON)),
  #                name_col = "name",
  #                description_col = "description",
  #                color_col = "color",
  #                fill_col = "fill",
  #                software_format = SOFTWARE)
  # 
  # # 13. Buoys
  # 
  # 
  # buoys <- readxl::read_xlsx(here::here("assets", "data", "buoys", "lnm_moorings_20240227.xlsx")) |>
  #   as.data.frame() |>
  #   dplyr::mutate(LONGITUDE = dms_string_to_dd(POSITION)[,1],
  #                 LATITUDE = dms_string_to_dd(POSITION)[,2]) |>
  #   sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = "EPSG:4326") |>
  #   dplyr::rename(TYPE_NAME = `TYPE/NAME`)
  # 
  # buoys$shape <- navmaps_sym_pal(values = "warning",
  #                                software_format = SOFTWARE,
  #                                file_type = FILE_TYPE_POINT)
  # buoys$color <- navmaps_pal(values = "darkorange",
  #                            software_format = SOFTWARE,
  #                            file_type = FILE_TYPE_POINT)
  # 
  # buoys$description <- paste0("Top float: ", buoys$`TOP FLOAT DEPTH`, "; Depth: ", buoys$`WATER DEPTH`)
  # 
  # sf_to_nav_file(x = buoys,
  #                file = here::here("output", region,  "navigation", SOFTWARE, paste0("buoys_20240227.", FILE_TYPE_POINT)),
  #                name_col = "TYPE_NAME",
  #                description_col = "description",
  #                color_col = "color",
  #                shape_col = "shape",
  #                software_format = SOFTWARE)

  # 14. Slope special project

  # Target strata
  slope_sample_zones <- sf::st_read(here::here("assets", "data", "special_projects", "EBS", "2024", "2024_sample_zones_shelf_slope.shp"))

  slope_sample_zones$color <- navmaps_pal(values = "cyan",
                                          software_format = SOFTWARE,
                                          file_type = FILE_TYPE_POLYGON)
  slope_sample_zones$fill <- 0
  slope_sample_zones$name <- "Shelf_Slope"

  sf_to_nav_file(x = slope_sample_zones,
                 file = here::here("output", region,  "navigation", SOFTWARE, paste0("shelf_slope_sample_zones.", FILE_TYPE_POLYGON)),
                 name_col = "name",
                 description_col = "label",
                 color_col = "color",
                 shape_col = "shape",
                 fill_col = "fill",
                 software_format = SOFTWARE)
  
  # 2024 station allocation
  slope_allocation <- sf::st_read(here::here("assets", "data", "special_projects", "EBS", "2024", "2024_slope_allocation.shp"))
  
  slope_allocation$color <- navmaps_pal(values = c("cyan", "orange"), 
                                     software_format = SOFTWARE,
                                     file_type = FILE_TYPE_POINT)[as.numeric(factor(slope_allocation$PRIMARY))]
  
  slope_allocation$shape <- navmaps_sym_pal(values = c("circle1", "square1"), 
                                         software_format = SOFTWARE,
                                         file_type = FILE_TYPE_POINT)[as.numeric(factor(slope_allocation$PRIMARY))]
  
  sf_to_nav_file(x = slope_allocation,
                 file = here::here("output", region,  "navigation", SOFTWARE, paste0("2024_slope_allocation.", FILE_TYPE_POINT)),
                 name_col = "STATIONID",
                 description_col = "PRIMARY",
                 color_col = "color",
                 shape_col = "shape",
                 software_format = SOFTWARE)

  # Historical tow paths
  slope_towpaths <- sf::st_read(here::here("assets", "data", "special_projects", "EBS", "2024", "slope_towpaths.shp")) |>
    dplyr::mutate(name = paste0(floor(CRUISE/100), "/", VESSEL, "/", HAUL),
                  desc = paste0(PERFORM, ": ", PERFDES))

  slope_towpaths$color <- navmaps_pal(values = c("red", "lightgreen", "purple"),
                                        software_format = SOFTWARE,
                                        file_type = FILE_TYPE_LINESTRING)[sign(slope_towpaths$PERFORM)+2]

  sf_to_nav_file(x = slope_towpaths,
                 file = here::here("output", region, "navigation", SOFTWARE, paste0("slope_towpaths.", FILE_TYPE_LINESTRING)),
                 name_col = "name",
                 description_col = "desc",
                 color_col = "color",
                 time_col = "TIME",
                 software_format = SOFTWARE)

  # Historical tow starts
  slope_tow_starts <- sf::st_read(here::here("assets", "data", "special_projects", "EBS", "2024", "slope_tow_starts.shp")) |>
    dplyr::mutate(name = paste0(floor(CRUISE/100), "/", VESSEL, "/", HAUL),
                  desc = paste0(PERFORM, ": ", PERFDES))

  slope_tow_starts$color <- navmaps_pal(values = c("red", "lightgreen", "purple"),
                                      software_format = SOFTWARE,
                                      file_type = FILE_TYPE_POINT)[sign(slope_tow_starts$PERFORM)+2]

  slope_tow_starts$shape <- navmaps_sym_pal(values = c("asterisk", "diamond", "diamond"),
                                            software_format = SOFTWARE,
                                            file_type = FILE_TYPE_POINT)[sign(slope_tow_starts$PERFORM)+2]

  sf_to_nav_file(x = slope_tow_starts,
                 file = here::here("output", region, "navigation", SOFTWARE, paste0("slope_tow_starts.", FILE_TYPE_POINT)),
                 name_col = "name",
                 description_col = "desc",
                 color_col = "color",
                 time_col = "TIME",
                 shape_col = "shape",
                 software_format = SOFTWARE)
  
  # Skate Nursery HAPC
  
  skate_hapc <- skate_hapc <- sf::st_read(here::here("assets", "data", "alaska_hapc", "alaska_hapc.shp")) |>
    dplyr::filter(grepl(pattern = "Skate Nursery Areas", x = SITENAME_L)) |>
    dplyr::mutate(name = "Skate Nursery (HAPC)")
  
  skate_hapc$color <- navmaps_pal(values = "red",
                                  software_format = SOFTWARE,
                                  file_type = FILE_TYPE_POLYGON)
  skate_hapc$fill <- 0
  
  
  sf_to_nav_file(x = skate_hapc,
                 file = here::here("output", region,  "navigation", SOFTWARE, paste0("slope_skate_hapc.", FILE_TYPE_POLYGON)),
                 name_col = "name",
                 description_col = "name",
                 color_col = "color",
                 fill_col = "fill",
                 software_format = SOFTWARE)


  # 15. 15/30 Special project
  zones_15_30 <- sf::st_read(here::here("assets", "data", "special_projects", "EBS", "2024", "2024_samples_zones_15_30.shp"))


  zones_15_30$color <- navmaps_pal(values = "yellow",
                                          software_format = SOFTWARE,
                                          file_type = FILE_TYPE_POLYGON)
  zones_15_30$fill <- 0
  zones_15_30$name <- "15_30"

  sf_to_nav_file(x = zones_15_30,
                 file = here::here("output", region,  "navigation", SOFTWARE, paste0("15_30_zones.", FILE_TYPE_POLYGON)),
                 name_col = "name",
                 description_col = "area",
                 color_col = "color",
                 shape_col = "shape",
                 fill_col = "fill",
                 software_format = SOFTWARE)
  
}
