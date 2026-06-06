# Layers for the AI

# 1. Setup
library(navmaps)
region <- "ai" # Options are sebs, nbs, ai, goa

# 2. Load shapefiles using the akgfmaps package
map_layers <- akgfmaps::get_base_layers(select.region = region, split.land.at.180 = FALSE)

# Save AI survey grid and stratum shapefiles
# sf::st_write(
#   obj = sf::st_transform(map_layers$survey.grid, crs = "EPSG:3338"), 
#   dsn = here::here("output", region, "shapefiles", paste0(region, "_station_grid.shp")),
#   append = FALSE
# )

# sf::st_write(
#   obj = sf::st_transform(map_layers$survey.strata, crs = "EPSG:3338"), 
#   dsn = here::here("output", region, "shapefiles", paste0(region, "_survey_strata.shp")),
#   append = FALSE
# )

channel <- get_connected(schema = "AFSC") # 64 bit
# channel <- get_connected(schema = "AFSC_32") # 32 bit for Globe files

# 3. Get data
# get_gps_data(region = region, channel = channel)

software_types <- "timezero"

# Download latest USCG hazNav GeoJSON files (moorings, buoys)
# download.file(
#   url = "https://www.navcen.uscg.gov/sites/default/files/msi/hazNav_1.geojson",
#   destfile = here::here("assets", "data", "buoys", "hazNav_1.geojson")
# )
# download.file(
#   url = "https://www.navcen.uscg.gov/sites/default/files/msi/hazNavLine_1.geojson",
#   destfile = here::here("assets", "data", "buoys", "hazNavLine_1.geojson")
# )
# download.file(
#   url = "https://www.navcen.uscg.gov/sites/default/files/msi/hazNavPoly_1.geojson",
#   destfile = here::here("assets", "data", "buoys", "hazNavPoly_1.geojson")
# )


# for(ii in 1:length(software_types)) {
  
  set_software("timezero")
  
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
    software_format = SOFTWARE,
    fill_grid_cells = TRUE,
    filename_suffix = "_fill"
  )
  
  # 7. Station allocation
  allocation <- sf::st_read(here::here("assets", "data", "allocation", "ai_2026_station_allocation_400stn.gpkg")) |>
    tidyr::drop_na(LONGITUDE, LATITUDE, VESSEL) |>
    dplyr::mutate(VESSEL = factor(VESSEL)) |>
    sf::st_transform(crs = "WGS84") |>
    dplyr::select(-LATITUDE, -LONGITUDE) |>
    sf::st_set_geometry("geometry")
  
  # Extra stations that are not in the allocation
  no_stn <- data.frame(
    STATION = c("205-20", "236-37", "32-43", "52-39", "165-14", "169-17", "268-49", "313-68"),
    STRATUM = c(614, 523, 213, 211, 424, 421, 793, 722)
  )
  
  allocation <- dplyr::anti_join(allocation, no_stn)
  
  allocation$color <- ifelse(allocation$VESSEL == "148", "cyan", "yellow")
  allocation$color <- navmaps::navmaps_pal(values = allocation$color, software_format = "timezero", file_type = "gpx")
  allocation$shape <- ifelse(allocation$VESSEL == "148", "circle1", "triangle1")
  allocation$shape <- navmaps::navmaps_sym_pal(values = allocation$shape, software_format = "timezero", file_type = "gpx")
  allocation$description <- paste0(
    "Vessel: ", allocation$VESSEL, ", Stratum: ", allocation$STRATUM
  )
  
  sf_to_nav_file(
    x = allocation,
    file = here::here("output", region, "navigation", SOFTWARE, paste0(region, "_allocation_2026.", FILE_TYPE_POINT)),
    name_col = "STATION",
    description_col = "description",
    color_col = "color",
    shape_col = "shape",
    software_format = "timezero"
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
  ssl <- sf::st_read(here::here("assets", "data", "SSLrookeries", "3nm_notransit.shp"))
  ssl$NAME <- "SSL No-Transit"
  ssl$color <- navmaps_pal(values = "red", software_format = SOFTWARE, file_type = FILE_TYPE_POLYGON)
  ssl$fill <- 0
  
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
  
  buoys |>
    dplyr::select(-COORDINATES_JSON) |>
    sf::st_transform(crs = "EPSG:3338") |>
    sf::st_write(, dsn = here::here("output", region,  "shapefiles", paste0("hazards_buoys_", gsub("[^0-9]", "", Sys.Date()), ".gpkg")), append=FALSE)
  
  sf_to_nav_file(
    x = buoys,
    file = here::here("output", region,  "navigation", SOFTWARE, paste0("hazards_buoys_", gsub("[^0-9]", "", Sys.Date()), ".", FILE_TYPE_POINT)),
    name_col = "TYPE",
    description_col = "DESCRIPTION",
    color_col = "color",
    shape_col = "shape",
    software_format = SOFTWARE
  )
  
  # 14. Crab pot storage (requires 32-bit R to open .mdb)
  
  # Add an entry for every crab pot storage data set
  
  crabpot_lines <- sf::st_read(dsn = here::here("assets", "data", "crabpots", "2026", "crabpot_lines_2026.shp")) |>
    dplyr::select(-Shape_Leng, -INDEX)
  
  crabpot_lines$description <- "Crab pot storage"
  crabpot_lines$color <- navmaps::navmaps_pal(values = "darkorange",
                                              software_format = SOFTWARE,
                                              file_type = FILE_TYPE_LINESTRING)
  
  sf::st_transform(crabpot_lines, crs = "EPSG:3338") |>
    sf::st_write(dsn = here::here("output", region, "shapefiles", "crabpot_lines_2026.shp"),
                 append = FALSE)
  
  sf_to_nav_file(x = crabpot_lines,
                 file = here::here("output", region, "navigation", SOFTWARE,  paste0("crabpot_lines_2026.", FILE_TYPE_LINESTRING)),
                 name_col = "vessel",
                 description_col = "description",
                 color_col = "color",
                 software_format = SOFTWARE)
  
  crabpot_marks <- sf::st_read(dsn = here::here("assets", "data", "crabpots", "2026", "crabpot_marks_2026.shp")) |>
    dplyr::mutate(color = navmaps::navmaps_pal(values = "darkorange",
                                               software_format = SOFTWARE,
                                               file_type = FILE_TYPE_POINT),
                  shape = navmaps::navmaps_sym_pal("asterisk",
                                                   software_format = SOFTWARE,
                                                   file_type = FILE_TYPE_POINT),
                  description = "Crab pot storage")
  
  sf_to_nav_file(x = crabpot_marks,
                 file = here::here("output", region, "navigation", SOFTWARE, paste0("crabpot_marks_2026.", FILE_TYPE_POINT)),
                 name_col = "vessel",
                 description_col = "description",
                 shape_col = "shape",
                 color_col = "color",
                 software_format = SOFTWARE)
  
  sf::st_transform(crabpot_marks, crs = "EPSG:3338") |>
    sf::st_write(dsn = here::here("output", region, "shapefiles", "crabpot_marks_2026.shp"),
                 append = FALSE)

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