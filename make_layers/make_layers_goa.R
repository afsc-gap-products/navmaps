# Layers for the GOA

# Update filepaths to match the year

# 1. Setup
library(navmaps)
region <- "goa" # Options are sebs, nbs, ai, goa

# 2. Load shapefiles using the akgfmaps package
map_layers <- akgfmaps::get_base_layers(select.region = region, split.land.at.180 = FALSE)

saveRDS(object = map_layers, file = here::here("assets", "data", paste0(region, "_map_layers.rds")))

channel <- get_connected(schema = "AFSC")

# 3. Get data
get_gps_data(region = region, channel = channel)

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

# 4. Make layers
software_types <- c("timezero", "opencpn")

for(ii in 1:length(software_types)) {
  
  set_software(software_types[ii])
  
  if(SOFTWARE == "globe") {
    map_layers <- readRDS(here::here("assets", "data", paste0(region, "_map_layers.rds")))
  }

  # 4. Historical towpath, tow start, and midpoint
  make_towpaths(
    region = region,
    overwrite_midpoint = FALSE,
    software_format = SOFTWARE
  )

  # 5. Trawlable/untrawlable station grid (lines) and marks
  make_trawlable(
    region = region,
    channel = channel,
    software_format = SOFTWARE,
    fill_grid_cells = TRUE,
    filename_suffix = "_fill"
  )
  
  # 6. Station marks
  grid_centers <- sf::st_centroid(trawlable_grid)# Points at the center of each grid cell
  grid_centers$shape <- 
    navmaps_sym_pal(
      values = "circle1",
      software_format = SOFTWARE,
      file_type = FILE_TYPE_POINT,
      color = "cyan"
    )
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

  # 7. Station allocation
  allocation <- 
    sf::st_read(here::here("assets", "data", "allocation", "goa_2025_station_allocation_450_aea.gpkg")) |>
    tidyr::drop_na(LONGITUDE, LATITUDE, VESSEL) |>
    dplyr::mutate(VESSEL = factor(VESSEL)) |>
    sf::st_transform(crs = "WGS84") |>
    dplyr::select(-LATITUDE, -LONGITUDE) |>
    sf::st_set_geometry("geometry")
  
  allocation$color <- ifelse(allocation$VESSEL == "148", "cyan", "yellow")
  allocation$color <- navmaps::navmaps_pal(values = allocation$color, software_format = "timezero", file_type = "gpx")
  allocation$shape <- ifelse(allocation$VESSEL == "148", "circle1", "triangle1")
  allocation$shape <- navmaps::navmaps_sym_pal(values = allocation$shape, software_format = "timezero", file_type = "gpx")
  allocation$description <- paste0(
    "Vessel: ", allocation$VESSEL, ", Stratum: ", allocation$STRATUM
  )
  
  sf_to_nav_file(
    x = allocation,
    file = here::here("output", region, "navigation", SOFTWARE, paste0(region, "_allocation_2025.", FILE_TYPE_POINT)),
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
  
  sf_to_nav_file(
    x = otters,
    file = here::here("output", region, "navigation",SOFTWARE, paste0("otter_habitat.", FILE_TYPE_POLYGON)),
    name_col = "name",
    description_col = "CH_Unit",
    color_col = "color",
    fill_col = "fill",
    software_format = SOFTWARE
  )
  
  # 11. North Pacific Right Whale Critical Habitat
  nprw <- sf::st_read(here::here("assets", "data", "NPRW", "NPRWCH.shp")) |>
    sf::st_transform(crs = "EPSG:4326")
  nprw$name <- "NPRW Critical Habitat"
  nprw$description <- "NPRW Critical Habitat"
  nprw$color <- 
    navmaps_pal(values = "red", software_format = SOFTWARE, file_type = FILE_TYPE_POLYGON)
  nprw$fill <- 0
  
  sf_to_nav_file(
    x = nprw,
    file = here::here("output", region, "navigation", SOFTWARE, paste0("NPRW_Critical_Habitat.", FILE_TYPE_POLYGON)),
    name_col = "name",
    description_col = "description",
    color_col = "color",
    fill_col = "fill",
    software_format = SOFTWARE
  )
  
  # 12. Humpback Whale Critical Habitat
  humpback <- sf::st_read(here::here("assets", "data", "humpback", "WhaleHumpback_WesternNorthPacificDPS_20210421.shp")) |>
    sf::st_transform(crs = "EPSG:4326")
  humpback$name <- "Humpback Critical Habitat"
  humpback$description <- "Humpback Critical Habitat"
  humpback$color <- 
    navmaps_pal(values = "red", software_format = SOFTWARE, file_type = FILE_TYPE_POLYGON)
  humpback$fill <- 0
  
  sf_to_nav_file(
    x = humpback,
    file = here::here("output", region, "navigation", SOFTWARE, paste0("Humpback_Critical_Habitat.", FILE_TYPE_POLYGON)),
    name_col = "name",
    description_col = "description",
    color_col = "color",
    fill_col = "fill",
    software_format = SOFTWARE
  )

  # 13 Cook Inlet Beluga Whale Critical Habitat

  beluga <- sf::st_read(here::here("assets", "data", "cibeluga", "cib_critical_habitat.shp")) |>
    sf::st_transform(crs = "EPSG:4326")
  beluga$name <- "Beluga Critical Habitat"
  beluga$description <- "Beluga Critical Habitat"
  beluga$color <- navmaps_pal(values = "red", software_format = SOFTWARE, file_type = FILE_TYPE_POLYGON)
  beluga$fill <- 0

  sf_to_nav_file(
    x = beluga,
    file = here::here("output", region, "navigation", SOFTWARE, paste0("CIBeluga_Critical_Habitat.", FILE_TYPE_POLYGON)),
    name_col = "name",
    description_col = "description",
    color_col = "color",
    fill_col = "fill",
    software_format = SOFTWARE
  )

  # 14. Navigation hazards (moorings, shipwrecks, etc.)
  # From USCG Local Notice to Mariners (Arctic/District 17)
  # https://www.navcen.uscg.gov/msi
  buoys <- sf::st_read(dsn = here::here("assets", "data", "buoys", "hazNav_1.geojson")) |>
    dplyr::filter(ATU == 17)
  
  buoys$shape <- 
    navmaps_sym_pal(
      values = "warning",
      software_format = SOFTWARE,
      file_type = FILE_TYPE_POINT
    )
  buoys$color <- 
    navmaps_pal(
      values = "darkorange",
      software_format = SOFTWARE,
      file_type = FILE_TYPE_POINT
    )
  
  sf_to_nav_file(
    x = buoys,
    file = here::here("output", region, "navigation", SOFTWARE, paste0("hazards_buoys_", gsub("[^0-9]", "", Sys.Date()), ".", FILE_TYPE_POINT)),
    name_col = "SUB_CATEGORY",
    description_col = "DESCRIPTION",
    color_col = "color",
    shape_col = "shape",
    software_format = SOFTWARE
  )
  
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
  
  
  # # 15. Crab pot storage
  # 
  # # Add an entry for every crab pot storage data set
  # 
  # crabpots <- rbind(
  #   sf::st_read(here::here("assets", "data", "crabpots", "2025", "crabpots_early_dawn_2025.shp")),
  #   sf::st_read(here::here("assets", "data", "crabpots", "2025", "crabpots_erla_n_2025.shp"))
  # )
  # 
  # crabpots$description <- "Crab pot storage"
  # crabpots$color <- navmaps_pal(values = "red", software_format = SOFTWARE, file_type = FILE_TYPE_POINT)
  # 
  # sf_to_nav_file(x = crabpots,
  #                file = here::here("output", region, "navigation", SOFTWARE, paste0("goa_crabpots_2025.", FILE_TYPE_LINESTRING)),
  #                name_col = "vessel",
  #                description_col = "description",
  #                color_col = "color",
  #                software_format = SOFTWARE)
  # 
  # sf::st_transform(crabpots, crs = "EPSG:3338") |>
  #   st_write(here::here("output", region, "shapefiles", "goa_crabpots_2025.shp"),
  #            overwrite = TRUE)
  # 
  # 16. Canadian border claim at Dixon Entrance
  dixon_entrance <- sf::st_read(here::here("assets", "data", "dixon_entrance", "dixon_entrance.shp"),
                                crs = "EPSG:3338") |>
    sf::st_transform(crs = "WGS84")

  dixon_entrance$description <- "Dixon Entrance disputed border"
  dixon_entrance$name <- "Dixon Entrance disputed border"
  dixon_entrance$fill <- 0
  dixon_entrance$color <- 
    navmaps_pal(
      values = "darkorange",
      software_format = SOFTWARE,
      file_type = FILE_TYPE_POLYGON
    )

  sf_to_nav_file(
    x = dixon_entrance,
    file = here::here("output", region, "navigation", SOFTWARE, paste0("disputed_border.", FILE_TYPE_POLYGON)),
    name_col = "name",
    description_col = "description",
    color_col = "color",
    fill_col = "fill",
    software_format = SOFTWARE
  )

  # # 17. Special collection: GOA drop camera zones ----
  drop_cam_zones <-
    sf::st_read(
      dsn = here::here("assets", "data", "special_projects", "GOA", "2025", "catcam_areas.gpkg")
    ) |>
    sf::st_set_geometry("geometry") |>
    sf::st_cast("POLYGON")

  drop_cam_zones$name <- "Drop cam zones"
  drop_cam_zones$description <- c("Zone #1", "Zone #2", "Zone #3")
  drop_cam_zones$fill <- 0
  drop_cam_zones$color <- 
    navmaps_pal(
      values = "magenta",
      software_format = SOFTWARE,
      file_type = FILE_TYPE_POLYGON
    )

  sf_to_nav_file(
    x = drop_cam_zones,
    file = here::here("output", region, "navigation", SOFTWARE, paste0("drop_cam_zones.", FILE_TYPE_POLYGON)),
    name_col = "name",
    description_col = "description",
    color_col = "color",
    fill_col = "fill",
    software_format = SOFTWARE
  )
  
  sf::st_write(
    drop_cam_zones,
    here::here("output", region, "shapefiles", "drop_cam_zones.shp"),
    append = FALSE
  )

  # 18. Rocket exclusion zone ----

  rocket_launch_zone <- sf::st_read(
    dsn = here::here("assets", "data", "Rocket_Exclusion_Zone", "DARPA", "DARPALaunch.shp")
    ) |>
    sf::st_transform(crs = "WGS84")

  rocket_launch_zone$name <- "Rocket launch zone"
  rocket_launch_zone$description <- "Rocket launch zone"
  rocket_launch_zone$fill <- 0
  rocket_launch_zone$color <- 
    navmaps::navmaps_pal(
      values = "red",
      software_format = SOFTWARE,
      file_type = FILE_TYPE_POLYGON
    )

  sf_to_nav_file(
    x = rocket_launch_zone,
    file = here::here("output", region, "navigation", SOFTWARE, paste0("rocket_exclusion_zone.", FILE_TYPE_POLYGON)),
    name_col = "name",
    description_col = "description",
    color_col = "color",
    fill_col = "fill",
    software_format = SOFTWARE
  )
  
  
}
