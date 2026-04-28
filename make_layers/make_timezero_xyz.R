# Create .xyz files for TimeZero Personal Bathymetry Generator
# 
# This script generates lat/lon/depth .xyz files from bathymetry raster files and bottom trawl 
# survey track data, which can be imported into TimeZero. xyz files are comma separated value files
# with longitude, latitude, and depth (negative for below the sea surface) columns without column
# names. Refer to the TimeZero User Manual or help files for more information.
#
# Last update: 4/28/2026 by Sean Rohan

library(navmaps)

region <- "AI"

# Path to Globe/TimeZero track data compilation ----
dsn_tracks <- here::here("output", paste0("tracks_", region, ".gpkg"))

# Path to Zimmerman's bathymetry grid ----
bathy <- terra::rast(x = here::here("data", paste0(region, "_grid_100m")))

design_year <-
  switch(
    region,
    AI = 1984,
    GOA = 1980,
    EBS = 2010
  )

buffer_m <-
  switch(
    region,
    AI = 3000,
    GOA = 3000,
    EBS = 100000
  )

# max_index <- 
#   sf::st_read(
#     dsn = dsn_tracks, 
#     query = paste0("SELECT COUNT(*) FROM tracks_", region)
#   ) |>
#   as.numeric()

# start_index <- 0

# Survey area including Bower's ridge for the AI - modify for other regions
map_vector <- 
  akgfmaps::get_base_layers(
    select.region = region, 
    design.year = 1984, 
    set.crs = 3338
  )

# 3 km buffer around survey area
survey_area <- sf::st_buffer(map_vector$survey.area, dist = buffer_m) |>
  dplyr::select(geometry)

# while(start_index < max_index) {

# end_index <- start_index + 5e6
# end_index <- start_index + 1e6

# tracks <- sf::st_read(
#   dsn = dsn_tracks, 
#   query = paste0("SELECT * from tracks_", region, " where DEPTH > 0 and DEPTH < 1200 and FID > ", start_index, " and FID <= ", end_index)
#   ) |>
#   sf::st_transform(crs = crs(bathy))

# Make track xyz file ----

# Get data from 0-1200 m
tracks <- 
  sf::st_read(
    dsn = dsn_tracks, 
    query = paste0("SELECT * from tracks_", region, " where DEPTH > 0 and DEPTH < 1200")
  ) |>
  sf::st_transform(crs = terra::crs(bathy))

# Only use data within 3 km of the survey area boundary and without large differences compared to bathymetry grid
tracks$RASTER_DEPTH <- terra::extract(bathy, tracks)[,2]

in_area <- as.numeric(sf::st_within(tracks, survey_area)) 

tracks$flag <- ifelse(is.na(in_area), 1, 0)

tracks <- tracks |>
  dplyr::mutate(
    flag = dplyr::case_when(
      # Condition: > 50% difference
      abs(DEPTH - RASTER_DEPTH) / RASTER_DEPTH > 0.50 ~ -3,
      
      # Condition: 20-50% difference AND depth > 50 m
      abs(DEPTH - RASTER_DEPTH) / RASTER_DEPTH > 0.20 & 
        abs(DEPTH - RASTER_DEPTH) / RASTER_DEPTH <= 0.50 & 
        RASTER_DEPTH > 50 ~ 3,
      
      # Condition: 30-50% difference AND depth <= 50
      abs(DEPTH - RASTER_DEPTH) / RASTER_DEPTH > 0.30 & 
        abs(DEPTH - RASTER_DEPTH) / RASTER_DEPTH <= 0.50 & 
        RASTER_DEPTH <= 50 ~ 3,
      TRUE ~ flag
    )
  )

coords <- sf::st_coordinates(tracks$geom)

clean_tracks <- dplyr::filter(tracks, flag == 0) |>
  dplyr::select(DEPTH) |>
  sf::st_transform(crs = "WGS84")

out <- 
  data.frame(
    x = coords[,1],
    y = coords[,2],
    z = tracks$DEPTH*-1 # TimeZero 
  )

write.table(
  out,
  here::here("output", tolower(region), "navigation", "timezero", paste0(tolower(region), "_tracks.xyz")),
  col.names = FALSE,
  row.names = FALSE,
  sep = ","
)

# start_index <- end_index
# }

# Make bathymetry xyz file ----

gc()

# Use data within 3 km of the survey area
survey_area_bathy <- 
  terra::mask(bathy, survey_area) |>
  terra::trim() |> 
  terra::as.points() |>
  sf::st_as_sf() |>
  sf::st_transform(crs = "WGS84")

names(survey_area_bathy)[1] <- "DEPTH"

survey_area_bathy <-
  dplyr::filter(survey_area_bathy, DEPTH > 1)

coords <- sf::st_coordinates(survey_area_bathy)

bathy_out <- 
  data.frame(
    x = coords[,1],
    y = coords[,2],
    z = round(survey_area_bathy$DEPTH*-1, 1)
  )

write.table(
  bathy_out,
  here::here("output", tolower(region), "navigation", "timezero", paste0(tolower(region), "_bathy.xyz")),
  col.names = FALSE,
  row.names = FALSE,
  sep = ","
)

