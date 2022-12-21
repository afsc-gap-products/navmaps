# 1. Install navmaps on 64-bit version of R 4.1.1-4.1.3
#  library(devtools)
#  install_github("sean-rohan-noaa/navmaps")
# 2. Open R Studio and create a new project with the navmaps repo cloned from GitHub
# 3. Switch to a 32-bit version of R 4.1.1-4.1.3 in Global Options
# 4. Restart R Studio and verify that the startup message in the console shows 'R version 4.1.X' and 'Platform: i386-w64-mingw32/i386 (32-bit)'
# 5. Run the code below line by line. If everything runs correctly, the code should write the following files files:
#   - [local_dir]/output/sebs/navigation/sebs_towpath.csv
#   - [local_dir]/output/sebs/navigation/sebs_towpath.mdb
#   - [local_dir]/output/sebs/navigation/sebs_towstarts.csv
#   - [local_dir]/output/sebs/navigation/sebs_towstarts.accdb

library(navmaps)
region <- "sebs"

# Test line plotting as .csv and .mdb
start_and_end <- readRDS(file = here::here("assets", paste0(region, "_haul_start_end.rds"))) |>
  sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = "EPSG:4326") |>
  dplyr::group_by(VESSEL, CRUISE, HAUL, PERFORMANCE, PERFORMANCE_DESCRIPTION, BOTTOM_DEPTH) |> 
  dplyr::summarize(DATE_TIME = mean(DATE_TIME, na.rm = TRUE), do_union = FALSE) |> 
  sf::st_cast("LINESTRING") |>
  dplyr::rename(DEPTH = BOTTOM_DEPTH)

start_and_end$color <- 255

sf_to_globe_linestring(
  x = start_and_end,
  file = here::here("output", region, "navigation", paste0(region,  "_towpath.csv")),
  time_col = "DATE_TIME",
  color_col = "color",
  extra_cols = c("CRUISE", "HAUL", "DEPTH", "PERFORMANCE")
)

sf_to_globe_linestring(
  x = start_and_end,
  file = here::here("output", region, "navigation", paste0(region,  "_towpath.mdb")),
  time_col = "DATE_TIME",
  color_col = "color",
  extra_cols = c("CRUISE", "HAUL", "DEPTH", "PERFORMANCE")
)



# Test mark files as .csv and .accdb
start_only <- readRDS(file = here::here("assets", paste0(region, "_haul_start_end.rds"))) |>
  dplyr::filter(EVENT == "start") |>
  sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = "EPSG:4326") |>
  dplyr::rename(DEPTH = BOTTOM_DEPTH)

start_only$color <- globe_pal(1)
start_only$shape <- 2

sf_to_globe_points(x = start_only, 
                   file  = here::here("output", region, "navigation", paste0(region,  "_towstarts.csv")), 
                   color_col = "color", 
                   shape_col = "shape", 
                   time_col = "DATE_TIME", 
                   extra_cols c("PERFORMANCE", "EVENT", "HAULJOIN"))

sf_to_globe_points(x = start_only, 
                   file  = here::here("output", region, "navigation", paste0(region,  "_towstarts.accdb")), 
                   color_col = "color", 
                   shape_col = "shape", 
                   time_col = "DATE_TIME", 
                   extra_cols c("PERFORMANCE", "EVENT", "HAULJOIN"))

