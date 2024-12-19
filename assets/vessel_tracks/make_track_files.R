library(navmaps)

# Get list of files and check that all files exist and are assigned to the correct region
track_files <- readxl::read_xlsx(path = here::here("data", "globe_tracks.xlsx")) |>
  as.data.frame()

track_files$exists <- NA

check_files <- data.frame()

track_regions <- unique(track_files$region)

for(ii in 1:length(track_regions)) {
  
  track_files$exists[track_files$region == track_regions[ii]] <- track_files$file[track_files$region == track_regions[ii]] %in%
  list.files(path = here::here("data", track_regions[ii]))
  
  
  folder_files <- data.frame(file = track_files$file[track_files$region == track_regions[ii]],
                             region = track_regions[ii])
  
  folder_files$infile <- track_files$file[track_files$region == track_regions[ii]] %in% folder_files$file
  
  check_files <- dplyr::bind_rows(check_files, folder_files)
}

# Confirm that files exist
all(track_files$exists)
all(check_files$infile)

vcr_combos <- dplyr::select(track_files,
                            dplyr::all_of(c("vessel", "year", "region"))) |>
  unique()

for(jj in 1:nrow(vcr_combos)) {

  sel_vessel_files <- track_files$file[track_files$year == vcr_combos$year[jj] &
                                         track_files$region == vcr_combos$region[jj] &
                                         track_files$vessel == vcr_combos$vessel[jj]]
  
  map_region <- tolower(vcr_combos$region[jj])
  
  if(map_region == "cs") {
    map_region <- "ecs"
  }
  
  if(map_region == "bs") {
    map_region <- "ebs"
  }
  
  map_layers <- akgfmaps::get_base_layers(select.region = map_region,
                                          set.crs = "EPSG:3338",
                                          split.land.at.180 = FALSE)
  
  for(kk in 1:length(sel_vessel_files)) {
    
    fpath <- here::here("data", vcr_combos$region[jj], sel_vessel_files[kk])
    cat("Starting file: ", fpath, "\n")
    
    sel_path <- navmaps::globe_to_sf(dsn = here::here("data", vcr_combos$region[jj], sel_vessel_files[kk]), 
                                      wkt_geometry_type = "POINT", 
                                      keep_index = FALSE) |>
      janitor::clean_names(case = "snake") |>
      dplyr::select(dplyr::all_of(c("date_time", "depth", "geometry")))
    
    if(nrow(sel_path) < 1) {
      next
    }
    
    sel_path <- sel_path |>
      dplyr::mutate(date_time = lubridate::force_tz(date_time, tzone = "UTC"),
                    color = 255) |>
      dplyr::filter(lubridate::year(date_time) == vcr_combos$year[jj])
    
    if(kk == 1) {
      path_data <- sel_path
    } else {
      path_data <- dplyr::bind_rows(path_data, sel_path)
    }
    
  }
  
  path_data <- unique(path_data) |>
    dplyr::arrange(date_time) |>
    dplyr::mutate(name = paste0(vcr_combos$region[jj], vcr_combos$year[jj], vcr_combos$vessel[jj]))
  
  # Create output directory
  dir.create(here::here("output", vcr_combos$region[jj]), recursive = TRUE, showWarnings = FALSE)
  dir.create(here::here("plots", vcr_combos$region[jj]), recursive = TRUE, showWarnings = FALSE)
  
  navmaps::sf_to_nav_file(x = path_data,
                          software_format = "globe", 
                          file = here::here("output", 
                                            vcr_combos$region[jj], 
                                            paste0("track_", 
                                                   vcr_combos$region[jj], "_", 
                                                   vcr_combos$year[jj], "_",
                                                   vcr_combos$vessel[jj], 
                                                   ".mdb")), 
                          time_col = "date_time", 
                          index_col = NULL, 
                          depth_col = "depth",
                          color_col = "color",
                          globe_track = TRUE)
  
  
  navmaps::sf_to_nav_file(x = path_data,
                          software_format = "timezero", 
                          file = here::here("output", 
                                            vcr_combos$region[jj], 
                                            paste0("track_", 
                                                   vcr_combos$region[jj], "_", 
                                                   vcr_combos$year[jj], "_",
                                                   vcr_combos$vessel[jj], 
                                                   ".kml")), 
                          time_col = "date_time", 
                          name_col = "name",
                          index_col = NULL, 
                          depth_col = "depth",
                          color_col = "color",
                          globe_track = TRUE)
  
  
  x = navmaps::globe_to_sf(dsn = here::here("assets", "track_AI_2006_143.mdb"))
  
  # x$name = "Sea_Storm_AI_2006"
  # 
  # file = "test.kml"
  # depth_col = "Depth"
  # name_col = "name"
  # time_col = "DateTime"
  
  
  path_data_aea <- sf::st_transform(path_data, crs = "EPSG:3338")

  ragg::agg_png(filename = here::here("plots", vcr_combos$region[jj], 
                                      paste0("track_", 
                                             vcr_combos$region[jj], "_", 
                                             vcr_combos$year[jj], "_",
                                             vcr_combos$vessel[jj], ".png")),
                width = 6.5, 
                height = 6.5 * diff(map_layers$plot.boundary$y)/diff(map_layers$plot.boundary$x),
                units = "in",
                res = 180)
  print(
    ggplot() +
      geom_sf(data = path_data_aea,
              mapping = aes(color = depth),
              size = rel(0.5)) +
      geom_sf(data = map_layers$akland) +
      scale_color_viridis_c(name = "Depth (m)", option = "H") +
      scale_x_continuous(limits = map_layers$plot.boundary$x,
                         breaks = map_layers$lon.breaks) +
      scale_y_continuous(limits = map_layers$plot.boundary$y,
                         breaks = map_layers$lat.breaks) +
      theme_bw() +
      theme(axis.title = element_blank())
  )
  dev.off()
  
  gc()
  
}
