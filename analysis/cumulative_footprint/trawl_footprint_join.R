# Make approximate area swept polygons ----

library(navmaps)
library(lubridate)
library(akgfmaps)
library(ggthemes)

region <- "ai"
demo_mode <- FALSE
demo_station <- "228-30"

map_layers <- akgfmaps::get_base_layers(select.region = "AI", set.crs = "EPSG:3338")

station_cell <- dplyr::filter(map_layers$survey.grid, ID == demo_station)

station_bbox <- sf::st_bbox(station_cell)

dir.create(here::here("analysis", "cumulative_footprint", "plots"),
           showWarnings = FALSE)

# What proportion of the towed area is 'new' in each year?

footprint_data <- sf::st_read(
  here::here("output", 
             region, 
             "gps", 
             paste0(region, "_buffered_towpath.gpkg")
  )
)

unique_contact_type <- unique(footprint_data$contact_type)

for(ii in 1:length(unique_contact_type)) {
  
  sel_footprint_data <- dplyr::filter(footprint_data, contact_type == unique_contact_type[ii])
  
  survey_years <- sort(unique(lubridate::year(sel_footprint_data$START_TIME)))
  
  annual_area_km2 <- numeric(length = length(survey_years))
  cumulative_area_km2 <- numeric(length = length(survey_years)) 
  new_area_km2 <- numeric(length = length(survey_years))
  
  process_start_time <- Sys.time()
  
  for(jj in 1:length(survey_years)) {
    
    if(demo_mode) {
      
      sel_year <- dplyr::filter(sel_footprint_data, 
                                lubridate::year(START_TIME) == survey_years[jj],
                                STATIONID == demo_station) |>
        dplyr::select(geom) |> 
        sf::st_union(by_feature = FALSE)
      
    } else {
      
      sel_year <- dplyr::filter(sel_footprint_data, 
                                lubridate::year(START_TIME) == survey_years[jj]) |>
        dplyr::select(geom) |> 
        sf::st_union(by_feature = FALSE)
      
    }
    
    annual_area_km2[jj] <- sum(as.numeric(sf::st_area(sel_year))/1e6)
    
    if(jj == 1) {
      
      cumulative_footprint <- sel_year
      
    } else {
      
      old_footprint <- cumulative_footprint
      
      cumulative_footprint <- sf::st_union(cumulative_footprint, sel_year) |>
        sf::st_combine() |>
        sf::st_as_sf() |>
        sf::st_make_valid()
      
      new_area_km2[jj] <- cumulative_area_km2[jj]-cumulative_area_km2[jj-1]
      
    }
    
    cumulative_footprint <- sf::st_union(cumulative_footprint)
    
    cumulative_area_km2[jj] <- sum(as.numeric(sf::st_area(cumulative_footprint))/1e6)
    
    print(paste0(Sys.time(), ": ", jj, 
                 " - Elapsed: ", 
                 round(difftime(Sys.time(), 
                                process_start_time, units = "mins"), 
                       2)))
    
    if(demo_mode) {
      
      if(jj == 1) {
        old_footprint <- cumulative_footprint
        next
      }
      
      demo_cumulative_footprint <- dplyr::bind_rows(
        sf::st_as_sf(old_footprint) |>
          select(geom = x) |>
          dplyr::mutate(YEAR = paste0(min(survey_years), "-", survey_years[jj-1])),
        sf::st_as_sf(sel_year) |>
          select(geom = x) |>
          dplyr::mutate(YEAR = as.character(survey_years[jj]))
      )
      
      demo_station_plot <- ggplot() +
        geom_sf(data = map_layers$survey.grid, 
                fill = NA, color = "grey50") +
        geom_sf(data = demo_cumulative_footprint, 
                mapping = aes(fill = YEAR), 
                color = NA) +
        geom_sf(data = map_layers$akland) +
        ggtitle(survey_years[jj]) +
        scale_fill_manual(name = "Years", values = c("#F28E2B", "#E15759")) +
        scale_x_continuous(limits = c(station_bbox['xmin'], station_bbox['xmax']),
                           breaks = c(-172.96, -172.92, -172.88)) +
        scale_y_continuous(limits = c(station_bbox['ymin'], station_bbox['ymax']),
                           breaks = c(52.22, 52.24, 52.26, 52.28)) +
        theme_bw() +
        theme(legend.position = "bottom",
              plot.title = element_text(hjust = 0.5),
              panel.background = element_rect(fill = "#CCFFFF"))
      
      ragg::agg_png(here::here("analysis", "cumulative_footprint", "plots", 
                               paste0("demo_station_plot_", unique_contact_type[ii], "_", survey_years[jj], ".png")), 
                    width = 100, 
                    height = 100, 
                    units = "mm", 
                    res = 300)
      print(demo_station_plot)
      dev.off()
      
    }
    
  }
  
  new_areas <- data.frame(year = survey_years,
                          cumulative_km2 = cumulative_area_km2,
                          new_km2 = c(cumulative_area_km2[1], diff(cumulative_area_km2)),
                          survey_km2 = annual_area_km2) |>
    dplyr::mutate(prop_new = new_km2/survey_km2)
  
  
  p_cumulative_footprint <- ggplot() +
    geom_point(data = new_areas,
               mapping = aes(x = year, y = cumulative_km2)) +
    geom_path(data = new_areas,
              mapping = aes(x = year, y = cumulative_km2)) +
    scale_y_continuous(name = expression('Cumulative footprint '~(km^2))) +
    scale_x_continuous(name = "Year") +
    theme_bw()
  
  p_new_footprint <- ggplot(data = dplyr::select(new_areas, year, survey_km2, new_km2) |>
                              tidyr::pivot_longer(cols = c("survey_km2", "new_km2")),
                            mapping = aes(x = year, 
                                          y = value,
                                          color = name)) +
    geom_point() +
    geom_path() +
    scale_y_continuous(name = expression(Area~(km^2))) +
    scale_x_continuous(name = "Year") +
    scale_color_colorblind(name = expression("Area")) +
    theme_bw()
  
  p_proportion_new <- ggplot(data = new_areas,
                             mapping = aes(x = year, y = prop_new)) +
    geom_point() +
    geom_path() +
    scale_y_continuous("Proportion new area swept", 
                       limits = c(0, 1)) +
    scale_x_continuous(name = "Year") +
    theme_bw()
  
  ragg::agg_png(here::here("analysis", "cumulative_footprint", "plots", paste0("cumulative_footprint_", unique_contact_type[ii], "_", "ts.png")), 
                width = 6, height = 4, units = "in", res = 300)
  print(p_cumulative_footprint)
  dev.off()
  
  ragg::agg_png(here::here("analysis", "cumulative_footprint", "plots", paste0("new_footprint_", unique_contact_type[ii], "_", "ts.png")), 
                width = 6, height = 4, units = "in", res = 300)
  print(p_new_footprint)
  dev.off()
  
  
  ragg::agg_png(here::here("analysis", "cumulative_footprint", "plots", paste0("proportion_new_footprint_", unique_contact_type[ii], "_", "ts.png")), 
                width = 6, height = 4, units = "in", res = 300)
  print(p_proportion_new)
  dev.off()
  
}


