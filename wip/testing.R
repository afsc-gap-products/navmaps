map_layers <- akgfmaps::get_base_layers(select.region = "sebs")

bathy_layer <- map_layers$bathymetry
bathy_layer$color <- sample(x = 1:11, 
                            size = nrow(bathy_layer ), 
                            replace = TRUE)

sf_to_gpx_track(
  x = bathy_layer,
  name_col = "METERS",
  description_col = "LAST_UPDAT",
  color_col = "color",
  file = "test_lines.gpx")


sf::st_write(bathy_layer, "test.shp")

library(navmaps)

map_layers <- akgfmaps::get_base_layers(select.region = "goa")
strata <- map_layers$survey.strata
strata$color <- sample(1:12, nrow(strata), replace = TRUE)
strata$fill <- 0

start3 <- Sys.time()
sf_to_kml_polygon3(x = strata,
                   name_col = "STRATUM",
                   description_col = "CONTACT",
                   color_col = "color",
                   fill_col = "fill",
                   file = here::here("output", "test_polygon3.kml"))
end3 <- Sys.time() 

start1 <- Sys.time()
sf_to_kml_polygon(x = strata,
                  name_col = "STRATUM",
                  description_col = "CONTACT",
                  color_col = "color",
                  fill_col = "fill",
                  file = here::here("output", "test_polygon.kml"))
end1 <- Sys.time() 

start2 <- Sys.time()
sf_to_kml_polygon2(x = strata,
                  name_col = "STRATUM",
                  description_col = "CONTACT",
                  color_col = "color",
                  fill_col = "fill",
                  file = here::here("output", "test_polygon2.kml"))
end2 <- Sys.time() 

end1 - start1
end2 - start2
end3 - start3


sf_to_gpx_track(
  x = strata,
  name_col = "Stratum",
  description_col = "SURVEY",
  file = "test_polygon.gpx")

library(navmaps)

start <- Sys.time()
make_trawlable(region = "goa")
end <- Sys.time()
print(end-start)

test <- sf::st_read("C:/Users/sean.rohan/Work/afsc/navmaps/output/ai/navigation/ai_trawlwable.kml")

ggplot() +
  geom_sf(data = test |>
            sf::st_transform(crs = "EPSG:3338"),
          mapping = aes(fill = Description)) +
  theme(legend.position = "none")

# Test allocation function
make_station_allocation(
  allocation_df = read.csv(file = here::here("data", "allocation", "AIallocation420.csv")) |>
    tidyr::drop_na(Longitude, Latitude),
  lon_col = "Longitude",
  lat_col = "Latitude",
  region = "ai",
  survey_year = NULL,
  station_col = "stationid",
  stratum_col = "stratum",
  vessel_col = "vessel",
  extra_col = "Priority",
  file_format = "gpx",
  software_format = "timezero"
)
