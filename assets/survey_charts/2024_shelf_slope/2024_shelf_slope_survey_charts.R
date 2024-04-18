# Make 18x24 charts for 2024 shelf/slope tow comparison
# Created by Sean Rohan <sean.rohan@noaa.gov>
# Last update: April 18, 2024

library(akgfmaps)
library(ggspatial)
library(ggrepel)

dir.create(here::here("assets", "survey_charts", "2024_shelf_slope", "plots"))

slope_layers <- akgfmaps::get_base_layers(select.region = "ebs.slope", set.crs = "WGS84")
shelf_layers <- akgfmaps::get_base_layers(select.region = "sebs", set.crs = "WGS84")

project_areas <- sf::st_read(here::here("assets", "survey_charts", "2024_shelf_slope", "data", "2024_sample_zones_shelf_slope.shp"))
slope_allocation <- sf::st_read(here::here("assets", "survey_charts", "2024_shelf_slope", "data", "2024_slope_allocation.shp"))

skate_nursery <- readxl::read_xlsx(path = here::here("assets", "survey_charts", "2024_shelf_slope", "data", "Jerrys_Skate_nursery_sites_EBS.xlsx")) |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"), 
               crs = "WGS84") |>
  dplyr::mutate(label = "Skate Nursery (non-HAPC)") |>
  dplyr::select(label) |>
  sf::st_buffer(dist = 3000)

skate_hapc <- sf::st_read(here::here("assets", "survey_charts", "2024_shelf_slope", "data", "alaska_hapc.shp")) |>
  dplyr::filter(grepl(pattern = "Skate Nursery Areas", x = SITENAME_L)) |>
  sf::st_transform(crs = "WGS84") |>
  dplyr::mutate(label = "Skate Nursery (HAPC)")

project_areas <- dplyr::bind_rows(project_areas, skate_nursery, skate_hapc)

area_1 <- dplyr::filter(project_areas, label %in% c("Stratum 50", 
                                                    "Subarea 1", 
                                                    "Skate Nursery (non-HAPC)", 
                                                    "Skate Nursery (HAPC)"))
area_1_stations <- dplyr::filter(area_1, !is.na(STATIONID)) |>
  sf::st_centroid()
area_1_bbox <- area_1 |> 
  dplyr::filter(label %in% c("Stratum 50", "Subarea 1")) |>
  sf::st_bbox()

allocation <- read.csv(file = here::here("assets", "survey_charts", "2024_shelf_slope", "data", "slope_allocation.csv"))

allocation_points <- allocation |>
  sf::st_as_sf(coords = c("MEAN_LONGITUDE", "MEAN_LATITUDE"),
               crs = "WGS84")

dutch_harbor <- data.frame(x = -166.5417847, y = 53.8940888, label = "Dutch Harbor")

subarea1 <- ggplot() +
  geom_sf(data = slope_layers$akland) +
  geom_sf(data = shelf_layers$survey.grid, fill = NA) +
  geom_sf(data = slope_layers$survey.strata, fill = NA, color = "grey50") +
  geom_sf(data = area_1,
          mapping = aes(fill = label),
          alpha = 0.5) +
  geom_sf_text(data = area_1_stations,
               mapping = aes(label = STATIONID),
               size = rel(6.5)) +
  geom_sf(data = allocation_points,
          mapping = aes(shape = PRIMARY, color = PRIMARY),
          size = rel(4.5)) +
  geom_text_repel(data = allocation,
            mapping = aes(x = MEAN_LONGITUDE, y = MEAN_LATITUDE, label = STATIONID),
            size = rel(5.5)) +
  geom_point(data = dutch_harbor,
             mapping = aes(x = x, y = y),
             size = rel(5)) +
  geom_text(data = dutch_harbor,
             mapping = aes(x = x, y = y, label = label),
            hjust = -0.1,
            vjust = 0.1,
            size = rel(6)) +
  annotation_scale(text_cex = 2.3) +
  scale_fill_manual(name = "Area", values = c("Subarea 1" = "#9ECAE1", 
                                              "Stratum 50" = "#3182BD", 
                                              "Skate Nursery (non-HAPC)" = "pink", 
                                              "Skate Nursery (HAPC)" = "red")) +
  scale_color_manual(name = "Priority", values = c("Primary" = "#000000", "Alternate" = "#E69F00")) +
  ggtitle("Shelf/Slope Gear Comparison Project Sample Areas\nSubarea 1 and Stratum 50") +
  scale_shape(name = "Priority") +
  scale_x_continuous(limits = c(area_1_bbox['xmin'], area_1_bbox['xmax'])) +
  scale_y_continuous(limits = c(area_1_bbox['ymin']-0.4, area_1_bbox['ymax']+0.25)) +
  theme_bw() +
  theme(axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 30),
        legend.position = c(0.13, 0.1),
        legend.text = element_text(size = 24),
        legend.title = element_text(size = 24),
        axis.text = element_text(size = 24))

# ragg::agg_png(filename = here::here("assets", "survey_charts", "2024_shelf_slope","plots", "slope_subarea1_chart.png"), 
#               width = 18, 
#               height = 24, 
#               units = "in", 
#               res = 220)
# print(subarea1)
# dev.off()


pdf(file = here::here("assets", "survey_charts", "2024_shelf_slope","plots", "slope_subarea1_chart.pdf"), 
    width = 18, 
    height = 24)
print(subarea1)
dev.off()


# Subarea 6 ----


area_6 <- dplyr::filter(project_areas, label %in% c("Stratum 61", 
                                                    "Subarea 6", 
                                                    "Skate Nursery (non-HAPC)", 
                                                    "Skate Nursery (HAPC)"))
area_6_stations <- dplyr::filter(area_6, !is.na(STATIONID)) |>
  sf::st_centroid()
area_6_bbox <- area_6 |> 
  dplyr::filter(label  %in% c("Stratum 61", "Subarea 6")) |>
  sf::st_bbox()

subarea6 <- ggplot() +
  geom_sf(data = slope_layers$akland) +
  geom_sf(data = shelf_layers$survey.grid, fill = NA) +
  geom_sf(data = slope_layers$survey.strata, fill = NA, color = "grey50") +
  geom_sf(data = area_6,
          mapping = aes(fill = label),
          alpha = 0.5) +
  geom_sf_text(data = area_6_stations,
               mapping = aes(label = STATIONID),
               size = rel(6.5)) +
  geom_sf(data = allocation_points,
          mapping = aes(shape = PRIMARY, color = PRIMARY),
          size = rel(4.5)) +
  geom_text_repel(data = allocation,
                  mapping = aes(x = MEAN_LONGITUDE, y = MEAN_LATITUDE, label = STATIONID),
                  size = rel(5.5)) +
  annotation_scale(text_cex = 2.3) +
  scale_fill_manual(name = "Area", values = c("Subarea 6" = "#BCBDDC", 
                                              "Stratum 61" = "#756BB1", 
                                              "Skate Nursery (non-HAPC)" = "pink", 
                                              "Skate Nursery (HAPC)" = "red")) +
  scale_color_manual(name = "Priority", values = c("Primary" = "#000000", "Alternate" = "#E69F00")) +
  ggtitle("Shelf/Slope Gear Comparison Project Sample Areas\nSubarea 6 and Stratum 61") +
  scale_shape(name = "Priority") +
  scale_x_continuous(limits = c(area_6_bbox['xmin']-0.2, area_6_bbox['xmax'])) +
  scale_y_continuous(limits = c(area_6_bbox['ymin'], area_6_bbox['ymax'])) +
  theme_bw() +
  theme(axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 30),
        legend.position = c(0.18, 0.1),
        legend.text = element_text(size = 24),
        legend.title = element_text(size = 24),
        axis.text = element_text(size = 24))

ragg::agg_png(filename = here::here("assets", "survey_charts", "2024_shelf_slope","plots", "slope_subarea6_chart.png"), 
              width = 18, 
              height = 24, 
              units = "in", 
              res = 220)
print(subarea6)
dev.off()


pdf(file = here::here("assets", "survey_charts", "2024_shelf_slope","plots", "slope_subarea6_chart.pdf"), 
    width = 18, 
    height = 24)
print(subarea6)
dev.off()
