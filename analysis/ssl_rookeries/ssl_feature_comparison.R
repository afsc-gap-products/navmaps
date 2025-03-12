library(navmaps)
library(cowplot)

goa_ai_layers <- akgfmaps::get_base_layers(select.region = c("ai", "goa"), set.crs = "EPSG:3338")

all_rookeries_for_wayne <- sf::st_read(here::here("assets", "data", "SSLrookeries", "All_rookeries_for_Wayne.shp")) |>
  sf::st_transform(crs = "EPSG:3338") |>
  sf::st_buffer(dist = 5500)


no_transit_3nm <- sf::st_read(here::here("assets", "data", "SSLrookeries", "3nm_No_Transit.shp")) |>
  sf::st_transform(crs = "EPSG:3338")

habitat_restrictions <- sf::st_read(here::here("analysis", "ssl_rookeries", "Habitat_Restrictions.shp")) |>
  sf::st_transform(crs = "EPSG:3338") |>
  dplyr::filter(HTM == "3nm_No_Transit.htm") |>
  sf::st_cast("POLYGON")

sweeney <- read.csv(here::here("analysis", "ssl_rookeries", "NCEI-0129877_US_SSL_Sites.csv")) |>
  dplyr::filter(REGNO >= 5, ROOKERY == 1) |>
  sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"),
                   crs = "WGS84") |>
  sf::st_transform(crs = "EPSG:3338") |>
  sf::st_buffer(dist = 5500)

p1 <- 
  ggplot() +
  geom_sf(data = goa_ai_layers$akland, alpha = 0.4, color = NA) +
  geom_sf(data = no_transit_3nm ,
          fill = "red", 
          color = "red") +
  scale_x_continuous(limits = goa_ai_layers$plot.boundary$x, 
                     breaks = goa_ai_layers$lon.breaks) +
  scale_y_continuous(limits = goa_ai_layers$plot.boundary$y, 
                     breaks = goa_ai_layers$lon.breaks) +
  theme_bw() +
  facet_wrap(~"3nm_No_Transit.shp")

p2 <- 
  ggplot() +
  geom_sf(data = goa_ai_layers$akland, alpha = 0.4, color = NA) +
  geom_sf(data = all_rookeries_for_wayne,
          fill = "red",
          color = "red") +
  scale_x_continuous(limits = goa_ai_layers$plot.boundary$x, 
                     breaks = goa_ai_layers$lon.breaks) +
  scale_y_continuous(limits = goa_ai_layers$plot.boundary$y, 
                     breaks = goa_ai_layers$lon.breaks) +
  theme_bw() +
  facet_wrap(~"All_rookeries_for_Wayne.shp")


p3 <- 
  ggplot() +
  geom_sf(data = goa_ai_layers$akland, alpha = 0.4, color = NA) +
  geom_sf(data = habitat_restrictions,
          fill = "red",
          color = "red") +
  scale_x_continuous(limits = goa_ai_layers$plot.boundary$x, 
                     breaks = goa_ai_layers$lon.breaks) +
  scale_y_continuous(limits = goa_ai_layers$plot.boundary$y, 
                     breaks = goa_ai_layers$lon.breaks) +
  theme_bw() +
  facet_wrap(~"AKRO Habitat_Restrictions.shp")

p4 <- 
  ggplot() +
  geom_sf(data = goa_ai_layers$akland, alpha = 0.4, color = NA) +
  geom_sf(data = sweeney,
          fill = "red",
          color = "red") +
  scale_x_continuous(limits = goa_ai_layers$plot.boundary$x, 
                     breaks = goa_ai_layers$lon.breaks) +
  scale_y_continuous(limits = goa_ai_layers$plot.boundary$y, 
                     breaks = goa_ai_layers$lon.breaks) +
  theme_bw() +
  facet_wrap(~"MML 2016 (Sweeney)")

plot_grid(p1, p2, p3, p4, nrow = 4)
