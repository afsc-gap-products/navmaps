library(navmaps)
library(cowplot)
library(ggplot2)

goa_ai_layers <- akgfmaps::get_base_layers(select.region = c("ai", "goa"), set.crs = "EPSG:3338")

all_rookeries_for_wayne <- 
  here::here("assets", "data", "SSLrookeries", "All_rookeries_for_Wayne.shp") |> 
  sf::st_read() |>
  sf::st_transform(crs = "EPSG:3338") |>
  sf::st_buffer(dist = 5500)

# navmaps file
no_transit_3nm <- 
  here::here("assets", "data", "SSLrookeries", "3nm_No_Transit.shp") |> 
  sf::st_read() |>
  sf::st_transform(crs = "EPSG:3338")

# File from Kelly Cates (2022)
no_transit_3nm_kelly_cates <- 
  here::here("analysis", "ssl_rookeries", "3nm_noTransit_cates", "3nm_No_Transit.shp") |>
  sf::st_read() |>
  sf::st_transform(crs = "EPSG:3338")

# File from MACE (used Nov. 2024)
no_transit_3nm_mace <- 
  here::here("analysis", "ssl_rookeries", "3nm_no_transit_mace", "3nm_No_Transit.shp") |>
  sf::st_read() |>
  sf::st_transform(crs = "EPSG:3338")

identical(no_transit_3nm, no_transit_3nm_kelly_cates)
identical(no_transit_3nm, no_transit_3nm_mace)

# MACE critical habitat from 2024
ch_mace <- 
  here::here("analysis", "ssl_rookeries", "mace_critical_habitat_nov2024", "StellarSeaLionCriticalHabitat.shp") |>
  sf::st_read() |>
  sf::st_transform(crs = "EPSG:3338") |>
  sf::st_cast("POLYGON")

# navmaps critical habitat
ch_navmaps <- here::here("assets", "data", "SSLcriticalhabitat", "Crit_Hab_Coded_WDPS.shp") |> 
  sf::st_read() |>
  sf::st_transform(crs = "EPSG:3338")

# Sweeney rookeries 2025
sweeney_rookeries_2025 <- 
  here::here("analysis", "ssl_rookeries", "AK_SSL_sites_abbrev.xlsx") |>
  readxl::read_xlsx() |>
  dplyr::filter(ROOK == 1) |>
  sf::st_as_sf(crs = "WGS84",
               coords = c("LON", "LAT"))

sweeney_rookeries_2025 <- 
  here::here("analysis", "ssl_rookeries", "AK_SSL_sites_abbrev.xlsx") |>
  readxl::read_xlsx() |>
  dplyr::filter(ROOK == 1) |>
  sf::st_as_sf(crs = "WGS84",
               coords = c("LON", "LAT"))

dir.create(here::here("analysis", "ssl_rookeries", "AK_SSL_sites_abbrev"))

sf::st_write(obj = sweeney_rookeries_2025,
             dsn = here::here("analysis", "ssl_rookeries", "AK_SSL_sites_abbrev", "AK_SSL_sites_abbrev.shp"))


ggplot() +
  geom_sf(data = goa_ai_layers$akland, fill = "yellow", color = NA) +
  geom_sf(data = ch_navmaps, fill = "blue", alpha = 0.5) +
  geom_sf(data = ch_mace, fill = "red", alpha = 0.5) +
  scale_x_continuous(limits = goa_ai_layers$plot.boundary$x, 
                     breaks = goa_ai_layers$lon.breaks) +
  scale_y_continuous(limits = goa_ai_layers$plot.boundary$y, 
                     breaks = goa_ai_layers$lon.breaks)

# MACE critical habitat layer from 2024
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
