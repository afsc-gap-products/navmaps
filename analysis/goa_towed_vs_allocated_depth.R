# Compare tow depth distribution by year and allocated vs. towed

library(navmaps)
library(ggthemes)

channel <- navmaps::get_connected(schema = "AFSC")


# Tow depth
goa_hauls <- 
  RODBC::sqlQuery(
    channel = channel,
    query = "select h.*, c.year from 
    gap_products.akfin_haul h, 
    gap_products.akfin_cruise c 
    where c.survey_definition_id = 47 
    and c.cruisejoin = h.cruisejoin"
  )

ggplot() +
  stat_ecdf(data = 
              dplyr::filter(goa_hauls, YEAR != 2001),
            mapping = 
              aes(x = DEPTH_M, group = factor(YEAR), color = YEAR, linewidth = YEAR == 2025)) +
  scale_x_continuous(name = "Bottom depth (m)") +
  scale_y_continuous(name = "Cumulative proportion") +
  scale_color_viridis_c(option = "H") +
  scale_linewidth_manual(values = c(rel(0.5), rel(1.2)), guide = "none") +
  ggtitle("Cumulative distribution of tow bottom depth in the GOA (excluding 2001)") +
  scale_y_continuous(name = "Cumulative proportion") +
  theme_bw()


ggplot() +
  stat_ecdf(data = 
              dplyr::filter(goa_hauls, YEAR != 2001, DEPTH_M < 700),
            mapping = 
              aes(x = DEPTH_M, group = factor(YEAR), color = YEAR, linewidth = YEAR == 2025)) +
  scale_color_viridis_c(option = "H") +
  scale_linewidth_manual(values = c(rel(0.5), rel(1.2)), guide = "none") +
  scale_x_continuous(name = "Bottom depth (m)") +
  scale_y_continuous(name = "Cumulative proportion") +
  ggtitle("Cumulative distribution of tow bottom depth in the GOA (excluding 2001 and depth > 700 m)") +
  theme_bw()


prop_geq_200m <- goa_hauls |>
  dplyr::filter(DEPTH_M < 700) |>
  dplyr::group_by(YEAR) |>
  dplyr::summarise(N_GEQ_200 = sum(DEPTH_M >= 200),
                   N = n()
  ) |>
  dplyr::mutate(PCT_GEQ_200 = N_GEQ_200/N*100)


ggplot() +
  geom_bar(data = prop_geq_200m,
           mapping = aes(x = YEAR, y = PCT_GEQ_200, group = YEAR),
           stat = "identity") + 
  scale_y_continuous(name = "Hauls 200-700 m (%)") +
  scale_x_continuous(name = "Year") +
  ggtitle("Proportion of GOA hauls at 200 to 700 m") +
  theme_bw()

ggplot() +
  geom_bar(data = goa_hauls |>
             dplyr::group_by(YEAR) |>
             dplyr::summarise(N = n()
             ),
           mapping = aes(x = YEAR, y = N, group = YEAR),
           stat = "identity") + 
  scale_y_continuous(name = "# Hauls") +
  scale_x_continuous(name = "Year") +
  ggtitle("Total number of GOA hauls") +
  theme_bw()


# Allocation versus realized depth

goa_1990_design <- akgfmaps::get_base_layers(select.region = "goa", design.year = 1990, set.crs = "EPSG:3338")

goa_2025_design <- akgfmaps::get_base_layers(select.region = "goa", design.year = 2025, set.crs = "EPSG:3338")

# Can use Mark's bathymetry instead
bathy_1km <- 
  terra::rast(
    system.file("extdata", "bathymetry.gpkg", package = "akgfmaps")
  )

allocation <-
  read.csv(file = here::here("assets", "data", "allocation", "GOA 2021 stations for Mark.csv")) |>
  dplyr::mutate(year = 2021) |>
  dplyr::rename(STATION = stationid,
                STRATUM = stratum) |>
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = "WGS84") |>
  sf::st_transform(crs = "EPSG:3338") |>
  dplyr::bind_rows(
    read.csv(file = here::here("assets", "data", "allocation", "GOA2023_Station_allocation_520_EW.csv")) |>
      dplyr::mutate(year = 2023) |>
      dplyr::rename(STATION = id,
                    STRATUM = stratum) |>
      sf::st_as_sf(coords = c("longitude", "latitude"), crs = "WGS84") |>
      sf::st_transform(crs = "EPSG:3338"),
    sf::st_read(here::here("assets", "data", "allocation", "goa_2025_station_allocation_450_aea.gpkg")) |>
      dplyr::mutate(year = 2025) |>
      sf::st_set_geometry("geometry")) |>
  dplyr::select(STATION, STRATUM, YEAR = year)

allocation$type <- "Allocated"

# Could make line segments instead of using start lat/lon. Then use use navmaps::st_line_midpoints() to calculate the tow midpoint.

towed <- goa_hauls |>
  dplyr::filter(YEAR >= 2021) |>
  sf::st_as_sf(coords = c("LONGITUDE_DD_START", "LATITUDE_DD_START"), crs = "WGS84") |>
  sf::st_transform(crs = "EPSG:3338")

towed$type <- "Towed"

# Use depths estimated from the bathymetry grid to calculate tow depths

towed_vs_allocated <- 
  dplyr::bind_rows(allocation,
                   towed)

towed_vs_allocated$GRID_DEPTH <-
  terra::extract(
    bathy_1km,
    y = sf::st_coordinates(towed_vs_allocated),
    method = "bilinear"
  )[,1]


ggplot() +
  geom_sf(data = goa_1990_design$survey.area) +
  geom_sf(data = dplyr::filter(towed_vs_allocated, is.na(GRID_DEPTH)),
          mapping = aes(color = factor(YEAR), shape = type)) +
  facet_wrap(~type, nrow = 2)

ggplot() +
  stat_ecdf(
    data = towed_vs_allocated,
    mapping = aes(x = GRID_DEPTH, color = type)
  ) +
  scale_color_colorblind() +
  facet_wrap(~YEAR) +
  theme_bw()

ggplot() +
  stat_ecdf(
    data = towed_vs_allocated,
    mapping = aes(x = GRID_DEPTH, color = factor(YEAR))
  ) +
  scale_color_colorblind() +
  facet_wrap(~type) +
  theme_bw()
