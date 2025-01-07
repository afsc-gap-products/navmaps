library(akgfmaps)
library(shadowtext)

map_crs <- "EPSG:3338"

# Setup for two charts: one 42x34, one 17x11
chart_width_in <- 42
chart_height_in <- 34
chart_scale <- 3

island_label <- data.frame(label = c("St. Lawrence", "Nunivak"),
                           x = c(-170.27, -166.4),
                           y = c(63.4, 60.08),
                           rel_size = 4) |>
  transform_data_frame_crs(in.crs = "EPSG:4269",
                           out.crs = map_crs)

alaska_label <- data.frame(x = -159, 
                           y = 61.75,
                           label = "Alaska",
                           rel_size = 7) |>
  transform_data_frame_crs(in.crs = "EPSG:4269",
                           out.crs = map_crs)

title_label <- data.frame(label = "Eastern Bering Sea\nContinental Shelf Bottom Trawl Survey of\nGroundfish and Invertebrate Resources",
                          x = -171.5, 
                          y = 53.3,
                          rel_size = 7) |>
  transform_data_frame_crs(in.crs = "EPSG:4269",
                           out.crs = map_crs)


# Setup high density pollock stations
high_pollock_stations <- c("AZ0504", "Z-05", 
                           paste0("A-0", 2:6),
                           paste0("B-0", 1:9),
                           paste0("C-0", 1:9), "C-18",
                           paste0("D-0", 1:9), "D-10", "D-18",
                           paste0("E-0", 1:9), paste0("E-", c(10:12, 18:22)),
                           paste0("F-0", 1:9), paste0("F-", c(10:14, 18:25)),
                           paste0("G-0", 1:5), paste0("G-", 18:26),
                           paste0("H-0", 1:2), paste0("H-", 18:26),
                           paste0("I-", 18:26),
                           paste0("J-", 18:26),
                           paste0("K-", 21:27),
                           paste0("L-", 21:31),
                           paste0("M-", 21:32),
                           paste0("N-", 22:31),
                           paste0("O-", 22:31),
                           paste0("P-", 25:32),
                           paste0("Q-", 25:31),
                           paste0("R-", 25:32),
                           paste0("S-", 25:31),
                           paste0("T-", 25:30),
                           paste0("U-", 25:29),
                           paste0("V-", 25:28))

ebs_layers <- akgfmaps::get_base_layers(select.region = "sebs",
                                        set.crs = map_crs,
                                        include.corners = FALSE)

ebs_layers$survey.grid$otoliths <- ifelse(ebs_layers$survey.grid$STATION %in% high_pollock_stations, "Pollock high (5)", "Pollock low (3)")

ebs_centroid <- sf::st_centroid(ebs_layers$survey.grid) |>
  dplyr::mutate(station_label = "Index Station")

outside_grid <- sf::st_read(system.file("/extdata/bs_grid.shp", package = "akgfmaps")) |>
  dplyr::rename(STATION = STATIONID) # |>
#   dplyr::filter((!STATIONID %in% c(ebs_layers$survey.grid$STATIONID, "Z-04")))
# 
# outside_grid$touches <- apply(
#   X = sf::st_is_within_distance(outside_grid, 
#                                 ebs_layers$survey.grid, 
#                                 dist = 1000, 
#                                 sparse = FALSE), 
#   MARGIN = 1, 
#   FUN = any)
# 
# outside_grid <- dplyr::filter(outside_grid, touches)

col_stations <- c("W-28", "W-27", "W-26", "W-25", "T-24", "T-23", "T-22", "R-21", "R-20", "R-19", "R-18", "R-01", 
                  "R-02", "R-03", "O-05", "O-06", "O-07", "N-08", "M-09", "L-10", "L-11", "L-12", "L-13", "L-14", 
                  "K-15", "K-16")

row_stations <- c("V-29", "U-30", "T-31", "S-32", "R-33", "Q-33", "P-33", "O-33", "N-33", "M-33", "L-32",
                  "K-28", "J-27", "I-27", "H-27", "G-27", "F-26", "E-23", "D-19", "C-19", "B-18", "A-01",
                  "Z-03")

col_grid <- dplyr::filter(outside_grid, 
                          STATION %in% col_stations) |>
  dplyr::mutate(label = gsub(pattern = "[^0-9]", replacement = "", x = STATION))

col_grid_labels <- col_grid |> 
  sf::st_drop_geometry() |>
  dplyr::bind_cols(
    as.data.frame(
      sf::st_coordinates(sf::st_centroid(col_grid))
    )
  )

row_grid <- dplyr::filter(outside_grid, 
                          STATION %in% row_stations) |>
  dplyr::mutate(label = gsub(pattern = "[^[:alpha:]]", replacement = "", x = STATION))

row_grid_labels <- row_grid |> 
  sf::st_drop_geometry() |>
  dplyr::bind_cols(
    as.data.frame(
      sf::st_coordinates(sf::st_centroid(row_grid))
    )
  )

grid_labels <- dplyr::bind_rows(row_grid_labels, col_grid_labels)

ebs_chart <- ggplot() +
  geom_sf(data = ebs_layers$survey.grid, 
          mapping = aes(fill = otoliths)) +
  geom_sf(data = ebs_layers$survey.strata, 
          fill = NA,
          color = "grey50") +
  geom_sf(data = col_grid, fill = NA) +
  geom_sf(data = row_grid, fill = NA) +
  geom_sf(data = ebs_layers$akland, 
          fill = "white", 
          color = "black") +
  geom_text(data = alaska_label,
            mapping = aes(x = x, y = y, label = label),
            size = rel(alaska_label$rel_size*chart_scale)) +
  geom_shadowtext(data = island_label,
                  mapping = aes(x = x, y = y, label = label),
                  size = rel(island_label$rel_size*chart_scale),
                  color = "black", 
                  bg.color = "white") +
  geom_sf(data = ebs_centroid, 
          mapping = aes(shape = station_label),
          size = rel(3*chart_scale)) +
  geom_shadowtext(data = title_label,
                  mapping = aes(x = x, y = y, label = label),
                  size = rel(title_label$rel_size*chart_scale),
                  fontface = "bold",
                  color = "black", 
                  bg.color = "white") +
  geom_shadowtext(data = grid_labels,
                  mapping = aes(x = X, y = Y, label = label),
                  color = "black", 
                  bg.color = "white",
                  fontface = "bold",
                  size = rel(4*chart_scale)) +
  scale_x_continuous(limits = ebs_layers$plot.boundary$x + c(-5e4, 5e4),
                     breaks = ebs_layers$lon.breaks) +
  scale_y_continuous(limits = ebs_layers$plot.boundary$y + c(-5e4, 5e4),
                     breaks = ebs_layers$lat.breaks) +
  scale_fill_manual(values = c("Pollock high (5)" = "grey85", "Pollock low (3)" = "white")) +
  theme_bw() +
  theme(axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.87, 0.07),
        axis.text = element_text(size = 18*chart_scale),
        legend.text = element_text(size = 22*chart_scale),
        legend.key.spacing.y = unit(0.1*chart_scale, "in"),
        legend.key.size = unit(0.3*chart_scale, "in"))

pdf(file = here::here("assets", 
                      "survey_charts", 
                      "2025_EBS", 
                      paste0("2025_ebs_chart_", chart_width_in, "_", chart_height_in, ".pdf")), 
    width = chart_width_in, 
    height = chart_height_in)
print(ebs_chart)
dev.off()
