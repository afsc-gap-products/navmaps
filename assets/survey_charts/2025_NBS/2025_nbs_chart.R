library(akgfmaps)
library(shadowtext)

map_crs <- "EPSG:3338"

dir.create(here::here("assets", "survey_charts", "2025_NBS"))

# Setup for two charts: one 42x34, one 17x11
chart_width_in <- c(42, 17)
chart_height_in <- c(34, 11)
chart_scale <- c(3, 1)

# Create location labels
landmark_label <- data.frame(label = c("St. Lawrence", "Nunivak", "Nome"),
                             x = c(-170.27, -166.4, -165.05),
                             y = c(63.4, 60.08, 64.65),
                             rel_size = 5) |>
  transform_data_frame_crs(in.crs = "EPSG:4269",
                           out.crs = map_crs)

nome_pin <- data.frame(label = "Nome", 
                       x = -165.4,
                       y = 64.504,
                       rel_size = 4) |>
  sf::st_as_sf(coords = c("x", "y"),
               crs = "EPSG:4269") |>
  sf::st_transform(crs = map_crs)

alaska_label <- data.frame(x = -162.5, 
                           y = 61.75,
                           label = "Alaska",
                           rel_size = 7) |>
  transform_data_frame_crs(in.crs = "EPSG:4269",
                           out.crs = map_crs)

# Setup chart title
title_label <- data.frame(label = "Northern Bering Sea\nContinental Shelf Bottom Trawl Survey of\nGroundfish and Invertebrate Resources",
                          x = -175, 
                          y = 65,
                          rel_size = 7) |>
  transform_data_frame_crs(in.crs = "EPSG:4269",
                           out.crs = map_crs)

# Load regional vector features
nbs_layers <- akgfmaps::get_base_layers(select.region = "nbs",
                                        set.crs = map_crs,
                                        include.corners = FALSE)

# Load spectacled Eider critical habitata file
spectacled_eider_ch <- sf::st_read(here::here("assets", "data", "spectacled_eider", "FCH_Somateria_fischeri_20010206.shp")) |>
  dplyr::filter(Unit_ID %in% c("3 - Norton Sound")) |>
  sf::st_transform(crs = map_crs) |>
  dplyr::mutate(name = "Spectacled Eider Crit. Hab.",
                rel_size = 4)

# Get station centroids for label locations
nbs_centroid <- sf::st_centroid(nbs_layers$survey.grid)

# Load 2025 spectacled eider habitat benthic grab sample table
spectacled_eider_sampling <- 
  read.csv(file = here::here("assets", "data", "special_projects", "NBS", "2025", "benthic_sampling_stations.csv")) |>
  dplyr::mutate(Priority = ifelse(Priority == "high", "Hi", "Md")) |>
  dplyr::mutate(STATION = Station.ID, station_label = paste0("Ben. grab (", Priority, ")")) |>
  dplyr::select(-Station.ID, -Priority)

# Add spetacled eider sampling stations to centroids
nbs_centroid <- dplyr::full_join(nbs_centroid, spectacled_eider_sampling) |>
  dplyr::mutate(station_label = ifelse(is.na(station_label), "Index Station", station_label))

# Create station row and column labels
outside_grid <- sf::st_read(system.file("/extdata/bs_grid.shp", package = "akgfmaps")) |>
  dplyr::rename(STATION = STATIONID)

col_stations <- c(paste0("V-", 25:27), 
                  paste0("S-", 22:24),
                  paste0("Q-", 18:21),
                  paste0("Q-0", 1:3),
                  "X-04",
                  "Y-05",
                  paste0("ZZ-0", 6:8),
                  "AA-09",
                  "AA-10"
                  )

row_stations <- c("FF-19", "EE-20", "DD-21", "CC-22", "BB-23", "AA-24", "ZZ-25", "Y-26", "X-27", "W-28")

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

for(ii in 1:length(chart_width_in)) {
  
  nbs_chart <-
    ggplot() +
    geom_sf(data = nbs_layers$survey.grid, fill = NA) +
    geom_sf(data = nbs_layers$survey.strata, 
            fill = NA,
            color = "grey30") +
    geom_sf(data = nbs_layers$akland,
            fill = "white",
            color = "black") +
    geom_sf(data = nbs_centroid, 
            mapping = aes(shape = station_label, color = station_label),
            size = rel(3*chart_scale[ii])) +
    geom_text(data = alaska_label,
              mapping = aes(x = x, y = y, label = label),
              size = rel(alaska_label$rel_size*chart_scale[ii])) +
    geom_shadowtext(data = landmark_label,
                    mapping = aes(x = x, y = y, label = label),
                    size = rel(landmark_label$rel_size*chart_scale[ii]),
                    color = "black", 
                    bg.color = "white") +
    geom_shadowtext(data = title_label,
                    mapping = aes(x = x, y = y, label = label),
                    size = rel(title_label$rel_size*chart_scale[ii]),
                    fontface = "bold",
                    color = "black", 
                    bg.color = "white") +
    geom_shadowtext(data = grid_labels,
                    mapping = aes(x = X, y = Y, label = label),
                    color = "black", 
                    bg.color = "white",
                    fontface = "bold",
                    size = rel(5*chart_scale[ii])) +
    geom_sf(data = nome_pin,
            shape = 8,
            color = "limegreen",
            size = rel(nome_pin$rel_size*chart_scale[ii])) +
    scale_x_continuous(limits = nbs_layers$plot.boundary$x + c(-5e4, 5e4),
                       breaks = nbs_layers$lon.breaks) +
    scale_y_continuous(limits = nbs_layers$plot.boundary$y + c(-5e4, 5e4),
                       breaks = nbs_layers$lat.breaks) +
    scale_shape_manual(
      values = c("Index Station" = 19, "Ben. grab (Hi)" = 17, "Ben. grab (Md)" = 17)
      ) +
    scale_color_manual(
      values = c("Index Station" = "black", "Ben. grab (Hi)" = "magenta", "Ben. grab (Md)" = "cyan")
      ) +
    theme_bw() +
    theme(axis.title = element_blank(),
          legend.title = element_blank(),
          legend.position = c(0.13, 0.09),
          axis.text = element_text(size = 18*chart_scale[ii]),
          legend.text = element_text(size = 22*chart_scale[ii]),
          legend.key.spacing.y = unit(0.1*chart_scale[ii], "in"),
          legend.key.size = unit(0.3*chart_scale[ii], "in"))
  
  nbs_ch_chart <-
    ggplot() +
    geom_sf(data = nbs_layers$survey.grid, fill = NA) +
    geom_sf(data = nbs_layers$survey.strata, 
            fill = NA,
            color = "grey30") +
    geom_sf(data = nbs_layers$akland,
            fill = "white",
            color = "black") +
    geom_sf(data = spectacled_eider_ch,
            mapping = aes(fill = name),
            alpha = 0.5,
            color = NA) +
    geom_sf(data = nbs_centroid, 
            mapping = aes(shape = station_label, color = station_label),
            size = rel(3*chart_scale[ii])) +
    geom_shadowtext(data = landmark_label,
                    mapping = aes(x = x, y = y, label = label),
                    size = rel(landmark_label$rel_size*chart_scale[ii]),
                    color = "black", 
                    bg.color = "white") +
    geom_text(data = alaska_label,
              mapping = aes(x = x, y = y, label = label),
              size = rel(alaska_label$rel_size*chart_scale[ii])) +
    geom_shadowtext(data = title_label,
                    mapping = aes(x = x, y = y, label = label),
                    size = rel(title_label$rel_size*chart_scale[ii]),
                    fontface = "bold",
                    color = "black", 
                    bg.color = "white") +
    geom_shadowtext(data = grid_labels,
                    mapping = aes(x = X, y = Y, label = label),
                    color = "black", 
                    bg.color = "white",
                    fontface = "bold",
                    size = rel(5*chart_scale[ii])) +
    geom_sf(data = nome_pin,
            shape = 8,
            color = "limegreen",
            size = rel(nome_pin$rel_size*chart_scale[ii])) +
    scale_x_continuous(limits = nbs_layers$plot.boundary$x + c(-5e4, 5e4),
                       breaks = nbs_layers$lon.breaks) +
    scale_y_continuous(limits = nbs_layers$plot.boundary$y + c(-5e4, 5e4),
                       breaks = nbs_layers$lat.breaks) +
    scale_shape_manual(
      values = c("Index Station" = 19, "Ben. grab (Hi)" = 17, "Ben. grab (Md)" = 17)
    ) +
    scale_color_manual(
      values = c("Index Station" = "black", "Ben. grab (Hi)" = "magenta", "Ben. grab (Md)" = "cyan")
    ) +
    scale_fill_manual(values = "red") +
    theme_bw() +
    theme(axis.title = element_blank(),
          legend.title = element_blank(),
          legend.position = c(0.17, 0.11),
          axis.text = element_text(size = 18*chart_scale[ii]),
          legend.text = element_text(size = 22*chart_scale[ii]),
          legend.key.spacing.y = unit(0.1*chart_scale[ii], "in"),
          legend.key.size = unit(0.3*chart_scale[ii], "in"))
  
  pdf(file = here::here("assets", 
                        "survey_charts", 
                        "2025_NBS", 
                        paste0("2025_nbs_chart_", chart_width_in[ii], "_", chart_height_in[ii], ".pdf")), 
      width = chart_width_in[ii], 
      height = chart_height_in[ii])
  print(nbs_chart)
  dev.off()
  
  pdf(file = here::here("assets", 
                        "survey_charts", 
                        "2025_NBS", 
                        paste0("2025_nbs_chart_ch_", chart_width_in[ii], "_", chart_height_in[ii], ".pdf")), 
      width = chart_width_in[ii], 
      height = chart_height_in[ii])
  print(nbs_ch_chart)
  dev.off()
  
}
