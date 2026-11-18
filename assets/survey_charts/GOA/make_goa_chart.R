library(navmaps)
library(ggnewscale)

# GOA Chart

crs_aea <- 3338

# Settings: Colors and fill ----

akland_color <- "grey70"
akland_fill <- "#FFEABE"

vessel_codes_names <- 
  data.frame(
    VESSEL = c(148, 176),
    VESSEL_NAME = c("Ocean Explorer", "Alaska Provider")
             )


allocation_color = "black"
allocation_fill <- c("blue", "red")
allocation_shape <- c(21, 24)
names(allocation_fill) <- vessel_codes_names$VESSEL_NAME
names(allocation_shape) <- vessel_codes_names$VESSEL_NAME


wrecks_color <- c("Wrecks and obstructions" = "#FF48DC")
wrecks_shape <- 11

ch_sea_otter_color <- "grey20"
ch_sea_otter_fill <- c("Sea Otters" =  "#D86ECC")
ch_sea_otter_alpha <- 0.3

ch_nprw_color <- "grey20"
ch_nprw_fill <- c("N. Pac. Right Whales" = "#46B1E1")
ch_nprw_alpha <- 0.3

ch_humpback_color <- "grey70"
ch_humpback_fill <- c("Humpback Whales" = "grey70")
ch_humpback_alpha <- 0.3

hazards_color <- c("Buoys/moorings" = "grey20")
hazards_shape <- 10

performance_color <- c(
  "Good tow" = "green2", 
  "Unsatisfactory" = "red", 
  "Other problems, satisfactory" = "cyan"
)

trawlable_color <- "grey20"
trawlable_fill <- c("Unknown grid cell" = NA, "Trawlable grid cell" = "lightgreen", "Untrawlable grid cell" = "salmon")
trawlable_alpha <- 0.5

ssl_notransit_color <- "magenta"
ssl_notransit_fill <- NA

crabpot_colors <- c("Crabpot storage" = "red")

graticule_color <- "#197DFF"

region_boundary_color <- "red"


# Functions ----

read_transform <- function(fpath, crs = 3338) {
  
  sf::st_read(fpath) |>
    sf::st_transform(crs)
  
}

map_layers <- akgfmaps::get_base_layers(select.region = "goa", set.crs = crs_aea)

hazards <- read_transform(here::here("output", "goa", "shapefiles", "hazards_buoys_20250121.gpkg"))

allocation <- read_transform(here::here("output", "goa", "shapefiles", "goa_station_allocation.shp")) |>
  dplyr::inner_join(
    vessel_codes_names,
    by = "VESSEL"
  )

towpath <- read_transform(here::here("output", "goa", "shapefiles", "goa_towpath.shp")) |>
  dplyr::mutate(
    PERF_CAT = 
      dplyr::case_when(
        PERFORM < 0 ~ "Unsatisfactory",
        PERFORM >= 0 & PERFORM <= 4.6 ~ "Good tow",
        PERFORM > 4.6 ~ "Other problems, satisfactory"
      ),
    PERF_CAT = factor(PERF_CAT, levels = names(performance_color))
  )

tow_start <- read_transform(here::here("output", "goa", "shapefiles", "goa_towstart.shp"))

enc_wrecks <- read_transform(here::here("assets", "data", "wrecks", "enc_wrecks.shp"))

awois_wrecks <- read_transform(here::here("assets", "data", "wrecks", "awois_wrecks.shp"))

awois_obstructions <- read_transform(here::here("assets", "data", "wrecks", "awois_obstructions.shp"))

wrecks_obstructions <- dplyr::bind_rows(enc_wrecks, awois_wrecks) |> dplyr::bind_rows(awois_obstructions)

island_names <- read_transform(here::here("assets", "data", "placenames", "GOA_place_names_islands_large.shp"))

region_boundaries <- read_transform(here::here("assets", "data", "region_boundaries", "nmfs_boundaries.gpkg"))

trawlable_grid <- 
  read_transform(here::here("output", "goa", "shapefiles", "goa_trawlwable_grid.shp")) |>
  dplyr::mutate(
    TRAWLABLE = dplyr::case_when(
      is.na(TRAWLAB) ~ "Unknown grid cell",
      TRAWLAB == "N" ~ "Untrawlable grid cell",
      TRAWLAB == "Y" ~ "Trawlable grid cell"
    ),
    TRAWLABLE = factor(TRAWLABLE, levels = names(trawlable_fill))
  )

ch_nprw <- read_transform(here::here("assets", "data", "NPRW", "nprwch.shp"))

ch_sea_otter <- read_transform(here::here("assets", "data", "otters", "SeaOtterFinalCH_Project.shp"))

ch_humpback <- read_transform(here::here("assets", "data", "humpback", "WhaleHumpback_WesternNorthPacificDPS_20210421.shp"))

ssl_notransit <- read_transform(here::here("assets", "data", "SSLrookeries", "3nm_notransit.shp"))

akland <- map_layers$akland

darpa <- read_transform(here::here("assets", "data", "rockets", "DARPADownRange.shp"))

crabpots <- read_transform(here::here("output", "goa", "shapefiles", "goa_crabpots_2025.shp"))

# cables

# Multibeam

# if(region == "goa") {
ggplot() +
  geom_sf(
    data = akland,
    color = akland_color,
    fill = akland_fill
  ) +
  geom_sf(
    data = trawlable_grid,
    mapping = aes(fill = TRAWLABLE),
    color = trawlable_color,
    alpha = trawlable_alpha
  ) +
  scale_fill_manual(
    name = "Trawlable Status",
    values = trawlable_fill
  ) +
  new_scale_fill() +
  geom_sf(
    data = ch_nprw,
    mapping = aes(fill = "N. Pac. Right Whales"),
    color = ch_nprw_color,
    alpha = ch_nprw_alpha
  ) +
  geom_sf(
    data = ch_nprw,
    mapping = aes(fill = "N. Pac. Right Whales"),
    color = ch_nprw_color,
    alpha = ch_nprw_alpha
  ) +
  geom_sf(
    data = ch_sea_otter,
    mapping = aes(fill = "Sea Otters"),
    color = ch_sea_otter_color,
    alpha = ch_sea_otter_alpha
  ) +
  geom_sf(
    data = ch_humpback,
    mapping = aes(fill = "Humpback Whales"),
    color = ch_humpback_color,
    alpha = ch_humpback_alpha
  ) +
  scale_fill_manual(
    name = "Critical Habitat Areas",
    values = c(ch_nprw_fill, ch_sea_otter_fill, ch_humpback_fill)
  ) +
  geom_sf(
    data = ssl_notransit,
    mapping =
      aes(color = "SSL No Transit Zone"),
    fill = ssl_notransit_fill
  ) +
  scale_color_manual(name = "Other symbols", values = ssl_notransit_color) +
  new_scale_color() +
  # geom_sf(data = cables) +
  # geom_sf(data = darpa) +
  # geom_sf(data = trawl_cam) +
  geom_sf(
    data = crabpots, 
    mapping = aes(color = "Crabpot storage")
  ) +
  scale_color_manual(name = "Other symbols", values = c(crabpot_colors)) +
  geom_sf_text(data = island_names,
               mapping = aes(label = NAME)) +
  new_scale_color() +
  geom_sf(
    data = hazards,
    mapping = aes(color = "Buoys/moorings"),
    shape = hazards_shape
  ) +
  geom_sf(
    data = wrecks_obstructions,
    mapping = aes(color = "Wrecks and obstructions"), 
    shape = wrecks_shape
  ) +
  scale_color_manual(name = "Hazards", values = c(hazards_color, wrecks_color)) +
  new_scale_color() +
  geom_sf(
    data = towpath,
    mapping = aes(color = PERF_CAT)
  ) +
  geom_sf(
    data = region_boundaries,
    color = region_boundary_color
  ) +
  geom_sf(
    data = tow_start,
    size = rel(0.5)
  ) +
  scale_color_manual(name = "Historical tow performance", values = performance_color) +
  new_scale_fill() +
  geom_sf(
    data = allocation,
    mapping = aes(fill = VESSEL_NAME, shape = VESSEL_NAME),
    color = allocation_color
  ) +
  scale_fill_manual(name = "Stations", values = allocation_fill) +
  scale_shape_manual(name = "Stations", values = allocation_shape) +
  scale_x_continuous(limits = c(-100000, 250000)) +
  scale_y_continuous(limits = c(800000, 1100000)) +
  theme_bw() +
  theme(
    plot.background = element_rect(fill = NA, color = "black"),
    legend.box.background = element_rect(fill = "white", color = "black"),
    axis.title = element_blank(),
    legend.title = element_text(face = "bold")
  )
# }



