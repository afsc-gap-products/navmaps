library(navmaps)
library(ggnewscale)
library(ggrepel)
library(ggstar)
library(geomtextpath)

# GOA Chart

crs_aea <- 3338

# Settings: Colors and fill ----

# standard layers
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
allocation_color <- allocation_fill


wrecks_color <- c("Wrecks and obstructions" = "#FF48DC")
wrecks_shape <- "\u2605"

ch_sea_otter_color <- "grey20"
ch_sea_otter_fill <- c("Sea Otters" =  "#D86ECC")
ch_sea_otter_alpha <- 0.3

ch_nprw_color <- "grey20"
ch_nprw_fill <- c("N. Pac. Right Whales" = "#46B1E1")
ch_nprw_alpha <- 0.3

ch_humpback_color <- "grey20"
ch_humpback_fill <- c("Humpback Whales" = "grey70")
ch_humpback_alpha <- 0.3

ch_beluga_color <- "grey20"
ch_beluga_fill <- c("Cook Inlet Beluga Whales" = "gold2")
ch_beluga_alpha <- 0.3

hazards_color <- c("Buoys/moorings" = "grey20")
hazards_shape <- 10

performance_color <- c(
  "Good tow" = "darkgreen", 
  "Unsatisfactory" = "red", 
  "Other problems, satisfactory" = "cyan"
)

trawlable_color <- "black"
trawlable_fill <- c("Unknown" = NA, "Trawlable" = "lightgreen", "Untrawlable" = "salmon")
trawlable_alpha <- 0.5

ssl_notransit_color <- "magenta"
ssl_notransit_fill <- NA

crabpot_colors <- c("Crabpot storage" = "red")

graticule_color <- "#197DFF"

region_boundary_color <- "red"

cable_color <- c("Fiber optic and older" = "grey30")

# special projects
dropcam_color <- "violet"
dropcam_fill <- NA


# Functions ----

read_transform <- function(fpath, crs = 3338) {
  
  sf::st_read(fpath) |>
    sf::st_transform(crs)
  
}

get_top_inner_edge <- function(
    sf_obj,
    label_col,
    inset_dist = NULL,
    top_quantile = 0.70,
    smooth_tol = NULL,
    largest_only = TRUE
) {
  
  if (st_is_longlat(sf_obj)) {
    warning("Input sf object uses geographic coordinates (degrees). Project to a planar CRS (e.g., EPSG:3338) for uniform inset buffering.")
  }
  
  results <- lapply(seq_len(nrow(sf_obj)), function(i) {
    feat <- sf_obj[i, ]
    lbl <- pull(feat, {{ label_col }})[1]
    geom <- st_geometry(feat)
    
    if (largest_only && inherits(geom, "sfc_MULTIPOLYGON")) {
      polys <- st_cast(st_sfc(geom, crs = st_crs(sf_obj)), "POLYGON")
      geom <- polys[which.max(st_area(polys))]
    }
    
    bb <- st_bbox(geom)
    dist <- if (is.null(inset_dist)) {
      -0.05 * (bb["ymax"] - bb["ymin"])
    } else {
      -abs(inset_dist)
    }
    
    inner_poly <- st_buffer(geom, dist = dist)
    if (st_is_empty(inner_poly)) {
      stop(sprintf("Inset distance (%s) is too large for feature '%s', resulting in an empty geometry.", dist, lbl))
    }
    
    if (!is.null(smooth_tol)) {
      inner_poly <- st_simplify(inner_poly, dTolerance = smooth_tol, preserveTopology = TRUE)
    }
    
    if (inherits(inner_poly, "sfc_MULTIPOLYGON")) {
      polys <- st_cast(inner_poly, "POLYGON")
      inner_poly <- polys[which.max(st_area(polys))]
    }
    
    inner_line <- st_cast(inner_poly, "LINESTRING")
    coords <- st_coordinates(inner_line)
    
    y_cutoff <- quantile(coords[, "Y"], probs = top_quantile, na.rm = TRUE)
    top_coords <- coords[coords[, "Y"] >= y_cutoff, , drop = FALSE]
    
    top_coords <- top_coords[order(top_coords[, "X"]), , drop = FALSE]
    
    line_sfc <- st_sfc(st_linestring(top_coords[, c("X", "Y")]), crs = st_crs(sf_obj))
    st_sf(label = lbl, geometry = line_sfc)
  })
  
  do.call(rbind, results)
}

# Load standard layers ----

map_layers <- akgfmaps::get_base_layers(select.region = "goa", set.crs = crs_aea)

akland <- map_layers$akland

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

island_names_large <- read_transform(here::here("assets", "data", "placenames", "GOA_place_names_islands_large.shp"))

island_names_small <- read_transform(here::here("assets", "data", "placenames", "GOA_place_names_islands_small.shp"))

region_boundaries <- read_transform(here::here("assets", "data", "region_boundaries", "nmfs_boundaries.gpkg"))

trawlable_grid <- 
  read_transform(here::here("output", "goa", "shapefiles", "goa_trawlwable_grid.shp")) |>
  dplyr::mutate(
    TRAWLABLE = dplyr::case_when(
      is.na(TRAWLAB) ~ "Unknown",
      TRAWLAB == "N" ~ "Untrawlable",
      TRAWLAB == "Y" ~ "Trawlable"
    ),
    TRAWLABLE = factor(TRAWLABLE, levels = names(trawlable_fill))
  )

ch_nprw <- read_transform(here::here("assets", "data", "NPRW", "nprwch.shp"))

ch_sea_otter <- read_transform(here::here("assets", "data", "otters", "SeaOtterFinalCH_Project.shp"))

ch_humpback <- read_transform(here::here("assets", "data", "humpback", "WhaleHumpback_WesternNorthPacificDPS_20210421.shp"))

ch_beluga <- read_transform(here::here("assets", "data", "CIbeluga", "CIB_critical_habitat.shp"))

ssl_notransit <- read_transform(here::here("assets", "data", "SSLrookeries", "3nm_notransit.shp"))

darpa <- read_transform(here::here("assets", "data", "rockets", "DARPALaunch.shp"))

cable_files <- list.files(here::here("assets", "data", "cables"), pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)

cables <- lapply(cable_files, FUN = read_transform) |>
  do.call(what = dplyr::bind_rows)

mb_annotation <- read_transform(here::here("assets", "data", "goa_multibeam_annotations.gpkg"))

semidi_islands <- read_transform(here::here("assets", "data", "anwr", "anwr_semidi_islands.gpkg"))

semidi_label <- semidi_islands |>
  dplyr::mutate(
    label = "ANWR Semidi Islands: Ok to tow,\nbut no anchoring or visiting the islands."
  ) |>
  get_top_inner_edge(label_col = label)

# Load special project layers ----

dropcam <- read_transform(here::here("output", "goa", "shapefiles", "drop_cam_zones.shp"))

# Load crab pot layers ----

crabpots <- read_transform(here::here("output", "goa", "shapefiles", "goa_crabpots_2025.shp"))

crabpot_marks <- crabpots |>
  sf::st_segmentize(dfMaxLength = 2000) |>
  sf::st_cast(to = "MULTIPOINT")

# Set extent for each chart
chart_boudaries <- data.frame(
  chart_number = 1:4,
  min_lat  = c(  51.5,   54.5,   57.5,   55.3),
  max_lat  = c(  56.8,   59.8,   63.0,   60.5),
  min_lon  = c(-169.5, -159.1, -154.5, -144.5),
  max_lon  = c(-159.5, -147.2, -141.5, -132.3),
  # Set legend anchor positions
  leg_x    = c(0.02, 0.98, 0.02, 0.02),
  leg_y    = c(0.78, 0.05, 0.96, 0.14),
  leg_just_x = c(0, 1, 0, 0),
  leg_just_y = c(1, 0, 1, 0)
)

# Function to project WGS84 bounding-box corners to EPSG:3338 xlim / ylim
get_chart_limits <- function(row, crs_target = 3338) {
  corners <- expand.grid(
    lon = c(row$min_lon, row$max_lon),
    lat = c(row$min_lat, row$max_lat)
  )
  pts_proj <- st_as_sf(corners, coords = c("lon", "lat"), crs = 4326) |>
    st_transform(crs = crs_target) |>
    st_coordinates()
  
  list(
    xlim = range(pts_proj[, "X"]),
    ylim = range(pts_proj[, "Y"])
  )
}

# Function to build either a 42x34" PDF plot or a screen-sized preview
build_goa_chart <- function(chart_num, mode = c("pdf", "preview")) {
  mode <- match.arg(mode)
  cfg  <- chart_boudaries[chart_boudaries$chart_number == chart_num, ]
  lims <- get_chart_limits(cfg, crs_target = 3338)
  
  # Crop allocation points to the current chart extent so ggrepel only 
  # repels labels that are actually visible on this chart
  chart_bbox <- sf::st_bbox(
    c(xmin = lims$xlim[1], xmax = lims$xlim[2],
      ymin = lims$ylim[1], ymax = lims$ylim[2]),
    crs = sf::st_crs(3338)
  )
  allocation_sub <- sf::st_crop(
    sf::st_transform(allocation, 3338),
    chart_bbox
  )
  
  # Set sizes for pdf and preview plots
  if(mode == "pdf") {
    txt_darpa     <- 7.5
    txt_island    <- 5.5
    txt_station   <- 3.2
    txt_mb        <- 4.8
    arrow_len_s   <- unit(0.32, "cm")
    arrow_len_d   <- unit(0.38, "cm")
    arrow_lwd     <- 0.9
    tow_pt_size   <- rel(0.5)
    alloc_pt_size <- 2.8
    base_txt_size <- 18
    leg_title_sz  <- 16
    leg_text_sz   <- 13
    leg_key_sz    <- unit(0.65, "cm")
  } else {
    txt_darpa     <- 3.2
    txt_island    <- 2.3
    txt_station   <- 1.5
    txt_mb        <- 2.1
    arrow_len_s   <- unit(0.14, "cm")
    arrow_len_d   <- unit(0.16, "cm")
    arrow_lwd     <- 0.45
    tow_pt_size   <- rel(0.2)
    alloc_pt_size <- 1.4
    base_txt_size <- 9
    leg_title_sz  <- 8.5
    leg_text_sz   <- 7
    leg_key_sz    <- unit(0.32, "cm")
  }
  
  ggplot() +
    geom_sf(
      data = akland,
      color = akland_color,
      fill = akland_fill,
      linewidth = 0.2
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
    # geom_sf(
    #   data = ch_humpback,
    #   mapping = aes(fill = "Humpback Whales"),
    #   color = ch_humpback_color,
    #   alpha = ch_humpback_alpha
    # ) +
    geom_sf(
      data = ch_beluga,
      mapping = aes(fill = "Cook Inlet Beluga Whales"),
      color = ch_beluga_color,
      alpha = ch_beluga_alpha
    ) +
    scale_fill_manual(
      name = "Critical Habitat Areas",
      values = c(ch_nprw_fill, ch_sea_otter_fill, ch_humpback_fill, ch_beluga_fill),
      guide = guide_legend(order = 6)
    ) +
    
    new_scale_fill() +
    geom_sf(
      data = trawlable_grid,
      mapping = aes(fill = TRAWLABLE),
      color = trawlable_color,
      alpha = trawlable_alpha,
      linewidth = 0.2
    ) +
    scale_fill_manual(
      name = "Station grid",
      values = trawlable_fill,
      guide = guide_legend(order = 3)
    ) +
    
    new_scale_color() +
    new_scale_fill() +
    geom_sf(
      data = ssl_notransit,
      mapping = aes(color = "SSL No Transit Zone"),
      fill = ssl_notransit_fill,
      linewidth = 1.5
    ) +
    scale_color_manual(
      name = "Steller Sea Lions",
      values = ssl_notransit_color,
      guide = guide_legend(order = 2)
    ) +
    
    new_scale_color() +
    geom_sf(
      data = cables,
      mapping = aes(color = "Fiber optic and older"),
      linetype = 4
    ) +
    scale_color_manual(
      name = "Cables",
      values = cable_color,
      guide = guide_legend(order = 5)
    ) +
    geom_sf(data = darpa) +
    geom_sf_text(
      data = st_centroid(darpa),
      mapping = aes(label = "Rocket Exclusion\nArea"),
      size = txt_darpa,
      lineheight = 0.95
    ) +
    
    new_scale_color() +
    geom_sf(
      data = crabpots,
      mapping = aes(color = "Crabpot storage"),
      linewidth = 0.7
    ) +
    geom_sf(
      data = crabpot_marks,
      mapping = aes(color = "Crabpot storage"),
      shape = 4,
      size = 1.5
    ) +
    scale_color_manual(
      name = "Crab pot storage",
      values = c(crabpot_colors),
      guide = guide_legend(order = 8)
    ) +
    geom_sf_text(
      data = island_names_large,
      mapping = aes(label = NAME),
      size = txt_island
    ) +
    geom_sf_text(
      data = island_names_small,
      mapping = aes(label = NAME),
      size = txt_island*0.4
    ) +
    
    new_scale_color() +
    geom_sf(
      data = hazards,
      mapping = aes(color = "Buoys/moorings"),
      shape = hazards_shape,
      size = 3
    ) +
    geom_star(
      data = wrecks_obstructions,
      aes(geometry = geometry, color = "Wrecks and obstructions", fill = "Wrecks and obstructions"),
      stat = "sf_coordinates",
      starshape = 1,
      size = 3
    ) +
    # geom_sf(
    #   data = wrecks_obstructions,
    #   mapping = aes(color = "Wrecks and obstructions"),
    #   shape = wrecks_shape,
    #   family = "DejaVu Sans"
    # ) +
    scale_color_manual(
      name = "Hazards",
      values = c(hazards_color, wrecks_color),
      guide = guide_legend(order = 7)
    ) +
    
    geom_sf(
      data = dropcam,
      color = dropcam_color,
      fill = dropcam_fill
    ) +
    
    new_scale_color() +
    geom_sf(
      data = towpath,
      mapping = aes(color = PERF_CAT),
      linewidth = 0.3
    ) +
    scale_color_manual(
      name = "Historic tow performance",
      values = performance_color,
      guide = guide_legend(order = 4)
    ) +
    geom_sf(
      data = tow_start,
      size = 0.08
    ) +
    geom_sf(
      data = region_boundaries,
      color = region_boundary_color
    ) +
    geom_sf(
      data = semidi_islands,
      color = "red",
      linewidth = 1.3,
      fill = NA
    ) +
    geomtextpath::geom_textsf(
      data = semidi_label,
      mapping = aes(label = label),
      size = txt_island*0.4,
      vjust = 0.5
    ) +
    
    new_scale_fill() +
    new_scale_color() +
    geom_sf(
      data = allocation_sub,
      mapping = aes(fill = VESSEL_NAME, shape = VESSEL_NAME),
      size = alloc_pt_size
    ) +
    ggrepel::geom_text_repel(
      data = allocation_sub,
      mapping = aes(color = VESSEL_NAME, label = STATION, geometry = geometry),
      stat = "sf_coordinates",
      size = txt_station,
      box.padding = 0.15,
      point.padding = 0.15,
      min.segment.length = 0.2,
      segment.size = 0.25,
      max.overlaps = Inf,
      seed = 42,
      hjust = -0.1
    ) +
    
    # Smooth sheet & multibeam arrows and rotated labels
    geom_sf(
      data = dplyr::filter(mb_annotation, arrow_type == "single", feature == "arrow"),
      color = "black",
      linewidth = arrow_lwd,
      arrow = arrow(ends = "last", type = "closed", length = arrow_len_s)
    ) +
    geom_sf(
      data = dplyr::filter(mb_annotation, arrow_type %in% c("double", "double_bent"), feature == "arrow"),
      color = "black",
      linewidth = arrow_lwd * 1.15,
      arrow = arrow(ends = "both", type = "closed", length = arrow_len_d)
    ) +
    geom_sf_text(
      data = dplyr::filter(mb_annotation, feature == "label"),
      aes(label = label_text, angle = angle),
      color = "firebrick3",
      size = txt_mb,
      fontface = "bold"
    ) +
    
    scale_fill_manual(
      name = "Vessel",
      values = allocation_fill,
      guide = guide_legend(order = 1)
    ) +
    scale_color_manual(values = allocation_color, guide = "none") +
    scale_shape_manual(
      name = "Vessel",
      values = allocation_shape,
      guide = guide_legend(order = 1)
    ) +
    
    # Set individual chart extent in EPSG:3338 with WGS84 graticule ticks
    coord_sf(
      xlim = lims$xlim,
      ylim = lims$ylim,
      crs = 3338,
      datum = sf::st_crs(4326),
      expand = FALSE,
      label_axes = list(
        top    = "E",
        bottom = "E",
        left   = "N",
        right  = "N"
      )
    ) +
    
    theme_bw(base_size = base_txt_size) +
    theme(
      plot.background       = element_rect(fill = "white", color = "black", linewidth = 0.8),
      panel.grid.major      = element_line(color = "grey82", linewidth = 0.3),
      axis.title            = element_blank(),
      axis.text             = element_text(size = rel(0.85), color = "black"),
      # Place combined legend inside the open corner of each chart
      legend.position       = "inside",
      legend.position.inside = c(cfg$leg_x, cfg$leg_y),
      legend.justification  = c(cfg$leg_just_x, cfg$leg_just_y),
      legend.box            = "vertical",
      legend.box.background = element_rect(fill = "white", color = "black", linewidth = 0.6),
      legend.box.margin     = margin(6, 8, 6, 8),
      legend.background     = element_blank(),
      legend.title          = element_text(face = "bold", size = leg_title_sz),
      legend.text           = element_text(size = leg_text_sz),
      legend.key.size       = leg_key_sz,
      legend.spacing.y      = unit(0.15, "cm")
    )
}

set_year <- 2027

# Generate and export 42 x 34 inch PDFs
pdf_plots <- lapply(chart_boudaries$chart_number, function(i, yr) {
  p <- build_goa_chart(i, mode = "pdf")
  out_file <- here::here("assets", "survey_charts", "GOA", sprintf("GOA_Station_map%d_%d.pdf", i, yr))
  ggsave(
    filename = out_file,
    plot     = p,
    device   = cairo_pdf,
    width    = 42,
    height   = 34,
    units    = "in"
  )
  p
}, yr = set_year)

names(pdf_plots) <- paste0("map", chart_boudaries$chart_number)

# Generate second set of screen-sized preview plots
preview_plots <- lapply(chart_boudaries$chart_number, function(i) {
  build_goa_chart(i, mode = "preview")
})
names(preview_plots) <- paste0("map", chart_boudaries$chart_number)

# View screen previews in RStudio / plotting device:
print(preview_plots$map1)
print(preview_plots$map2)
print(preview_plots$map3)
print(preview_plots$map4)
