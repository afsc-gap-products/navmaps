library(sf)
library(dplyr)

# 1. Region divider line segments (thick black meridian bars at 159W and 147W)
divider_lines_df <- data.frame(
  chart_number = c(1, 2, 2, 3),
  boundary_lon = c(-159.0, -159.0, -147.0, -147.0),
  lat_start    = c(52.85, 58.95, 57.05, 61.55),
  lat_end      = c(53.45, 59.55, 57.65, 62.25)
)

divider_lines_sf <- lapply(seq_len(nrow(divider_lines_df)), function(i) {
  row <- divider_lines_df[i, ]
  line_geom <- st_linestring(
    matrix(
      c(row$boundary_lon, row$lat_start,
        row$boundary_lon, row$lat_end),
      ncol = 2,
      byrow = TRUE
    )
  )
  st_sf(
    chart_number = row$chart_number,
    boundary_lon = row$boundary_lon,
    geometry = st_sfc(line_geom, crs = 4326)
  )
}) |>
  do.call(what = rbind) |>
  st_transform(crs = 3338)

# 2. Region divider text labels (preserving exact line breaks & wording per chart)
divider_labels_df <- data.frame(
  chart_number = c(
    1, #1,
    2, 2, 2, 2,
    3, 3,
    4
  ),
  side = c(
    "left", #"right",
    "left", "right", "left", "right",
    "left", "right",
    "center"
  ),
  hjust = c(
    0, #0,
    0, 0, 0, 0,
    0, 0,
    0.5
  ),
  lon = c(
    -159.75, #-158.75,
    -159.80, -158.85, -148.05, -146.75,
    -148.15, -146.75,
    -141.75
  ),
  lat = c(
    53.15, #53.15,
    59.25, 59.25, 57.35, 57.35,
    61.90, 61.90,
    56.05
  ),
  label = c(
    # Chart 1
    "Western\nGulf/\nShumagin\n(170W to\n159W)",
    # "Central\nGOA\nChirikof/\nKodiak\n(158.99W\nto 147W)",
    # Chart 2 (159W pair)
    "Western\nGulf/\nShumagin\n(170W to\n159W)",
    "Central Gulf\nChirikof/\nKodiak\n(158.99W to\n147W)",
    # Chart 2 (147W pair)
    "Central Gulf\nChirikof/\nKodiak\n(158.99W to\n147W)",
    "Eastern Gulf\nYakutat/\nSoutheast\n(146.99W to\n131W)",
    # Chart 3 (147W pair)
    "Central Gulf\nChirikof/\nKodiak\n(158.99W to\n147W)",
    "Eastern Gulf\nYakutat/\nSoutheast\n(146.99W to\n131W)",
    # Chart 4 (standalone)
    "Eastern Gulf\nYakutat/\nSoutheast\n(146.99W to\n131W)"
  )
)

divider_labels_sf <- divider_labels_df |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(crs = 3338)

# 3. Write both layers to a single GeoPackage
out_gpkg <- here::here("assets", "data", "region_boundaries", "goa_region_divider_annotations.gpkg")

st_write(divider_lines_sf, dsn = out_gpkg, layer = "boundary_lines", delete_dsn = TRUE)
st_write(divider_labels_sf, dsn = out_gpkg, layer = "boundary_labels", append = TRUE)
