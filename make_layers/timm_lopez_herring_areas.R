library(navmaps)

# Herring (Lopez & Timm) 
# Herring savings area: https://www.ecfr.gov/current/title-50/chapter-VI/part-679/appendix-Figure%204%20to%20Part%20679

# Summer Area 1 (Lat 55-57, Lon -164 to -162)
s1_coords <- matrix(c(
  -164, 55,
  -162, 55,
  -162, 57,
  -164, 57,
  -164, 55  # Closing the polygon
), ncol = 2, byrow = TRUE)

# Summer Area 2 (Lat 54-56.3, Lon -164 to -162)
s2_coords <- matrix(c(
  -164, 54,
  -167, 54,
  -167, 56.5,
  -164, 56.5,
  -164, 54  # Closing the polygon
), ncol = 2, byrow = TRUE)

# Winter Area (Lat 58-60, Lon -175 to -172)
w_coords <- matrix(c(
  -175, 58,
  -172, 58,
  -172, 60,
  -175, 60,
  -175, 58  # Closing the polygon
), ncol = 2, byrow = TRUE)

# 2. Create the sfc (Simple Feature Column) objects
polys_sfc <- st_sfc(
  st_polygon(list(s1_coords)),
  st_polygon(list(s2_coords)),
  st_polygon(list(w_coords)),
  crs = 4326 # Assigning WGS84 coordinate system
)

# 3. Create the final sf data frame with metadata
hsa_sf <- st_sf(
  name = c("Summer Area 1", "Summer Area 2", "Winter Area"),
  dates = c("15 June - 1 July", "1 July - 15 Aug", "1 Sep - 1 Mar"),
  geometry = polys_sfc
)

dir.create(here::here("assets", "data", "special_projects", "EBS", 2025), recursive = TRUE)

sf::st_write(
  hsa_sf,
  here::here("assets", "data", "special_projects", "EBS", 2025, "herring_savings_area.shp")
)
