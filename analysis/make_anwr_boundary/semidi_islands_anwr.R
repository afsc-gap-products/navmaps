# Get Alaska National Wildlife Refuge boundaries
anwr <- sf::st_read(here::here("analysis", "make_anwr_boundary", "National_Wildlife_Refuge_System_Boundaries.gpkg")) |>
  dplyr::filter(ORGNAME == "ALASKA MARITIME NATIONAL WILDLIFE REFUGE") |>
  sf::st_cast("POLYGON") |>
  dplyr::mutate(ID = row_number(ORGNAME)) |>
  sf::st_transform(crs = "EPSG:3338") |>
  sf::st_set_geometry("geom")

ggplot() +
  geom_sf(
    data = anwr
  ) +
  geom_sf_text(
    data = sf::st_centroid(anwr),
    mapping = aes(label = ID)
  )

ggplot() +
  geom_sf(
    data = dplyr::filter(anwr, ID == 3)
  ) +
  geom_sf_text(
    data = sf::st_centroid(dplyr::filter(anwr, ID == 3)),
    mapping = aes(label = ID)
  )

# Get Semidi islands polygon
semidi <- anwr |>
  dplyr::filter(ID == 3) |>
  dplyr::select(ORGNAME, ORGCODE, LIT, RSL_TYPE, FWSREGION) |>
  dplyr::mutate(label = "Semidi Islands")

sf::st_write(semidi, dsn = here::here("assets", "data", "anwr", "anwr_semidi_islands.gpkg"))
