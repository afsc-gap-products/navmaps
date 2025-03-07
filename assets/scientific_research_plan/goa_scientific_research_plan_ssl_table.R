# Generate SSL critical habitat catch table for GOA SRP
# Created by sean-rohan-noaa
# Last update: March 7, 2025

library(navmaps) # afsc-gap-products/navmaps
library(tidyr)

# Set vessel, cruise, and region ----
vessel <- c(148, 176)
cruise <- 202301
region <- tolower("goa")

# Retrieve towpath lines -- need to first generate towpaths using make_layers_goa.R or make_layers_ai.R
towpaths <- 
  sf::st_read(
    dsn = here::here("output", region, "shapefiles", paste0(region, "_towpath.shp"))
  ) |>
  dplyr::filter(CRUISE == cruise)

# Check that towpaths have been generated for the selected vessels/cruise
nrow(towpaths)

# Retrieve SSL critical habitat polygon
ssl_ch <- sf::st_read(
  dsn = here::here("assets", "data", "SSLcriticalhabitat", "Crit_Hab_Coded_WDPS.shp")
) |>
  sf::st_transform(crs = sf::st_crs(towpaths))

# Intersect towpaths with SSL Critical Habitat to find hauls inside SSL CH
ssl_ch_hauls <- 
  towpaths |>
  sf::st_intersection(ssl_ch) |>
  sf::st_drop_geometry() |>
  dplyr::select(VESSEL, CRUISE, HAUL) |>
  dplyr::mutate(SSL_CH = "INSIDE_SSL_CH_KG") |>
  unique()

# Connect
channel <- navmaps::get_connected(schema = "AFSC")

# Assign catch inside and outside of SSL critical habitat
catch <- 
  RODBC::sqlQuery(
    channel = channel,
    query = 
      paste0("
      SELECT 
        VESSEL,
        CRUISE,
        HAUL,
        SPECIES_CODE,
        WEIGHT,
        NUMBER_FISH
      FROM RACEBASE.CATCH
      WHERE
        VESSEL IN (", paste(vessel, collapse = ", "), ")",
             "AND CRUISE = ", cruise)
  ) |>
  dplyr::left_join(
    ssl_ch_hauls,
    by = c("VESSEL", "CRUISE", "HAUL")) |>
  dplyr::mutate(
    SSL_CH = dplyr::if_else(is.na(SSL_CH), "OUTSIDE_SSL_CH_KG", "INSIDE_SSL_CH_KG")
  )

# Update species list ----

# Individual species
taxa_species <- 
  RODBC::sqlQuery(
    channel = channel,
    query = "
      SELECT DISTINCT 
        SPECIES_NAME, 
        COMMON_NAME, 
        SPECIES_CODE, 
        COMMON_NAME AS TABLE_GROUP
      FROM
        GAP_PRODUCTS.TAXONOMIC_CLASSIFICATION 
      WHERE 
        SPECIES_CODE IN (21740, 21720, 20510, 21921, 23220, 30060, 30420, 30576, 10110, 10130, 
        10112, 10261, 10120, 10210, 69310, 69322, 68560)"
    )

taxa_species$TABLE_GROUP[taxa_species$TABLE_GROUP == "chinook salmon"] <- "Chinook salmon"

# Forage fishes - based on FMP definition
taxa_forage <- 
  RODBC::sqlQuery(
    channel = channel,
    query = "
      SELECT DISTINCT 
        SPECIES_NAME, 
        COMMON_NAME, 
        SPECIES_CODE, 
        'forage fish' AS TABLE_GROUP
      FROM
        GAP_PRODUCTS.TAXONOMIC_CLASSIFICATION 
      WHERE 
        FAMILY_TAXON IN ('Clupeidae', 'Osmeridae', 'Ammodytidae', 'Trichodontidae', 'Stichaeidae', 
          'Pholidae', 'Myctophidae', 'Bathylagidae', 'Gonostomatidae', 'Euphausiidae', 
          'Sergestidae', 'Sicyoniidae', 'Pandalidae', 'Thoridae', 'Penaeidae', 'Hippolytidae',
          'Crangonidae', 'Pasiphaeidae', 'Oplophoridae', 'Acanthephyridae', 'Benthesicymidae')
        AND DATABASE = 'ITIS'
    ")

# Black-spotted/rougheye rockfish
taxa_bsre <- 
  RODBC::sqlQuery(
    channel = channel,
    query = "
      SELECT DISTINCT
        SPECIES_NAME, 
        COMMON_NAME, 
        SPECIES_CODE,
        'rougheye/blackspotted rockfishes' AS TABLE_GROUP
      FROM
        GAP_PRODUCTS.TAXONOMIC_CLASSIFICATION 
      WHERE 
        SPECIES_CODE IN (30050, 30051, 30052)"
    )


# Sculpins
taxa_sculpins <- 
  RODBC::sqlQuery(
    channel = channel,
    query = "
      SELECT DISTINCT 
        SPECIES_NAME, 
        COMMON_NAME, 
        SPECIES_CODE,
        'sculpins' AS TABLE_GROUP
      FROM
        GAP_PRODUCTS.TAXONOMIC_CLASSIFICATION 
      WHERE 
        SUPERFAMILY_TAXON = 'Cottoidea'"
)

# Other flatfishes
taxa_flatfishes <- 
  RODBC::sqlQuery(
    channel = channel,
    query = "
      SELECT DISTINCT 
        SPECIES_NAME, 
        COMMON_NAME, 
        SPECIES_CODE,
        'other flatfish' AS TABLE_GROUP
      FROM 
        GAP_PRODUCTS.TAXONOMIC_CLASSIFICATION
      WHERE 
        ORDER_TAXON = 'Pleuronectiformes'"
) |>
  dplyr::filter(!(SPECIES_CODE %in% taxa_species$SPECIES_CODE))

# Other rockfishes
taxa_rockfishes <- 
  RODBC::sqlQuery(
    channel = channel,
    query = "
      SELECT DISTINCT 
        SPECIES_NAME, 
        COMMON_NAME, 
        SPECIES_CODE,
        'other rockfishes' AS TABLE_GROUP
      FROM 
        GAP_PRODUCTS.TAXONOMIC_CLASSIFICATION
      WHERE 
        SUBORDER_TAXON = 'Scorpaenoidei'"
  ) |>
  dplyr::filter(!(SPECIES_CODE %in% taxa_species$SPECIES_CODE),
                !(SPECIES_CODE %in% taxa_bsre$SPECIES_CODE))

# Squid
taxa_squid <- 
  RODBC::sqlQuery(
    channel = channel,
    query = "
      SELECT DISTINCT 
        SPECIES_NAME, 
        COMMON_NAME, 
        SPECIES_CODE,
        'squid' AS TABLE_GROUP
      FROM 
        GAP_PRODUCTS.TAXONOMIC_CLASSIFICATION
      WHERE 
        SUPERORDER_TAXON = 'Decapodiformes'"
  )

# Skates
taxa_skates <- 
  RODBC::sqlQuery(
    channel = channel,
    query = "
      SELECT DISTINCT 
        SPECIES_NAME, 
        COMMON_NAME, 
        SPECIES_CODE,
        'skates' AS TABLE_GROUP
      FROM 
        GAP_PRODUCTS.TAXONOMIC_CLASSIFICATION
      WHERE 
        ORDER_TAXON = 'Rajiformes'
        AND COMMON_NAME NOT LIKE '%egg%'
        AND DATABASE = 'ITIS'"
        
  )

# Sharks
taxa_sharks <- 
  RODBC::sqlQuery(
    channel = channel,
    query = "
      SELECT DISTINCT 
        SPECIES_NAME, 
        COMMON_NAME, 
        SPECIES_CODE,
        'sharks' AS TABLE_GROUP
      FROM 
        GAP_PRODUCTS.TAXONOMIC_CLASSIFICATION
      WHERE 
        INFRACLASS_TAXON = 'Selachii'"
  )

# Octopus
taxa_octopus <- 
  RODBC::sqlQuery(
    channel = channel,
    query = "
      SELECT DISTINCT 
        SPECIES_NAME, 
        COMMON_NAME, 
        SPECIES_CODE,
        'octopus' AS TABLE_GROUP
      FROM 
        GAP_PRODUCTS.TAXONOMIC_CLASSIFICATION
      WHERE 
        ORDER_TAXON IN ('Octopoda', 'Vampyromorpha')
        AND DATABASE = 'ITIS'"
  )

# Combine taxa lists ----
taxa_all <- 
  dplyr::bind_rows(
    taxa_bsre,
    taxa_flatfishes,
    taxa_forage,
    taxa_octopus,
    taxa_rockfishes,
    taxa_sculpins,
    taxa_sharks,
    taxa_skates,
    taxa_species,
    taxa_squid
  )

# Join catch with taxa list ----
catch_taxa <- 
  dplyr::inner_join(
    catch,
    taxa_all,
    by = "SPECIES_CODE"
  )

# Inside/outside/total with values rounded up to the nearest kilogram
catch_by_area <-
  catch_taxa |>
  dplyr::group_by(TABLE_GROUP, SSL_CH) |>
  dplyr::summarise(TOTAL_WEIGHT = ceiling(sum(WEIGHT))) |>
  tidyr::pivot_wider(
    id_cols = "TABLE_GROUP", 
    names_from = "SSL_CH",
    values_from = "TOTAL_WEIGHT",
    values_fill = 0) |>
  dplyr::mutate(TOTAL_CATCH_KG = INSIDE_SSL_CH_KG + OUTSIDE_SSL_CH_KG)

# Sort by FMP group
table_group_order <- data.frame(
  TABLE_GROUP = c(
    "walleye pollock",
    "Pacific cod",
    "sablefish",
    "Atka mackerel",
    "Chinook salmon",
    "forage fish",
    "sculpins",
    "Pacific ocean perch",
    "northern rockfish",
    "rougheye/blackspotted rockfishes",
    "shortraker rockfish",
    "other rockfishes",
    "arrowtooth flounder",
    "flathead sole",
    "Kamchatka flounder",
    "northern rock sole",
    "Pacific halibut",
    "yellowfin sole",
    "other flatfish",
    "sharks",
    "skates",
    "golden king crab",
    "red king crab",
    "Tanner crab",
    "octopus",
    "squid")
)

catch_by_area_sorted <- 
  dplyr::full_join(
    table_group_order, 
    catch_by_area,
    by = "TABLE_GROUP"
  )

# Add commas
catch_by_area_sorted$INSIDE_SSL_CH_KG <- 
  format(catch_by_area_sorted$INSIDE_SSL_CH_KG, big.mark = ",", trim = TRUE)

catch_by_area_sorted$OUTSIDE_SSL_CH_KG <- 
  format(catch_by_area_sorted$OUTSIDE_SSL_CH_KG, big.mark = ",", trim = TRUE)

catch_by_area_sorted$TOTAL_CATCH_KG <- 
  format(catch_by_area_sorted$TOTAL_CATCH_KG, big.mark = ",", trim = TRUE)

# Write to csv
write.csv(x = catch_by_area_sorted,
          file = here::here("assets", "scientific_research_plan", 
                            paste0(region, "_", cruise, "_ssl_critical_habitat_catch_", 
                                   gsub(pattern = "-", replacement = "", x = Sys.Date()),
                                   ".csv")),
          row.names = FALSE)