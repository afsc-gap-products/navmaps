# Prepare catch and specimen/voucher table for GOA/AI ADFG report
# Created by sean-rohan-noaa
# Last update: March 7, 2025

library(navmaps) # afsc-gap-products/navmaps (v 1.1.10)
library(akgfmaps) #afsc-gap-products/akgfmaps (v 4.0.3)
library(tidyr)
library(here)

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

# Retrieve Alaska DNR Land polygon from akgfmaps ----
layers <- 
  akgfmaps::get_base_layers(
    select.region = region, 
    set.crs = "EPSG:3338",
    high.resolution.coast = TRUE
  )

# Add 3 nm buffer to land ----
land_buffer <- 
  layers$akland |>
  dplyr::filter(STATE_PROVINCE == "Alaska") |>
  sf::st_buffer(
    dist = 3*1852, # 3 nautical miles
    single_side = TRUE
  )

state_hauls <- 
  towpaths |>
  sf::st_intersection(land_buffer) |>
  sf::st_drop_geometry() |>
  dplyr::select(VESSEL, CRUISE, HAUL) |>
  dplyr::mutate(STATE_WATERS = "STATE_KG") |>
  unique()

# Connect
channel <- navmaps::get_connected(schema = "AFSC")

# Retrieve catch and specimen data
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
    NUMBER_FISH,
    VOUCHER
  FROM RACEBASE.CATCH
  WHERE
    VESSEL IN (", paste(vessel, collapse = ", "), ")",
             "AND CRUISE = ", cruise)
  ) |>
  dplyr::left_join(state_hauls,
                   by = c("VESSEL", "CRUISE", "HAUL")) |>
  dplyr::mutate(STATE_WATERS = ifelse(is.na(STATE_WATERS), "FEDERAL", "STATE"))

specimen <- 
  RODBC::sqlQuery(
    channel = channel,
    query = 
      paste0("
  SELECT 
    VESSEL,
    CRUISE,
    HAUL,
    SPECIES_CODE,
    SPECIMENID,
    SPECIMEN_SAMPLE_TYPE
  FROM RACEBASE.SPECIMEN
  WHERE
    VESSEL IN (", paste(vessel, collapse = ", "), ")",
             "AND CRUISE = ", cruise)
  ) |>
  dplyr::left_join(state_hauls,
                   by = c("VESSEL", "CRUISE", "HAUL")) |>
  dplyr::mutate(STATE_WATERS = ifelse(is.na(STATE_WATERS), "FEDERAL", "STATE"))

# Get taxonomic info
worms_taxa <- 
  RODBC::sqlQuery(
    channel = channel,
    query = "
      SELECT DISTINCT 
        SPECIES_NAME, 
        COMMON_NAME, 
        SPECIES_CODE
      FROM
        GAP_PRODUCTS.TAXONOMIC_CLASSIFICATION
      WHERE 
        DATABASE = 'WORMS'"
  )

# Get specimen types

specimen_sample_type <- 
  RODBC::sqlQuery(
    channel = channel,
    query = "
      SELECT
        SPECIMEN_SAMPLE_TYPE_ID AS SPECIMEN_SAMPLE_TYPE,
        NAME AS SAMPLE_TYPE
      FROM
        RACE_DATA.SPECIMEN_SAMPLE_TYPES"
  )


# Calculate total catch and weight
catch_by_area <- 
  catch |>
  dplyr::group_by(STATE_WATERS, SPECIES_CODE) |>
  dplyr::summarise(TOTAL_WEIGHT_KG = sum(WEIGHT, na.rm = TRUE),
                   TOTAL_COUNT = sum(NUMBER_FISH, na.rm = TRUE),
                   .groups = "keep")

catch_state <- 
  catch_by_area |>
  dplyr::filter(STATE_WATERS == "STATE") |>
  tidyr::pivot_wider(id_cols = "SPECIES_CODE", 
                     values_from = c("TOTAL_COUNT", "TOTAL_WEIGHT_KG"), 
                     names_from = "STATE_WATERS") |>
  dplyr::left_join(worms_taxa, by = "SPECIES_CODE") |>
  dplyr::arrange(-TOTAL_WEIGHT_KG_STATE) |>
  dplyr::select(COMMON_NAME, SPECIES_NAME, SPECIES_CODE, TOTAL_WEIGHT_KG_STATE, TOTAL_COUNT_STATE)

catch_total <- 
  catch_by_area |>
  dplyr::ungroup() |>
  dplyr::group_by(SPECIES_CODE) |>
  dplyr::summarise(TOTAL_WEIGHT_KG = sum(TOTAL_WEIGHT_KG, na.rm = TRUE),
                   TOTAL_COUNT = sum(TOTAL_COUNT, na.rm = TRUE)) |>
  dplyr::left_join(worms_taxa, by = "SPECIES_CODE") |>
  dplyr::arrange(-TOTAL_WEIGHT_KG) |>
  dplyr::select(COMMON_NAME, SPECIES_NAME, SPECIES_CODE, TOTAL_WEIGHT_KG, TOTAL_COUNT)


# Tally vouchers
vouchers_state <- 
  catch |>
  dplyr::filter(STATE_WATERS == "STATE", !is.na(VOUCHER)) |>
  dplyr::left_join(worms_taxa, by = "SPECIES_CODE") |>
  dplyr::group_by(COMMON_NAME, SPECIES_NAME, SPECIES_CODE) |>
  dplyr::summarise(N_RECORDS_STATE = dplyr::n(), .groups = "keep") |>
  dplyr::arrange(SPECIES_CODE) |>
  dplyr::mutate(SAMPLE_TYPE = "Voucher")

vouchers_total <- 
  catch |>
  dplyr::filter(!is.na(VOUCHER)) |>
  dplyr::left_join(worms_taxa, by = "SPECIES_CODE") |>
  dplyr::group_by(COMMON_NAME, SPECIES_NAME, SPECIES_CODE) |>
  dplyr::summarise(N_RECORDS_TOTAL = dplyr::n(), .groups = "keep") |>
  dplyr::arrange(SPECIES_CODE) |>
  dplyr::mutate(SAMPLE_TYPE = "Voucher")


# Tally specimens
specimen_state <- 
  specimen |>
  dplyr::filter(STATE_WATERS == "STATE") |>
  dplyr::left_join(worms_taxa, by = "SPECIES_CODE") |>
  dplyr::left_join(specimen_sample_type, by = "SPECIMEN_SAMPLE_TYPE") |>
  dplyr::group_by(COMMON_NAME, SPECIES_NAME, SPECIES_CODE, SAMPLE_TYPE) |>
  dplyr::summarise(N_RECORDS_STATE = n(), .groups = "keep") |>
  dplyr::arrange(-N_RECORDS_STATE)

specimen_total <- 
  specimen |>
  dplyr::left_join(worms_taxa, by = "SPECIES_CODE") |>
  dplyr::left_join(specimen_sample_type, by = "SPECIMEN_SAMPLE_TYPE") |>
  dplyr::group_by(COMMON_NAME, SPECIES_NAME, SPECIES_CODE, SAMPLE_TYPE) |>
  dplyr::summarise(N_RECORDS_TOTAL = n(), .groups = "keep") |>
  dplyr::arrange(-N_RECORDS_TOTAL)

# Combine specimen and voucher tables

specimen_voucher_state <-
  dplyr::bind_rows(
    vouchers_state,
    specimen_state
  )

specimen_voucher_total <-
  dplyr::bind_rows(
    vouchers_total,
    specimen_total
  )

# Write to .csv

write.csv(x = catch_state,
          file = here::here("assets", "adfg_report", paste0(region, "_", cruise, "_adfg_catch_state.csv")),
          row.names = FALSE)

write.csv(x = catch_total,
          file = here::here("assets", "adfg_report", paste0(region, "_", cruise, "_adfg_catch_total.csv")),
          row.names = FALSE)

write.csv(x = specimen_state,
          file = here::here("assets", "adfg_report", paste0(region, "_", cruise, "_adfg_specimens_state.csv")),
          row.names = FALSE)

write.csv(x = specimen_total,
          file = here::here("assets", "adfg_report", paste0(region, "_", cruise, "_adfg_specimens_total.csv")),
          row.names = FALSE)
