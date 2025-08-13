#' Get GPS data and write to rds files
#' 
#' @param region Survey region as a character vector. One of "ai", "goa", "sebs", "nbs"
#' @param channel RODBC connection. Function will prompt for a connection if the user is not connected
#' @export
#' @import RODBC

get_gps_data <- function(region, channel = NULL) {
  
  channel <- get_connected(channel = channel)
  
  # Check access to Oracle tables
  .check_table_access(
    channel = channel,
    schema = "race_data",
    table_name = 
      c("survey_definitions", 
        "surveys", 
        "cruises", 
        "hauls", 
        "events", 
        "position_headers", 
        "positions", 
        "datum_codes", 
        "haul_performance_notes", 
        "haul_performance_codes")
  )
  
  .check_table_access(
    channel = channel,
    schema = "racebase",
    table_name = "haul"
  )
  
  .check_table_access(
    channel = channel,
    schema = "race_edit",
    table_name = "rb2_gps"
  )
  
  .check_region(region)
  
  if(region == "ai") {
    survey_definition_id <- 52 # AI bottom-trawl survey
    gear_codes <- c(160, 172) # Poly'noreastern
  }
  
  if(region == "goa") {
    survey_definition_id <- 47 # GOA bottom-trawl survey
    gear_codes <- c(160, 172) # Poly'noreastern
  }
  
  if(region == "sebs") {
    survey_definition_id <- 98 # EBS shelf bottom-trawl survey
    gear_codes <- 44 # 83-112
  }
  
  if(region == "nbs") {
    survey_definition_id <- 143 # NBS bottom-trawl survey
    gear_codes <- 44 # 83-112
  }
  
  if(region == "slope") {
    survey_definition_id <- 78 # EBS slope bottom-trawl survey
    gear_codes <- c(160, 172) # Poly'noreastern
  }
  
  
  # Haul start/end ----
  # Get haul start/end time and position and write to an rds file
  
  path_hs <- here::here("output", region, paste0(region, "_haul_start_end.rds"))
  .check_output_path(file = path_hs)
  
  message("Querying haul start/end time and position")
  
  performance_codes_rb <- 
    RODBC::sqlQuery(
      channel = channel,
      query = "select performance, description performance_description from racebase.performance")
  
  performance_codes_rd <- 
    RODBC::sqlQuery(
      channel = channel,
      query = "select hpn.note performance_description, hpc.haul_performance_code performance from race_data.haul_performance_notes hpn, 
                  race_data.haul_performance_codes hpc 
                  where hpc.HAUL_PERFORMANCE_NOTE_ID = hpn.HAUL_PERFORMANCE_NOTE_ID"
    ) |>
    dplyr::filter(!(PERFORMANCE %in% performance_codes_rb$PERFORMANCE))
  
  performance_codes <- dplyr::bind_rows(performance_codes_rb,
                                        performance_codes_rd)
  
  start_end <- 
    dplyr::bind_rows(
      RODBC::sqlQuery(
        channel = channel, 
        query = 
          paste0(
            "select c.vessel_id vessel, c.cruise, h.haul, h.haul_id hauljoin, 
                        h.performance, e.date_time, 
                        e.latitude latitude, e.longitude longitude, h.bottom_depth, 
                        h.gear_temperature
                      from race_data.survey_definitions sd, race_data.surveys s, 
                        race_data.cruises c, race_data.hauls h, race_data.events e 
                        where sd.survey_definition_id = ", survey_definition_id, 
            " and sd.survey_definition_id = s.survey_definition_id 
                        and h.gear in (", paste(gear_codes, collapse = ", "),
            ") and s.survey_id = c.survey_id 
                        and c.cruise_id = h.cruise_id 
                        and h.haul_id = e.haul_id 
                        and h.start_timelog = e.event_type_id"
          )
      ) |>
        dplyr::mutate(EVENT = "start"),
      RODBC::sqlQuery(
        channel = channel, 
        query = 
          paste0(
            "select c.vessel_id vessel, c.cruise, h.haul, h.haul_id hauljoin, e.date_time, 
                        h.performance, e.latitude latitude, e.longitude longitude, h.bottom_depth, 
                        h.gear_temperature
                      from race_data.survey_definitions sd, race_data.surveys s, 
                        race_data.cruises c, race_data.hauls h, race_data.events e
                      where sd.survey_definition_id = ", survey_definition_id, 
            " and sd.survey_definition_id = s.survey_definition_id 
                        and h.gear in (", paste(gear_codes, collapse = ", "),
            ") and s.survey_id = c.survey_id 
                      and c.cruise_id = h.cruise_id 
                      and h.haul_id = e.haul_id 
                      and h.end_timelog = e.event_type_id"
          )
      ) |>
        dplyr::mutate(EVENT = "end")
    ) |>
    dplyr::inner_join(performance_codes, by = "PERFORMANCE")
  
  start_end_rb <- dplyr::bind_rows(
    RODBC::sqlQuery(
      channel = channel,
      query = 
        paste0(
          "select h.vessel, h.cruise, h.haul, h.start_longitude longitude, 
                                  h.start_latitude latitude, h.performance, h.start_time date_time, 
                                  h.bottom_depth, h.gear_temperature
                                  from racebase.haul h, race_data.cruises c, race_data.surveys s
                                  where s.survey_definition_id = ", survey_definition_id, 
          " and s.survey_id = c.survey_id 
                                  and h.vessel = c.vessel_id 
                                  and h.cruise = c.cruise 
                                  and h.gear in (", paste(gear_codes, collapse = ", "),
          ")")
    ) |>
      dplyr::mutate(EVENT = "start"),
    RODBC::sqlQuery(
      channel = channel,
      query = 
        paste0(
          "select h.vessel, h.cruise, h.haul, end_longitude longitude, 
                          h.end_latitude latitude, h.performance, h.start_time date_time, 
                          h.bottom_depth, h.gear_temperature, h.duration
                           from racebase.haul h, race_data.cruises c, race_data.surveys s
                           where s.survey_definition_id = ", survey_definition_id, 
          " and s.survey_id = c.survey_id 
                           and h.vessel = c.vessel_id 
                           and h.cruise = c.cruise 
                           and h.gear in (", paste(gear_codes, collapse = ", "),
          ")")
    ) |>
      dplyr::mutate(EVENT = "end",
                    DATE_TIME = DATE_TIME + DURATION * 3600) |>
      dplyr::select(-DURATION)
  ) |>
    dplyr::inner_join(performance_codes, by = "PERFORMANCE") |> 
    dplyr::anti_join(dplyr::select(start_end, VESSEL, CRUISE, HAUL, EVENT),
                     by = c("VESSEL", "CRUISE", "HAUL", "EVENT"))
  
  RODBC::odbcTables(channel = channel, tableName = "racebase.haul")
  
  start_end <- dplyr::bind_rows(start_end, start_end_rb)
  
  saveRDS(object = start_end, 
          file = path_hs)
  
  unique_start_end_hauls <- dplyr::select(start_end, VESSEL, CRUISE, HAUL) |>
    unique()
  
  message("Haul start/end saved to ", path_hs)
  
  # Retrieve GPS data ----
  # GPS data are not available from all tables. Some tables have higher quality data than others. 
  # Retrieve GPS data from tables in order of data quality (race_data final, race_data edit, 
  # unreconciled racebase2).
  
  # Make a data.frame with unique vessel/cruise
  unique_vessel_cruise <- 
    RODBC::sqlQuery(
      channel = channel, 
      query = 
        paste0(
          "select c.vessel_id vessel, c.cruise
                                            from race_data.surveys s, race_data.cruises c
                                            where c.survey_id = s.survey_id
                                              and s.survey_definition_id = ", survey_definition_id)
    ) |>
    dplyr::arrange(CRUISE)
  
  message("Retrieving GPS data.")
  
  # Retrieve data for each unique vessel/cruise and write to rds files
  for(ii in 1:nrow(unique_vessel_cruise)) {
    
    temp_gps <- 
      RODBC::sqlQuery(
        channel = channel,
        query = paste0(
          "select c.vessel_id vessel, c.cruise, h.haul, p.edit_date_time date_time, p.latitude, 
                  p.longitude 
                from race_data.cruises c, 
                  race_data.hauls h, race_data.position_headers ph, race_data.positions p, 
                  race_data.datum_codes dc 
                where c.cruise_id = h.cruise_id 
                  and h.haul_id = ph.haul_id
                  and ph.position_header_id = p.position_header_id 
                  and p.datum_code = dc.datum_code
                  and dc.use_in_analysis = 'Y'
                 and c.cruise = ", unique_vessel_cruise$CRUISE[ii], 
          " and c.vessel_id = ", unique_vessel_cruise$VESSEL[ii],
          " and h.gear in (", paste(gear_codes, collapse = ", "), ")")
      )
    
    if(nrow(temp_gps) == 0) {
      
      temp_gps <- 
        RODBC::sqlQuery(
          channel = channel,
          query = paste0(
            "select c.vessel_id vessel, c.cruise, h.haul, p.edit_date_time date_time, 
                                    p.edit_latitude latitude, p.edit_longitude longitude 
                                  from race_data.cruises c, race_data.hauls h, 
                                    race_data.edit_position_headers ph, race_data.edit_positions p, 
                                    race_data.datum_codes dc 
                                  where c.cruise_id = h.cruise_id 
                                    and h.haul_id = ph.haul_id
                                    and ph.position_header_id = p.position_header_id 
                                    and p.datum_code = dc.datum_code 
                                    and dc.use_in_analysis = 'Y' 
                                    and c.cruise = ", unique_vessel_cruise$CRUISE[ii], 
            " and c.vessel_id = ", unique_vessel_cruise$VESSEL[ii],
            " and h.gear in (", paste(gear_codes, collapse = ", "), ")")
        )
      
    }
    
    if(nrow(temp_gps) == 0) {
      
      temp_gps <- 
        RODBC::sqlQuery(
          channel = channel,
          query = paste0(
            "select rbg.vessel, rbg.cruise, rbg.haul, rbg.date_time, 
                                      rbg.latitude, rbg.longitude
                                    from race_edit.rb2_gps rbg, racebase.haul r
                                    where rbg.datum_code = 1
                                      and r.haul = rbg.haul
                                      and r.cruise = rbg.cruise
                                      and r.vessel = rbg.vessel
                                    and rbg.vessel = ", unique_vessel_cruise$VESSEL[ii],
            " and rbg.cruise = ", unique_vessel_cruise$CRUISE[ii],
            " and r.gear in (", paste(gear_codes, collapse = ", "), ")"
          )
        )
      
    }
    
    # Check for missing GPS data
    
    sel_start_end <- dplyr::filter(
      unique_start_end_hauls, 
      VESSEL == unique_vessel_cruise$VESSEL[ii], 
      CRUISE == unique_vessel_cruise$CRUISE[ii]
    )
    
    unique_gps_hauls <- dplyr::select(temp_gps, VESSEL, CRUISE, HAUL) |>
      unique()
    
    if(nrow(unique_gps_hauls) == 0) {
      missing_gps <- sel_start_end
    } else {
      missing_gps <- dplyr::anti_join(unique_gps_hauls, 
                                      sel_start_end, 
                                      by = c("VESSEL", "CRUISE", "HAUL"))
    }
    
    if(nrow(missing_gps) > 0) {
      
      fill_missing <- dplyr::inner_join(start_end, 
                                        missing_gps, 
                                        by = c("VESSEL", "CRUISE", "HAUL")) |>
        dplyr::select(VESSEL, CRUISE, HAUL, DATE_TIME, LATITUDE, LONGITUDE, EVENT) |>
        dplyr::arrange(VESSEL, CRUISE, HAUL, desc(EVENT)) |>
        dplyr::select(-EVENT)
      
      if(nrow(temp_gps) > 0) {
        temp_gps <- dplyr::bind_rows(temp_gps, fill_missing)
      } else {
        temp_gps <- fill_missing
      }
      
    }
    
    if(nrow(temp_gps) > 0) {
      
      temp_gps_path <- 
        here::here("output", region, "gps", 
                   paste0("raw_gps_", unique_vessel_cruise$CRUISE[ii], "_", unique_vessel_cruise$VESSEL[ii], ".rds")
        )
      
      .check_output_path(file = temp_gps_path)
      
      message("Writing GPS data to ", temp_gps_path)
      
      saveRDS(object = temp_gps, file = temp_gps_path)
      
    } else {
      message("No GPS data from cruise ", unique_vessel_cruise$CRUISE[ii], " vessel ",  unique_vessel_cruise$VESSEL[ii])
    }
    
  }
  
}
