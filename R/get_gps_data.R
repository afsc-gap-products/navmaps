#' Get GPS data and write to rds files
#' 
#' @param region Survey region as a character vector. One of "ai", "goa", "sebs", "nbs"
#' @param channel RODBC connection. Function will prompt for a connection if the user is not connected
#' @export

get_gps_data <- function(region, channel = NULL) {
  
  .check_region(region)
  
  channel <- get_connected(channel = channel)
  
  if(region == "ai") {
    survey_definition_id <- 52
    gear_codes <- c(160, 172)
  }
  
  if(region == "goa") {
    survey_definition_id <- 47
    gear_codes <- c(160, 172)
  }
  
  if(region == "sebs") {
    survey_definition_id <- 98
    gear_codes <- 44
  }
  
  if(region == "nbs") {
    survey_definition_id <- 143
    gear_codes <- 44
  }

  
  message("Querying haul start/end time and position")
  path_hs <- here::here("output", region, "haul_start_end.rds")
  
  .check_output_path(file = path_hs)
  
  # Haul start/end ----
  # Get haul start/end time and position and write to an rds file
  dplyr::bind_rows(
    RODBC::sqlQuery(channel = channel, 
                    query = 
                      paste0(
                        "select c.vessel_id vessel, c.cruise, h.haul, h.haul_id hauljoin, 
                        h.performance, p.description performance_description, e.date_time, 
                        e.latitude latitude, e.longitude longitude 
                      from race_data.survey_definitions sd, race_data.surveys s, 
                        race_data.cruises c, race_data.hauls h, race_data.events e, 
                        racebase.performance p where sd.survey_definition_id = ", survey_definition_id, 
                        " and sd.survey_definition_id = s.survey_definition_id 
                        and h.gear in (", paste(gear_codes, collapse = ", "),
                        ") and s.survey_id = c.survey_id 
                        and c.cruise_id = h.cruise_id 
                        and h.haul_id = e.haul_id 
                        and h.start_timelog = e.event_type_id
                        and h.performance = p.performance"
                      )
    ) |>
      dplyr::mutate(EVENT = "start"),
    RODBC::sqlQuery(channel = channel, 
                    query = 
                      paste0(
                        "select c.vessel_id vessel, c.cruise, h.haul, h.haul_id hauljoin, 
                        h.performance, p.description performance_description, e.date_time, 
                        e.latitude latitude, e.longitude longitude
                      from race_data.survey_definitions sd, race_data.surveys s, 
                        race_data.cruises c, race_data.hauls h, race_data.events e, 
                        racebase.performance p 
                      where sd.survey_definition_id = ", survey_definition_id, 
                        " and sd.survey_definition_id = s.survey_definition_id 
                        and h.gear in (", paste(gear_codes, collapse = ", "),
                        ") and s.survey_id = c.survey_id 
                      and c.cruise_id = h.cruise_id 
                      and h.haul_id = e.haul_id 
                      and h.end_timelog = e.event_type_id
                      and h.performance = p.performance"
                      )
    ) |>
      dplyr::mutate(EVENT = "end")
  ) |>
    saveRDS(file = here::here("output", region, "haul_start_end.rds"))
  message("Haul start/end saved to ", here::here("output", region, "haul_start_end.rds"))
  
  
  
  # Retrieve GPS data ----
  # GPS data are not available from all tables. Some tables have higher quality data than others. 
  # Retrieve GPS data from tables in order of data quality (race_data final, race_data edit, 
  # unreconciled racebase2).
  
  # Make a data.frame with unique vessel/cruise
  unique_vessel_cruise <- RODBC::sqlQuery(channel = channel, 
                                          query = 
                                            paste0(
                                              "select c.vessel_id vessel, c.cruise
                                            from race_data.surveys s, race_data.cruises c
                                            where c.survey_id = s.survey_id
                                              and s.survey_definition_id = ", survey_definition_id)) |>
    dplyr::arrange(CRUISE)
  
  message("Retrieving GPS data.")
  
  # Retrieve data for each unique vessel/cruise and write to rds files
  for(ii in 1:nrow(unique_vessel_cruise)) {
    
    temp_gps <- RODBC::sqlQuery(channel = channel,
                                query = paste0(
                                  "select c.vessel_id, c.cruise, h.haul, p.edit_date_time date_time, p.latitude, 
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
      temp_gps <- RODBC::sqlQuery(channel = channel,
                                  query = paste0(
                                    "select c.vessel_id, c.cruise, h.haul, p.edit_date_time date_time, 
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
      ### Approach 3
      
      # "select vessel, cruise, haul, date_time, latitude, longitude
      #                    from race_edit.rb2_gps where vessel = ", cruise$vessel, "and cruise =", cruise$cruise,
      # " and datum_code = 1 order by date_time", sep = "")
    }
    
    if(nrow(temp_gps) > 0) {
      
      temp_gps_path <- here::here("output", region, "gps", paste0("gps_", unique_vessel_cruise$CRUISE[ii], "_", unique_vessel_cruise$VESSEL[ii], ".rds"))
      
      .check_output_path(file = temp_gps_path)
      
      message("Writing GPS data to ", temp_gps_path)
      saveRDS(object = temp_gps, file = temp_gps_path)
    } else{
      #!!!!!!! CONVERT THIS MESSAGE TO WARNING
      message("No GPS data from cruise ", unique_vessel_cruise$CRUISE[ii], " vessel ",  unique_vessel_cruise$VESSEL[ii])
    }
    
  }
  
}
