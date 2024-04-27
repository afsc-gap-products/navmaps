#' Make csv files with trawl data
#' 
#' This function retrieves catch and haul data from RACEBASE for species in the GOA or AI and writes them to csv files. Files are used to produce charts for ArcMap.
#' 
#' @param region Region as a character vector ("ai" or "goa")
#' @param channel RODBC connection. Function will prompt for a connection if the user is not connected
#' @param species_codes A vector of species_codes as a numeric vector. Can be used to retrieve a custom group of species.
#' @export

get_trawl_data_for_charts <- function(region, channel = NULL, species_codes = NULL) {
  
  region <- tolower(region)
  
  stopifnot("get_trawl_data_for_charts: Region must be 'ai' or 'goa'" = region %in% c("ai", "goa"))
  
  channel <- navmaps::get_connected(channel)
  
  if(region == "goa" & is.null(species_codes)) {
    species_codes <- c(232, 310, 320, 420, 435, 440, 455, 471, 472, 480, 710, 10010, 10110, 10112, 10120, 10130, 10150, 10160, 10170, 10180, 10190, 10200, 10210, 10220, 10250, 10261, 10262, 10270, 10285, 20202, 20510, 21110, 21220, 21230, 21232, 21300, 21316, 21341, 21347, 21352, 21354, 21370, 21371, 21380, 21390, 21420, 21592, 21710, 21720, 21740, 21910, 21921, 21935, 22500, 23000, 23010, 23041, 23220, 23235, 23240, 24001, 24191, 30020, 30030, 30051, 30052, 30060, 30100, 30151, 30152, 30170, 30200, 30220, 30240, 30270, 30320, 30330, 30410, 30420, 30430, 30470, 30475, 30535, 30550, 30560, 30576, 30600, 66000, 66031, 66120, 78010, 78403, 79000, 79210)
  }
  
  if(region == "ai" & is.null(species_codes)) {
    species_codes <- c(320, 400, 420, 435, 455, 460, 471, 472, 475, 477, 480, 483, 485, 10110, 10112, 10115, 10120, 10130, 10170, 10180, 10200, 10210, 10220, 10261, 10262, 10270, 20510, 21230, 21232, 21300, 21316, 21341, 21347, 21352, 21354, 21370, 21390, 21420, 21438, 21720, 21740, 21921, 24001, 30020, 30051, 30052, 30060, 30151, 30152, 30420, 30475, 30535, 30576, 78010, 78403, 79210)
  }
  
  message("get_trawl_data_for_charts: Retrieving catch data")
  catch_data <- RODBC::sqlQuery(channel = channel, 
                                query = 
                                  paste0(
                                    "SELECT 
                                    H.HAULJOIN, 
                                    H.VESSEL, 
                                    H.CRUISE, 
                                    H.HAUL, 
                                    S.COMMON_NAME, 
                                    C.WEIGHT 
                                FROM 
                                    RACEBASE.CATCH C, 
                                    RACEBASE.SPECIES S, 
                                    RACEBASE.HAUL H
                                WHERE 
                                    H.HAULJOIN = C.HAULJOIN
                                    AND H.REGION = '", toupper(region), "'
                                    AND H.HAUL_TYPE = 3
                                    AND H.CRUISE > 198000
                                    AND S.SPECIES_CODE = C.SPECIES_CODE 
                                    AND C.SPECIES_CODE IN (",
                                    paste(species_codes, collapse = ", "), 
                                    ")"
                                  )
  ) |>
    dplyr::mutate(
      LABEL = toupper(
        gsub(pattern = "[^[:alnum:]]", 
             replacement = "",
             x = trimws(COMMON_NAME)
        )
      )
    ) |>
    dplyr::arrange(LABEL, CRUISE, VESSEL, HAUL) |>
    tidyr::pivot_wider(id_cols = c(HAULJOIN, VESSEL, CRUISE, HAUL), 
                       names_from = LABEL, 
                       values_from = WEIGHT,
                       values_fill = 0) |>
    as.data.frame()
  
  message("get_trawl_data_for_charts: Retrieving haul data")
  haul_data <- RODBC::sqlQuery(channel = channel,
                               query = 
                                 paste0(
                                   "SELECT * 
                              FROM 
                                RACEBASE.HAUL 
                              WHERE REGION = '", toupper(region), "' 
                                AND HAUL_TYPE = 3 
                              AND CRUISE > 198000")
  )
  
  path_catch <- here::here("output", region, paste0(region, "_CatchDataForCharts.csv"))
  path_haul <- here::here("output", region, paste0(region, "_HaulDataForCharts.csv"))
  
  message("get_trawl_data_for_charts: Writing catch data to ", path_catch)
  write.csv(catch_data, file = path_catch, row.names = FALSE)
  
  message("get_trawl_data_for_charts: Writing haul data to ", path_haul)
  write.csv(haul_data, file = path_haul, row.names = FALSE)
  
}
