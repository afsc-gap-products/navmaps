#' Read Globe data from .mdb or .accdb and convert to simple features
#' 
#' Read Globe marks or lines from a Microsoft Access Database (.mdb or .accdb) and convert to an sf POINT or LINESTRING object.
#' 
#' @param dsn Path to a Globe .mdb, .accdb, .csv., .xls, or .xlsx file.
#' @param tablename Optional. table name as a character vector (typically "marks" or "lines"). The function attempts to detect the layer type if not provided.
#' @param wkt_geometry_type Optional. Geometry type for output as a character vector; must be either POINT or LINESTRING. Typically "POINT" for marks or "LINESTRING" for lines.
#' @param grouping_col Optional. Grouping column for LINESTRINGs.
#' @param keep_index Logical. Should index values from database files be retained?
#' @param driver odcb driver default = Microsoft Access Driver (*.mdb)
#' @return A simple features object
#' @export

globe_to_sf <- function(dsn, tablename = NULL, wkt_geometry_type = NULL, grouping_col = NULL, 
                        keep_index = TRUE, driver = "Microsoft Access Driver (*.mdb)") {
  
  file_ext <- tolower(sub(pattern = ".*\\.", replacement = "", x = dsn))
  
  if(!file.exists(dsn)) {
    stop("read_globe: ", dsn, " not found.")
  }
  
  if(file_ext %in% c("accdb", "mdb")) {
    
    .check_driver()
    
    message("write_to_access: Connecting to ", dsn)
    odbc_con <- RODBC::odbcDriverConnect(paste0("Driver={", driver, "};DBQ=", dsn))
    
    if(is.null(tablename)) {
      tablename <- c("lines", "marks", "Lines", "Marks", "track", "Track")[which(
        c("lines", "marks", "Lines", "Marks", "track", "Track") %in% 
          RODBC::sqlTables(channel = odbc_con)$TABLE_NAME)]
      
      stopifnot("read_globe: tablename argument was not specified and the Access database did not 
               contain EXACTLY one table named lines, marks, or track. Please provide the tablename 
               argument in the read_globe() function call." = length(tablename) == 1)
    }
    
    message("read_globe: Reading data from ", dsn)
    dat <- RODBC::sqlQuery(channel = odbc_con, query = paste0("select * from ", tablename))
    
  }
  
  if(file_ext %in% c("xlsx", "xls")) {
    dat <- readxl::read_excel(dsn)
  }
  
  if(file_ext == "csv") {
    dat <- utils::read.csv(dsn)
  }
  
  message("read_globe: Imported ", nrow(dat), " lines from ", dsn)
  
  # When wkt_geometry_type is not provided, attempt to detect object type (LINESTRING or POINTS) 
  # automatically by checking for blank lines between coordinates
  if(is.null(wkt_geometry_type)) {
    
    diff_lat <- is.na(diff(dat$Latitude))
    
    if(sum(diff_lat[1:(length(diff_lat)-1)]) > 0) {
      wkt_geometry_type <- "LINESTRING"
    } else {
      wkt_geometry_type <- "POINT"
    }
    
  } else {
    wkt_geometry_type <- toupper(wkt_geometry_type)
    
    stopifnot("read_globe: wkt_geometry_type must be LINESTRING or POINT." = wkt_geometry_type %in% c("LINESTRING", "POINT"))
    
  }
  
  # Convert latitude and longitude from radians to decimal degrees 
  dat$Latitude <- radians_to_dd(dat$Latitude)
  dat$Longitude <- radians_to_dd(dat$Longitude)
  
  if(is.null(grouping_col)) {
    message("read_globe: Setting grouping column")
    dat$temp_grp_col <- cumsum(is.na(dat$Latitude) & is.na(dat$Longitude))
    grouping_col <- "temp_grp_col"
  }

  if(any(c("Index", "index", "INDEX") %in% names(dat))) {
    names(dat)[which(names(dat) %in% c("Index", "index", "INDEX"))[1]] <- "INDEX"
  } else {
    dat$INDEX <- 1:nrow(dat)
  }
  
  message("read_globe: Making sf POINT object")
  out <- dat |>
    dplyr::filter(!is.na(Latitude), !is.na(Longitude)) |>
    sf::st_as_sf(coords = c("Longitude", "Latitude"), 
                 crs = "EPSG:4326")
  
  if(wkt_geometry_type == "LINESTRING") {
    
    message("read_globe: Converting to LINESTRING")
    out <- dplyr::rename(out, id = grouping_col) |>
      dplyr::select(id, INDEX, geometry) |>
      unique()
    
    n_obj <- out |>
      dplyr::group_by(id) |>
      dplyr::summarise(n = n()) |>
      dplyr::filter(n < 2)
    
    if(nrow(n_obj) > 0) {
      warning("read_globe: POINTS detected in globe lines table.", nrow(n_obj), " points removed)")
      out <- dplyr::filter(out, !(id %in% c(n_obj$id)))
    } 
    
    out <- out |>
      dplyr::arrange(INDEX) |>
      dplyr::group_by(id) |>
      dplyr::summarise(do_union = FALSE, 
                       INDEX = paste(range(INDEX), collapse = "-")) |>
      dplyr::group_by(id, INDEX) |>
      sf::st_cast(to = "LINESTRING") |>
      sf::st_wrap_dateline() |>
      dplyr::mutate(id = as.character(id))
    
  }
  
  if(!keep_index & any(c("Index", "index", "INDEX") %in% names(out))) {
    out <- out[, -which(names(out) %in% c("Index", "index", "INDEX"))]
  }
  
  if("temp_grp_col" %in% names(out)) {
    out <- out[, -which(names(out) %in% "temp_grp_col")]
  }
  
  try(RODBC::odbcClose(odbc_con), silent = TRUE)
  
  return(out)
  
}