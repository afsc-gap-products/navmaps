#' TimeZero default color palette
#' 
#' @param n Number of colors to return
#' @param full_code Should the full eight digit hex code (e.g., "#ffffff00") be returned? If not, returns the six digit hex equivalent (without transparency; e.g. "#ffff00").
#' @export

tz_pal <- function(n, full_code = TRUE) {
  
  stopifnot("Number of colors (n) must be less than 13." = n < 13)
  if(full_code) {
    out <- c(
      "ffe6d8ad",
      "ffffff00",
      "ffff00ff",
      "ffff0000",
      "ffd30094",
      "ff90ee90",
      "ff008000",
      "00008000",
      "ff00a5ff",
      "ff0000ff",
      "ff5d5d7f",
      "ff000000")[1:n]
  } else {
    out <- c(
      "#e6d8ad",
      "#ffff00",
      "#ff00ff",
      "#ff0000",
      "#d30094",
      "#90ee90",
      "#008000",
      "#008000",
      "#00a5ff",
      "#0000ff",
      "#5d5d7f",
      "#000000")[1:n]
  }
  return(out)
}



#' Create a database connection using RODBC
#'
#' A function that accepts a data source name, username, and password to establish returns an Oracle DataBase Connection (ODBC) as an RODBC class in R.
#'
#' @param schema Data source name (DSN) as a character vector.
#' @return An RODBC class ODBC connection.
#' @export

get_connected <- function(channel = NULL, schema = NA){
  if(is.null(channel)) {
    (echo = FALSE)
    if(is.na(schema)) {
      schema <- getPass::getPass(msg = "Enter ORACLE schema: ")
    }
    username <- getPass::getPass(msg = "Enter your ORACLE Username: ")
    password <- getPass::getPass(msg = "Enter your ORACLE Password: ")
    channel  <- RODBC::odbcConnect(dsn = paste(schema),
                                   uid = paste(username),
                                   pwd = paste(password),
                                   believeNRows = FALSE)
  }
  return(channel)
}


#' Check that columns exist in a data.frame/sf
#' 
#' Internal function
#' 
#' @param x data.frame or sf object
#' @param var_cols Variable columns that must be included in the data.frame
#' @param func_name Parent function name
#' @export
#' @keywords internal

.check_cols_exist <- function(x, var_cols, func_name = NULL) {
  missing_cols <- var_cols[which(!(var_cols %in% names(x)))]
  
  if(length(missing_cols) >=1) {
    stop(paste0(func_name, 
                ": The following variable columns were not found in the input: ", 
                paste(missing_cols, collapse = ", ")))
  }
}

#' Check that region is valid
#' 
#' Internal function
#' 
#' @param x character vector
#' @export
#' @keywords internal

.check_region <- function(x) {
  if(!(x %in% c("sebs", "nbs", "ai", "goa"))) {
    stop("Invalid region! Must be 'sebs', 'nbs', 'ai', or 'goa'")
  }
}

#' Check that sf geometry is valid
#' 
#' Internal function
#'
#' @param x sf object
#' @param valid character vector of valid geometries
#' @export
#' @keywords internal

.check_valid_geometry <- function(x, valid) {
  if(!all(sf::st_geometry_type(x) %in% valid)) {
    unique_geometry <- unique(sf::st_geometry_type(x))
    invalid_geometry <- unique_geometry[which(!(unique_geometry %in% valid))]
    
    stop(paste0("Invalid geometry type(s) found in x: ", 
                paste(invalid_geometry, collapse = ", "), 
                ". All geometries must be one of ", paste(valid, collapse = ", ")))
  }
}

#' Check that filepath is valid and output directory exists
#' 
#' Internal function
#' 
#' @param file Path to directory
#' @param ext Character vector of valid file extensions (e.g. c(".kml", ".gpx")).
#' @export
#' @keywords internal

.check_output_path <- function(file, ext) {
  
  ext_check <- c()
  for(hh in 1:length(ext)) {
    ext_check <- c(ext_check, grepl(pattern = ext[hh], x = file, ignore.case = TRUE))
  }
  
  if(!any(ext_check)) {
    
    if(length(ext) == 1) {
      stop("File extension must be ", ext)
    } else {
      stop("File extension must be one of ", paste(ext, collapse = ", "))
    }

  }
  
  if(!dir.exists(dirname(file))) {
    dir.create(dirname(file), recursive = TRUE)
  }
}
