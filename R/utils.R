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
