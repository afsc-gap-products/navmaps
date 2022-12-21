#' Write data.frame or matrix to .mdb or .accdb
#'
#' Connect to .mdb or .accdb file and writes data to a table. Must be run on 32-bit R, which only exists for through R 4.1! 
#' 
#' @param dsn Filepath to the .mdb or .accdb file
#' @param data data.frame to be written to the table
#' @param tablename character: a database table name accessible from the connected DSN. If missing, the name of dat. Passed to RODBC::sql
#' @param append logical. Should data be appended to an existing table? Passed to RODBC::sqlSave()
#' @param drop_existing Logical. Should the existing table be dropped and rewritten if it exists.
#' @export

write_to_mdb <- function(x, dsn, tablename, append = FALSE, drop_existing = TRUE) {
  
  if(!file.exists(dsn)) {
    message("No existing dsn object found at ", dsn, ". Creating a new file.")
    file.copy(from = system.file(package = "navmaps", "extdata/blank.mdb"),
              to = dsn)
  }

  odbc_con <- RODBC::odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", dsn))
  
  if(drop_existing & (tablename %in% RODBC::sqlTables(channel = odbc_con)$TABLE_NAME)) {
    message("Dropping table ", tablename, " from ", dsn)
    RODBC::sqlDrop(channel = odbc_con, sqtable = tablename)
  }
  
  sqlReturn <- RODBC::sqlSave(channel = odbc_con, 
                              dat = x, 
                              tablename = tablename, 
                              append = append,
                              rownames = FALSE)
  
  RODBC::odbcClose(odbc_con)
  
  return(sqlReturn)
}