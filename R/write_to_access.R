#' Write data.frame or matrix to .mdb or .accdb
#'
#'  Write data to a table in an .mdb or .accdb file. Must be run on 32-bit R, which only exists for through R 4.1! 
#' 
#' @param x data.frame to write to the database.
#' @param dsn Filepath to the .mdb or .accdb file
#' @param tablename character: a database table name accessible from the connected DSN. If missing, the name of dat. Passed to RODBC::sql
#' @param append logical. Should data be appended to an existing table? Passed to RODBC::sqlSave()
#' @param drop_existing Logical. Should the existing table be dropped and rewritten if it exists.
#' @param varTypes Optional list of variable types passed to RODBC::sqlSave(varTypes).
#' @param driver odcb driver default = Microsoft Access Driver (*.mdb)
#' @export
#' @import RODBC

write_to_access <- function(x, dsn, tablename, append = FALSE, drop_existing = TRUE, varTypes = NULL, 
                            driver = "Microsoft Access Driver (*.mdb)") {
    
    file_type <- tolower(strsplit(basename(dsn), split = "\\.")[[1]][-1])
    
    if(file_type == "mdb") {
      
      if(tolower(tablename) == "marks") {
        
        message("write_to_access: Staring marks file.")
        file.copy(from = system.file(package = "navmaps", "extdata/blank_marks.mdb"),
                  to = dsn,
                  overwrite = TRUE)
      }
      
      if(tolower(tablename) == "lines") {
        
        message("write_to_access: Starting lines file.")
        file.copy(from = system.file(package = "navmaps", "extdata/blank_lines.mdb"),
                  to = dsn,
                  overwrite = TRUE)
        
      }
      
      if(tolower(tablename) == "track") {
        
        message("write_to_access: Starting track file.")
        file.copy(from = system.file(package = "navmaps", "extdata/blank_track.mdb"),
                  to = dsn,
                  overwrite = TRUE)
  
      }
      
    }
    
    if(file_type == "accdb") {
      file.copy(from = system.file(package = "navmaps", "extdata/blank.accdb"),
                to = dsn)
    }

  message("write_to_access: Connecting to ", dsn)
  odbc_con <- RODBC::odbcDriverConnect(paste0("Driver={", driver,  "};DBQ=", dsn))
  
  if(drop_existing & (tablename %in% RODBC::sqlTables(channel = odbc_con)$TABLE_NAME)) {
    message("write_to_access: Dropping table ", tablename, " from ", dsn)
    RODBC::sqlDrop(channel = odbc_con, sqtable = tablename)
  }
  
  # Write data to tablename in odbc_con
  message("write_to_access: Saving data to ", tablename, " table in ", dsn, ". Header: ")
  # print(head(x))
  try_save <- try(RODBC::sqlSave(channel = odbc_con, 
                                  dat = x, 
                                  tablename = tablename, 
                                  append = append,
                                  rownames = FALSE,
                                 varTypes = varTypes), 
                  silent = TRUE)
  
  message("write_to_access: Closing connection to ", dsn)
  RODBC::odbcClose(odbc_con)
  
  if(class(try_save) == "try-error") {
    stop(attr(try_save, "condition"))
  }
}