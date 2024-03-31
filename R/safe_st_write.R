#' Make shapeifles valid and write
#' 
#' Wrapper around sf functions st_make_valid() and st_write(). See documentation for st_make_valid() and st_write() functions for additional arguments. sf argument descritpions are copied from the corresponding sf functions.
#' 
#' @param obj	object of class sf or sfc
#' @param dsn	data source name. Interpretation varies by driver: can be a filename, a folder, a database name, or a Database Connection
#' @param append logical; should we append to an existing layer, or replace it? if TRUE append, if FALSE replace. The default for st_write is NA which raises an error if the layer exists. The default for write_sf is FALSE, which overwrites any existing data. See also next two arguments for more control on overwrite behavior.
#' @param ... Addition arguments passed to st_write()
#' @export

safe_st_write <- function(obj, dsn, append, ...) {
  
  invalid <- which(is.na(sf::st_is_valid(obj)))
  
  if(length(invalid) > 0) {
    message("safe_st_write: removing ", length(invalid), " uncorrectable geometries from obj.")
    obj <- obj[-invalid, ]
  }
  
  obj |>
    sf::st_write(dsn = dsn, 
                 append = FALSE, 
                 ...)
  
}