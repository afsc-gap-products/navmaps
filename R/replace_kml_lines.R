#' Replace lines from a .kml file with a new line based on search pattern
#' 
#' @param kml_path Filepath to .kml file
#' @param line_pattern Pattern in the line to be replaced
#' @param replacement Full replacement
#' @export

replace_kml_lines <- function(kml_path, line_pattern, replacement) {

  kml_lines <- readLines(kml_path)
  
  kml_lines[grep(pattern = line_pattern, x = kml_lines)] <- replacement
  
  kml_con <- file(kml_path)
  
  writeLines(text = kml_lines, con = kml_con)
  
  close(kml_con)
  
}
