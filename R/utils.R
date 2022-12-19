#' TimeZero default color palette
#' 
#' @param n Number of colors to return
#' @param software_code Should the full eight digit hex code (e.g., "#ffffff00") be returned? If not, returns the six digit hex equivalent (without transparency; e.g. "#ffff00").
#' @export

tz_pal <- function(n, software_code = TRUE) {
  
  pal_vec <- c(
    "#e6d8ad",
    "#ffff00",
    "#ff00ff",
    "#ff0000",
    "#d30094",
    "#90ee90",
    "#008000",
    "#00a5ff",
    "#0000ff",
    "#5d5d7f",
    "#000000")
  
  if(is.infinite(n)) {
    n <- length(pal_vec)
  }
  
  if(n > length(pal_vec)) {
    stop(paste0("Number of colors (n) must be less than ", length(pal_vec) + 1))
  }
  
  if(software_code) {
    out <- gsub(pattern = "#", replacement = "ff", x = pal_vec)[1:n]
  } else {
    out <- pal_vec[1:n]
  }
  return(out)
}


#' Globe color palette
#' 
#' @param n Number of colors to return
#' @param software_code Should the Globe decimal color be returned (e.g., 255 for red, 12632256 for grey) be returned? If not, returns the six digit hex equivalent (e.g. "#ff0000" for red, ).
#' @export

globe_pal <- function(n, software_code = TRUE) {
  
  pal_vec <- c(
    12632256,
    32768,
    255,
    6684927,
    16737894)
  
  if(is.infinite(n)) {
    n <- length(pal_vec)
  }
  
  if(n > length(pal_vec)) {
    stop(paste0("Number of colors (n) must be less than ", length(pal_vec) + 1))
  }
  
  if(software_code) {
    out <- pal_vec[1:n]
  } else {
    out <- d10_to_hex_color(pal_vec[1:n])
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



#' Show marine navigation software palette colors
#' 
#' Show colors in a plot; function code and documentation based on show_col() function from the scales package
#' 
#' @param colors A character vector of colors.
#' @param labels Label each colour with its hex name?
#' @param borders Border colour for each tile. Default uses par("fg"). Use border = NA to omit borders.
#' @param cex_label Size of printed labels, as multiplier of default size.
#' @param ncol Number of columns. If not supplied, tries to be as square as possible.
#' @export

show_col_nav <- function(colors = NULL, labels = TRUE, borders = NULL, cex_label = 1, 
                         ncol = NULL) 
{
  
  n <- length(colors)
  col_index <- 1:n
  
  if(is.null(ncol)) {
    ncol <- ceiling(sqrt(length(colors)))
  }

  nrow <- ceiling(n/ncol)
  colors <- c(colors, rep(NA, nrow * ncol - length(colors)))
  colors <- matrix(colors, ncol = ncol, byrow = TRUE)
  old <- par(pty = "s", mar = c(0, 0, 0, 0))
  on.exit(par(old))
  size <- max(dim(colors))
  plot(c(0, size), c(0, -size), type = "n", xlab = "", ylab = "", 
       axes = FALSE)
  rect(col(colors) - 1, -row(colors) + 1, col(colors), -row(colors), 
       col = colors, border = borders)
  if (labels) {
    hcl <- farver::decode_colour(colors, "rgb", "hcl")
    label_col <- ifelse(hcl[, "l"] > 50, "black", "white")
    text(col(colors) - 0.5, -row(colors) + 0.5, paste0("Option: ", col_index, "\n", colors), 
         cex = cex_label, col = label_col)
  }
}



#' Convert decimal degrees to radians
#' 
#' @param x Numeric vector.
#' @export

dd_to_radians <- function(x) {
  x*pi/180
}


#' Convert decimal degrees to radians
#' 
#' @param x Numeric vector.
#' @export

radians_to_dd <- function(x) {
  x*180/pi
}

#' Convert dmm format to decimal degrees
#'
#' Convert degree decimal minute coordinates to decimal degrees (e.g. 16530.5 to 165.5083)

dmm_to_dd <- function(x) {
  abs_x <- abs(x)
  sign(x) * (abs(abs_x) %/% 100 + (abs_x %% 100)/60)
}

#' Convert decimal to hex color
#' 
#' Convert a base 16 decimal color to a hex color.
#' 
#' @param x Numeric vector
#' @examples # Convert Globe decimal color for grey to hex (#C0C0C0)
#' d10_to_hex_color(12632256)
#' @export

d10_to_hex_color <- function(x) {
  
  hex_vec <- c(0:9, "A", "B",	"C", "D", "E", "F")
  
  d1 <- hex_vec[x %% 16 + 1]
  quo <- floor(x/16)
  
  d2 <- hex_vec[quo %% 16 + 1]
  quo <- floor(quo/16)
  
  d3 <- hex_vec[quo %% 16 + 1]
  quo <- floor(quo/16)
  
  d4 <- hex_vec[quo %% 16 + 1]
  quo <- floor(quo/16)
  
  d5 <- hex_vec[quo %% 16 + 1]
  quo <- floor(quo/16)
  
  d6 <- hex_vec[quo %% 16 + 1]
  
  return(paste0("#", apply(X = cbind(d6, d5, d4, d3, d2, d1),
                           MARGIN = 1,
                           FUN = paste, collapse = "")))
}
