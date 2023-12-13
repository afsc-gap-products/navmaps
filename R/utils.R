#' Create a database connection using RODBC
#'
#' A function that accepts a data source name, username, and password to establish returns an Oracle DataBase Connection (ODBC) as an RODBC class in R.
#'
#' @param schema Data source name (DSN) as a character vector.
#' @param channel Open channel- used internally to handle connections.
#' @return An RODBC class ODBC connection.
#' @export
#' @import getPass RODBC

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
#' @keywords internal
#' @export

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
#' @keywords internal
#' @export


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
#' @keywords internal
#' @export

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
#' @keywords internal
#' @export

.check_output_path <- function(file, ext = NULL) {
  
  if(!is.null(ext)) {
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
  }
  
  if(!dir.exists(dirname(file))) {
    dir.create(dirname(file), recursive = TRUE)
  }
}



#' Check for 32-bit driver and installation of R
#' 
#' @export

.check_driver <- function() {
  
  # Check for 32-bit version of R
  address_bytes <- .Machine$sizeof.pointer 
  if(address_bytes != 4) {
    stop("Must use 32-bit verion of R to write to .mdb or .accdb. Currently using ", address_bytes*8, "-bit version. 32-bit installations were available for R version <= 4.1.")
  }
  
  # Check for Microsoft Access Driver
  stopifnot("Microsoft Access Driver not found. Check odbc::odbcListDrivers() for drivers that are currently installed." = "Microsoft Access Driver (*.mdb)" %in% odbc::odbcListDrivers()$name)
  
}



#' Check ellipsis for arguments and throw a warning if any are detected
#' 
#' @param ... Passed from higher level function
#' @keywords internal
#' @export

.check_extra_args <- function(...) {
  z <- list(...)
  
  if(length(z) > 0) {
    arg_names <- names(as.list(match.call()[-1]))
    arg_names <- arg_names[!arg_names %in% c("software_format", "geometry")]
    
    if(length(arg_names) > 0) {
      warning("Unused arguments: ", paste(arg_names, sep = ", "))
    }

  }
}



#' Check that software type is valid
#' 
#' @param software_format Character vector of the selected software type
#' @param valid_software Supported software options character vector.
#' @export

.check_software <- function(software_format, valid_software = c("globe", "timezero", "opencpn")) {
  
  if(length(software_format) > 1) {
    stop("Only one software type can be set at once.")
  }
  
  if(is.null(software_format)) {
    stop("Software type must be provided. Valid options are: ", paste(valid_software, collapse = ", "))
  }
  
  if(is.na(software_format)) {
    stop("Software type must be provided. Valid options are: ", paste(valid_software, collapse = ", "))
  }
  
  if(!(software_format %in% valid_software)) {
    stop("Invalid software name (", software_format, "). Valid options are: ", paste(valid_software, collapse = ", "))
  }
}



#' File type for a software format
#' 
#' @param software_format Character vector of the selected software type
#' @param marks File format for marks (TRUE) or lines (FALSE)
#' @export

set_file_type <- function(software_format, marks = TRUE) {
  
  if(software_format == "timezero") {
    ft <- ifelse(marks, "gpx", "kml")
  }
  
  if(software_format == "opencpn") {
    ft <- "gpx"
  }
  
  if(software_format == "globe") {
    ft <- "mdb"
  }
  
  return(ft)
}



#' Show marine navigation software palette colors
#' 
#' Show colors in a plot; function code and documentation based on show_col() function from the scales package
#' 
#' @param colors A character vector of colors.
#' @param labels Label each color with its hex name?
#' @param custom_labels Optional. Character vector to use for labels (instead of automatically using hex)
#' @param borders Border color for each tile. Default uses par("fg"). Use border = NA to omit borders.
#' @param cex_label Size of printed labels, as multiplier of default size.
#' @param ncol Number of columns. If not supplied, tries to be as square as possible.
#' @param main Plot title, passed to plot()
#' @param xlab X-axis title, passed to plot()
#' @param ylab Y-axis title, passed to plot()
#' @export
#' @import farver

show_col_nav <- function(colors = NULL, custom_labels = NULL, labels = TRUE, borders = NULL, cex_label = 1, 
                         ncol = NULL, main = NULL, xlab = "", ylab = "") {
  
  n <- length(colors)
  col_index <- 1:n
  
  if(is.null(ncol)) {
    ncol <- ceiling(sqrt(length(colors)))
  }

  nrow <- ceiling(n/ncol)
  colors <- c(colors, rep(NA, nrow * ncol - length(colors)))
  colors <- matrix(colors, ncol = ncol, byrow = FALSE)
  old <- par(pty = "s", mar = c(0, 0, 0, 0))
  on.exit(par(old))
  size <- max(dim(colors))
  plot(c(0, size), c(0, -size), type = "n", xlab = xlab, ylab = ylab, 
       axes = FALSE, main = main)
  rect(col(colors) - 1, -row(colors) + 1, col(colors), -row(colors), 
       col = colors, border = borders)
  if(labels) {
    hcl <- farver::decode_colour(colors, "rgb", "hcl")
    label_col <- ifelse(hcl[, "l"] > 50, "black", "white")
    
    if(is.null(custom_labels)) {
      color_label_text <- paste0("Value: ", col_index, "\n", colors)
    } else {
      color_label_text <- character(length = length(colors))
      color_label_text[1:length(custom_labels)] <- custom_labels
    }
    
    text(col(colors) - 0.5, -row(colors) + 0.5, color_label_text,
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
#' @param x Numeric degree decimal minute coordinats
#' @export

dmm_to_dd <- function(x) {
  abs_x <- abs(x)
  sign(x) * (abs(abs_x) %/% 100 + (abs_x %% 100)/60)
}



#' Function to convert degree-decimal minute degree-minute-second coordinates to decimal degrees
#' 
#' @param x Character string containing latitude and longitudes separated by a comma that includes hemisphere characters (see example)
#' @returns Decimal degree coordinates as a matrix with with longitude in the first column and latitude in the second column (see example)
#' @examples dms_string_to_dd("72 30.500'N, 152 00.000'W")
#' @export
#' @import stringr

dms_string_to_dd <- function(x) {
  
  fn <- function(x) {
    stopifnot("dms_string_to_dd: x must be a character vector." = is.character(x))
    stopifnot("dms_string_to_dd: x must be a 1L character vector." = length(x) == 1)
    
    # Split latitude and longitude into separate character strings
    pos_vec <- unlist(strsplit(x = x, split = ","))
    pos_vec <- trimws(pos_vec) # Remove white space
    
    stopifnot("dms_string_to_dd: x must be a character string with latitude and longitude in degrees and decimal minutes that are separated by a comma, such as: 72 30.500'N, 152 00.000'W)" = length(pos_vec) == 2)
    
    hemispheres <- stringr::str_extract(pos_vec, "[A-Z]+")
    
    # Detect axis (latitude or longitude) and sign based on N,S,E,W in strings
    axis_vec <- c(2, 2, 1, 1)[match(hemispheres, c("N", "S", "E", "W"))]
    sign_vec <- c(1, -1, 1, -1)[match(hemispheres, c("N", "S", "E", "W"))]
    
    dd_vec <- unlist(lapply(X = pos_vec, FUN = navmaps:::calc_coords)) # Calculate coordinates
    dd_vec <- dd_vec * sign_vec # Assign +/- based on hemisphere (east and north positive, west and south negative)
    dd_vec <- as.matrix(dd_vec[axis_vec], ncol = 2) # Order coordinates to have longitude first then latitude
    
    return(dd_vec)
  }
  
  out <- matrix(
    unlist(
      lapply(X = x, 
             FUN = fn)), 
         ncol = 2, byrow = TRUE)
  
  return(out)
  
}



#' Internal function called by dms_string_to_dd
#' 
#' Converts string to decimal degree longitude and latitude.
#' 
#' @param string A character string of 
#' @noRd

calc_coords <- function(string) {
  
  split_string <- unlist(
    strsplit(x = gsub("\\D+", " ", string),
             split = " "
    )
  )
  
  if( !(length(split_string) %in% c(3, 4))) {
    stop("dms_string_to_dd/calc_coords: string (", string, ") format unrecognized.")
  }

  split_string <- as.numeric(split_string)
  
  if(length(split_string) == 3) {
    dd_string <- split_string[1] + (split_string[2] + split_string[3]/1000) / 60
  }
  
  if(length(split_string) == 4) {
    dd_string <- split_string[1] + split_string[2]/60 + (split_string[3] + split_string[4]/1000) / 3600
  }

  return(dd_string)
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



#' Longitude to UTM zone
#' 
#' Identify return the UTM zone based on longitude. UTM has 60 zones, each spanning 6 degrees of longitude.
#' 
#' @param x longitude
#' @export

longitude_to_utm_zone <- function(x) {
  floor( (x + 180) / 6 ) + 1  
}



#' Find the midpoint of an sf LINESTRING
#' 
#' @param sf_lines sf object with LINESTRING geometries
#' @export

st_line_midpoints <- function(sf_lines = NULL) {
  
  .check_valid_geometry(sf_lines, valid = c("LINESTRING", "MULTILINESTRING"))
  
  g <- sf::st_geometry(sf_lines)
  
  g_mids <- lapply(g, function(x) {
    
    coords <- as.matrix(x)
    
    get_mids <- function(coords) {
      dist <- sqrt((diff(coords[, 1])^2 + (diff(coords[, 2]))^2))
      dist_mid <- sum(dist)/2
      dist_cum <- c(0, cumsum(dist))
      end_index <- which(dist_cum > dist_mid)[1]
      start_index <- end_index - 1
      start <- coords[start_index, ]
      end <- coords[end_index, ]
      dist_remaining <- dist_mid - dist_cum[start_index]
      mid <- start + (end - start) * (dist_remaining/dist[start_index])
      return(mid)
    }
    
    mids <- sf::st_point(get_mids(coords))
  })
  
  geometry <- sf::st_sfc(g_mids, crs = sf::st_crs(sf_lines))
  
  out <- sf::st_sf(geometry) |>
    dplyr::bind_cols(as.data.frame(sf_lines) |>
    dplyr::select(-geometry))
  
  return(out)
}



#' Detect geometry from an sf object
#' 
#' @param x sf object
#' @export

detect_geometry_type <- function(x) {
  
  geom_type <- table(sf::st_geometry_type(x))
  main_geom <- names(geom_type)[which.max(geom_type)]
  
  if(sum(geom_type > 1) > 1) {
    warning("Multiple geometry types detected (", names(which(geom_type > 0)), "). Using most common geometry in x (", main_geom, ")")
  } else {
    message("Geometry type detected: ", main_geom)
  }
  
  return(main_geom)
}



#' Function to remove invalid_geometries
#' 
#' Uses st_is_valid() to detect and remove invalid geometries, such as linestrings with one vertex.
#' 
#' @param x an sf object
#' @export

remove_invalid_geometry <- function(x) {
  invalid_index <- !sf::st_is_valid(x)
  
  if(length(invalid_index) > 0){
    warning("Excluding ", length(which(!sf::st_is_valid(x))), " invalid geometries.")
    
    x <- x[which(sf::st_is_valid(x)), ]
    
  }
  
  return(x)
}



#' Find midpoint between two points
#' 
#' Finds the midpoint between points. 
#' 
#' @param start_latitude Numeric vector of latitudes in decimal degrees.
#' @param start_longitude Numeric vector of longitudes in decimal degrees.
#' @param end_latitude Numeric vector of latitudes in decimal degrees.
#' @param end_longitude Numeric vector of longitudes in decimal degrees.
#' @param utm_zones Optional numeric vector indicating which UTM zone to use for coordinates. If provided, must be either 1L or the same length as coordinate vectors. If NULL (default), the UTM zones are automatically detected from the coordinates using navmaps::longitude_to_utm_zones().
#' @export

start_end_to_midpoint <- function(start_latitude,
                                  start_longitude,
                                  end_latitude,
                                  end_longitude, 
                                  utm_zones = NULL) {
  
  add_leading_zero <- function(value) {
    if (value < 10) {
      return(paste0("0", value))
    } else {
      return(as.character(value))
    }
  }
  
  n_values <- length(start_latitude)
  mid_latitude <- as.numeric(rep(NA, length(n_values)))
  mid_longitude <- as.numeric(rep(NA, length(n_values)))
  
  stopifnot("start_end_to_midpoint: start_latitude,
            start_longitude, end_latitude, and end_longitude must all be the same length." = all(c(length(start_longitude) == n_values,
                                                                                                     length(end_latitude) == n_values,
                                                                                                     length(end_longitude) == n_values)))
  
  stopifnot("start_end_to_midpoint: Invalid longitude value(s). Valid longitude range: [-180, 180]" = max(abs(c(start_longitude, end_longitude)), na.rm = TRUE) <= 180)
  
  stopifnot("start_end_to_midpoint: Invalid latitude value(s). Valid longitude range: [-90, 90]" =  max(abs(c(start_latitude, end_latitude)), na.rm = TRUE) <= 90)  

  if(is.null(utm_zones)) {
    # Detect UTM zones
    utm_zones <- navmaps::longitude_to_utm_zone(start_longitude)
    utm_zones[is.na(utm_zones)] <- navmaps::longitude_to_utm_zone(end_longitude)[is.na(utm_zones)]
    
    if(any(is.na(utm_zones))) {
      warning("No longitude values found for the following start_longitude or end_longitude indices: ", paste(which(is.na(utm_zones)), collapse = ", "))
    }
  } else {
    stopifnot("start_end_to_midpoint: utm_zone must have length 1 or be the same length as start_latitude, start_longitude, end_latitude, and end_longitude if it's provided." = any(length(utm_zones) == 1, length(utm_zones) == n_values))
  }
  
  if(length(utm_zones) == 1) {
    utm_zones <- rep(utm_zones, n_values)
  }
  
  for(ii in 1:length(start_latitude)) {
    
    if(any(is.na(start_longitude[ii]), 
           is.na(start_latitude[ii]), 
           is.na(end_longitude[ii]), 
           is.na(end_latitude[ii]), 
           is.na(utm_zones[ii]))) {
      next
    }
    
    utm_crs <- paste0("EPSG:326", add_leading_zero(utm_zones[ii]))
    
    start_pos <- sf::st_sfc(sf::st_point(c(start_longitude[ii], start_latitude[ii])), 
                            crs = "WGS84") |>
      sf::st_transform(crs = utm_crs)
    
    end_pos <- sf::st_sfc(sf::st_point(c(end_longitude[ii], end_latitude[ii])), 
                          crs = "WGS84") |>
      sf::st_transform(crs = utm_crs)
    
    approx_path <- sf::st_sfc(
      sf::st_linestring(
        rbind(
          sf::st_coordinates(start_pos), 
          sf::st_coordinates(end_pos)
        )
      ),
      crs = utm_crs
    )
    
    mid_coords <- navmaps::st_line_midpoints(approx_path) |>
      sf::st_transform(crs = "WGS84") |>
      sf::st_coordinates()
    
    mid_longitude[ii] <- mid_coords[1, 1]
    mid_latitude[ii] <- mid_coords[1, 2]
    
  }
  
  return(cbind(mid_longitude, mid_latitude))
  
}