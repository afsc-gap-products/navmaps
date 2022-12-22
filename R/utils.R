#' TimeZero default color palette
#' 
#' @param n Number of colors to return
#' @param values Color values as numeric or character of colors to choose. Valid color names: "tan", "yellow", "magenta", "red", "purple", "lightgreen", "darkgreen", "cyan", "blue", "darkorange", "darkgrey", "black", "maroon", "white"
#' @param type Type of value to return (hex, gpx, kml, names).
#' @examples # View colors 
#' show_col_nav(colors = tz_pal(n = Inf, type = "hex"))
#' 
#' # Software color palette. Eight digit hex but the first two digits represent alpha channel.
#' tz_pal(n = Inf, type = "kml")
#' 
#' # TimeZero integer color palette
#' tz_pal(n = Inf, type = "gpx")
#' 
#' # Return specific indexed color values
#' tz_pal(values = c(1,3,7))
#' 
#' # Return color values by name.
#' tz_pal(values = c("tan", "magenta", "darkgreen"))
#' @export

tz_pal <- function(n = NULL, values = NULL, type = "kml") {
  
  stopifnot("Only provide  'n' or 'values'. " = (is.null(n) + is.null(values)) == 1)
  
  if(!(type %in% c("kml", "hex", "gpx", "names"))) {
    stop("Invalid type argument, ", type, "must be one of kml, hex, or gpx")
  }
  
  pal_df <- data.frame(names = c("tan", "yellow", "magenta", "red", "purple", "lightgreen", "darkgreen", "cyan", "blue", "darkorange", "darkgrey", "black", "maroon", "white"),
                         hex = c("#d2b48c", "#ffff00", "#ff00ff", "#ff0000", "#d30094", "#90ee90", 
                               "#008000", "#00ffff", "#0000ff", "#ff5a00", "#a9a9a9", "#000000", 
                               "#000080", "#FFFFFF"),
             kml = c("ff8cd4ff", "ff00ffff", "ffff00ff", "ffff0000", "ffd30094", "ff90ee90", 
                     "ff008000", "ffffff00", "ffff0000", "ff00a5ff", "ffa9a9a9", "ff000000", 
                     "ff000080", "ffffffff"),
             gpx = c(14, 9, 5, 1, 15, 8, 2, 4, 3, 11, 19, 6, 17, 6))
  
  if(!is.null(values)) {
    
    if(class(values) == "character") {
      
      if(!all(values %in% pal_df$names)) {
        stop("tz_pal: Invalid colors passed to values argument: ", 
             paste(values[!(values %in% pal_df$names)], collapse = ", ") 
             ,". List valid color names using tz_pal(n = Inf, type = 'names')")
      }
      
      values <- match(values,
                      pal_df$names)
    }
    
    sel <- pal_df[values, ]
    
  } else {
    if(is.infinite(n)) {
      n <- nrow(pal_df)
    }
    
    if(n > nrow(pal_df)) {
      stop(paste0("Number of colors (n) must be less than ", nrow(pal_df) + 1))
    }
    
    sel <- pal_df[1:n, ]
  }
  
  out <- sel[[type]]
  
  return(out)
}



#' Globe color palette
#' 
#' @param n Number of colors to return
#' @param values Color values as numeric or character of colors to choose. Valid color names: "tan", "yellow", "magenta", "red", "purple", "lightgreen", "darkgreen", "cyan", "blue", "darkorange", "darkgrey", "black", "maroon", "white"
#' @param type Type of value to return (hex, Globe integer ("integer"), Globe decimal ("decimal"), or names).
#' @examples # View colors 
#' show_col_nav(colors = globe_pal(n = Inf, type = "hex"))
#' 
#' # Globe color palette. Decimal numbers.
#' globe_pal(n = Inf, type = "decimal")
#' 
#' # Globe integer color palette
#' globe_pal(n = Inf, type = "integer")
#' 
#' # Return specific indexed color values
#' globe_pal(values = c(1,3,7))
#' 
#' # Return color values by name.
#' globe_pal(values = c("tan", "magenta", "darkgreen"))
#' @export

globe_pal <- function(n = NULL, values = NULL, type = "decimal") {
  
  stopifnot("Only provide  'n' or 'values'. " = (is.null(n) + is.null(values)) == 1)
  
  stopifnot("Only provide  'n' or 'values'. " = (is.null(n) + is.null(values)) == 1)
  
  if(!(type %in% c("decimal", "integer", "hex", "names"))) {
    stop("Invalid type argument, ", type, "must be one of decimal, integer, or hex")
  }
  
  pal_df <- data.frame(names = c("tan", "yellow", "magenta", "red", "purple", "lightgreen", "darkgreen", "cyan", "blue", "darkorange", "darkgrey", "black", "maroon", "white"),
                      hex = c("#d2b48c", "#ffff00", "#ff00ff", "#ff0000", "#d30094", "#90ee90", 
                                        "#008000", "#00ffff", "#0000ff", "#ff5a00", "#a9a9a9", "#000000", 
                                        "#000080", "#FFFFFF"),
                                        decimal = c(13808780, 65535, 16711935, 255, 16711808, 8454016, 32768, 16776960, 16711680, 33023, 8421504, 0, 4194432, 16777215),
                       integer = c(6, 14, 13, 4, 5, 10, 2, 3, 1, 12, 7, 0, 9, 15))
  
  if(!is.null(values)) {
    
    if(class(values) == "character") {
      
      if(!all(values %in% pal_df$names)) {
        stop("globe_pal: Invalid colors passed to values argument: ", 
             paste(values[!(values %in% pal_df$names)], collapse = ", ") 
             ,". List valid color names using globe_pal(n = Inf, type = 'names')")
      }
      
      values <- match(values,
                      pal_df$names)
    }
    
    sel <- pal_df[values, ]
    
  } else {
    if(is.infinite(n)) {
      n <- nrow(pal_df)
    }
    
    if(n > nrow(pal_df)) {
      stop(paste0("Number of colors (n) must be less than ", nrow(pal_df) + 1))
    }
    
    sel <- pal_df[1:n, ]
  }
  
  out <- sel[[type]]
  
  return(out)

}



#' Globe symbol palette
#' 
#' @param n Optional. Number of shapes to return.
#' @param values Shapes to return by integer index (0-55) or character (see examples for how to show valid names names).
#' @param type Type of value to return (integer, names).
#' @examples 
#' # List symbol names
#' globe_sym_pal(n = Inf, type = "names")
#' 
#' # Return symbol integer values for cross and circle
#' globe_sym_pal(values = c("cross", "circle"), type = "integer")
#'  
#' # Return symbol names by integer value
#' globe_sym_pal(values = c(2, 5), type = "names")
#' @export 

globe_sym_pal <- function(n = NULL, values = NULL, type = "integer") {
  
  stopifnot("Only provide  'n' or 'values'. " = (is.null(n) + is.null(values)) == 1)
  
  if(!(type %in% c("names", "integer"))) {
    stop("Invalid type argument, ", type, "must be names or integer")
  }
  
  sym_df <- data.frame(names = c("triangle", "diamond", "cross", "square1", "y", 
                                 "circle", "headstone1", "headstone2", "star", "asterisk",
                                 "anchor", "wreck1", "wreck2", "starsmall", "crab",
                                 "fish1", "waves", "wind", "cross", "flag",
                                 "wk", "secchi", "fish2", "nofish", "eddy_right",
                                 "eddy_left", "branch", "x", "jollyroger", "donut",
                                 "crosshairs1", "crosshairs2", "warning", "question", "pound",
                                 "dollar", "smile", "frown", "banner1", "banner2",
                                 "club", "diamond2", "heart", "spade", "uparrow",
                                 "rightarrow", "downarrow", "leftarrow", "wreck3", "wreck4",
                                 "wreck5", "lane", "bearing1", "mark1", "mark2", "bearing2"),
                       integer = c(0:55)
  )
  
  if(!is.null(values)) {
    
    if(class(values) == "character") {
      
      if(!all(values %in% sym_df$names)) {
        stop("globe_sym_pal: Invalid colors passed to values argument: ", 
             paste(values[!(values %in% sym_df$names)], collapse = ", ") 
             ,". List valid symbol names using globe_sym_pal(n = Inf, type = 'names')")
      }
      
      values <- match(values,
                      sym_df$names)
    }
    
    sel <- sym_df[values, ]
    
  } else {
    if(is.infinite(n)) {
      n <- nrow(sym_df)
    }
    
    if(n > nrow(sym_df)) {
      stop(paste0("Number of symbols (n) must be less than ", nrow(sym_df) + 1))
    }
    
    sel <- sym_df[1:n, ]
  }
  
  out <- sel[[type]]
  
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
