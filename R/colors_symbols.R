#' Retrieve colors for a software and file format
#' 
#' @param values Color values as a character vector (see ?tz_pal or ?globe_pal for color options)
#' @param software_format Software format as a character vector.
#' @param file_type File type for OpenCPN or TimeZero ("kml" or "gpx")
#' @param ... Ignored
#' @export

navmaps_pal <- function(values, software_format, file_type = NULL, ...) {
  
  .check_extra_args(...)
  
  stopifnot("navmaps_pal: values must be a character class. " = is.character(values))
  
  if(software_format == "globe") {
    out_col <- globe_pal(values = values, type = "integer")
  }
  
  if(software_format == "timezero") {
    
    stopifnot("navmaps_pal: Must specify 'file_type' when software_format is 'timezero'" = is.character(file_type))
    
    if(file_type == "kml") {
      out_col <- tz_pal(values = values, type = "kml")
    }
    
    if(file_type == "gpx") {
      out_col <- tz_pal(values = values, type = "gpx")
    }
  }
  
  if(software_format == "opencpn") {
    out_col <- opencpn_pal(values = values, type = "gpx")
  }
  
  return(out_col)
}



#' Retrieve colors for a software and file format
#' 
#' @param values Symbol values as a character vector (see ?tz_sym_pal or ?globe_sym_pal for shape options)
#' @param software_format Software as a character vector.
#' @param file_type File type for OpenCPN or TimeZero ("kml" or "gpx")
#' @param ... Ignored
#' @export

navmaps_sym_pal <- function(values, software_format, color = NULL, file_type = NULL, ...) {
  
  .check_extra_args(...)
  
  stopifnot("navmaps_sym_pal: values must be a character class. " = is.character(values))
  
  if(software_format == "globe") {
    out_col <- globe_sym_pal(values = values, type = "integer")
  }
  
  if(software_format == "timezero") {
    
    if(file_type == "kml") {
      out_col <- tz_sym_pal(values = values, type = "kml")
    }
    
    if(file_type == "gpx") {
      out_col <- tz_sym_pal(values = values, type = "gpx")
    }
  }
  
  if(software_format == "opencpn") {
    out_col <- opencpn_sym_pal(values = values, color = color, type = "gpx")
  }
  
  return(out_col)
}



#' TimeZero default color palette
#' 
#' @param n Number of colors to return
#' @param values Color values as numeric or character of colors to choose. Valid color names: "tan", "yellow", "magenta", "red", "purple", "lightgreen", "darkgreen", "cyan", "blue", "darkorange", "darkgrey", "black", "maroon", "white"
#' @param type Type of value to return (hex, gpx, kml, names).
#' @param ... Ignored
#' @examples 
#' # List color names
#' tz_pal(n = Inf, type = "names")
#' 
#' # View colors 
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

tz_pal <- function(n = NULL, values = NULL, type = "kml", ...) {
  
  .check_extra_args(...)
  
  stopifnot("Only provide  'n' or 'values'. " = (is.null(n) + is.null(values)) == 1)
  
  if(!(type %in% c("kml", "hex", "gpx", "names"))) {
    stop("Invalid type argument, ", type, "must be one of kml, hex, or gpx")
  }
  
  pal_df <- data.frame(names = c("tan", "yellow", "magenta", "red", "purple", "lightgreen", "darkgreen", "cyan", "blue", "darkorange", "darkgrey", "black", "maroon", "white"),
                       hex = c("#d2b48c", "#ffff00", "#ff00ff", "#ff0000", "#d30094", "#90ee90", 
                               "#008000", "#00ffff", "#0000ff", "#ff5a00", "#a9a9a9", "#000000", 
                               "#000080", "#FFFFFF"),
                       kml = c("ff8cd4ff", "ff00ffff", "ff00ffff", "ff0000ff", "ffd30094", "ff90ee90", 
                               "ff008000", "ffffff00", "ffff00ff", "ff00a5ff", "ffa9a9a9", "ff000000", 
                               "ff000080", "ffffffff"),
                       gpx = c(14, 9, 5, 1, 15, 8, 2, 4, 3, 11, 19, 6, 17, 6))
  
  if(class(values) == "character") {
    values <- find_closest_color(input_color = values, valid_color_names = pal_df$names)
  }
  
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
#' @param ... Ignored
#' @examples 
#' # List color names
#' globe_pal(n = Inf, type = "names")
#' 
#' # View colors 
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

globe_pal <- function(n = NULL, values = NULL, type = "decimal", ...) {
  
  .check_extra_args(...)
  
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
  
  if(class(values) == "character") {
    values <- find_closest_color(input_color = values, valid_color_names = pal_df$names)
  }
  
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



#' OpenCPN color palette
#' 
#' @param n Number of colors to return
#' @param values Color values as numeric or character of colors to choose. Valid color names: "yellow", "magenta", "red", "green", "blue", "darkorange", "black", "white"
#' @param type Type of value to return (hex, gpx, names).
#' @param ... Ignored
#' @examples 
#' # List color names
#' opencpn_pal(n = Inf, type = "names")
#' 
#' # View colors 
#' show_col_nav(colors = opencpn_pal(n = Inf, type = "hex"))
#' 
#' # Software color palette. Eight digit hex but the first two digits represent alpha channel.
#' opencpn_pal(n = Inf, type = "kml")
#' 
#' # OpenCPN named gpx color palette
#' opencpn_pal(n = Inf, type = "gpx")
#' 
#' # Return specific indexed color values
#' opencpn_pal(values = c(1,3,7))
#' @export

opencpn_pal <- function(n = NULL, values = NULL, type = "gpx", fill_missing_color = TRUE, ...) {
  
  .check_extra_args(...)
  
  stopifnot("Only provide  'n' or 'values'. " = (is.null(n) + is.null(values)) == 1)
  
  stopifnot("Only provide  'n' or 'values'. " = (is.null(n) + is.null(values)) == 1)
  
  if(!(type %in% c("gpx", "hex", "names"))) {
    stop("Invalid type argument, ", type, "must be one of gpx, hex, or names.")
  }
  
  pal_df <- data.frame(names = c("yellow", "magenta", "red", "green", "blue", "darkorange",  "black", "white"),
                       hex = c("#ffff00", "#ff00ff", "#ff0000", "#00ff00", "#0000ff", "#ff5a00",  "#000000", "#FFFFFF"),
                       gpx = c("Yellow", "Magenta", "Red", "Green", "Blue", "Orange", "Black", "White"))
  
  if(class(values) == "character") {
      values <- find_closest_color(input_color = values, valid_color_names = pal_df$names)
  }
  
  if(!is.null(values)) {
    
    if(class(values) == "character") {
      
      if(!all(values %in% pal_df$names)) {
        stop("opencpn_pal: Invalid colors passed to values argument: ", 
             paste(values[!(values %in% pal_df$names)], collapse = ", ") 
             , ". List valid color names using opencpn_pal(n = Inf, type = 'names')")
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
#' @param values Shapes to return by integer index value (0-55) or symbol name (see examples for how to show valid names names).
#' @param type Type of value to return (integer, names).
#' @param ... Ignored
#' @examples 
#' # List symbol names
#' globe_sym_pal(n = Inf, type = "names")
#' 
#' # Globe integer values for triangle1, cirle1, and
#' globe_sym_pal(values = c("triangle1", "circle1", "star"), type = "integer")
#' @export 

globe_sym_pal <- function(n = NULL, values = NULL, type = "integer", ...) {
  
  .check_extra_args(...)
  
  stopifnot("Only provide  'n' or 'values'. " = (is.null(n) + is.null(values)) == 1)
  
  if(!(type %in% c("names", "integer"))) {
    stop("Invalid type argument, ", type, "must be names or integer")
  }
  
  sym_df <- data.frame(names = c("triangle1", "diamond", "cross", "square1", "y", 
                                 "circle1", "headstone1", "headstone2", "star", "asterisk",
                                 "anchor", "wreck1", "wreck2", "starsmall", "crab",
                                 "fish1", "waves", "wind", "cross", "flag",
                                 "wk", "marker", "fish2", "nofish", "eddy_right",
                                 "eddy_left", "branch", "x", "skull", "donut",
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



#' TimeZero symbol palette
#' 
#' @param n Optional. Number of shapes to return.
#' @param values Shapes to return by gpx value (integer), kml value (URL as a character) or symbol name (see examples for how to show valid names names).
#' @param type Type of value to return (gpx, kml, names).
#' @param ... Ignored
#' @examples 
#' # List symbol names
#' tz_sym_pal(n = Inf, type = "names")
#' 
#' # gpx symbol values for triangle1, circle1, and star
#' tz_sym_pal(values = c("triangle1", "circle1", "star"), type = "gpx")
#'  
#' # kml symbol values for triangle1, circle1, and star
#' tz_sym_pal(values = c("triangle1", "circle1", "star"), type = "kml")
#' @export 

tz_sym_pal <- function(n = NULL, values = NULL, type = "names", ...) {
  
  .check_extra_args(...)
  
  stopifnot("Only provide  'n' or 'values'. " = (is.null(n) + is.null(values)) == 1)
  
  if(!(type %in% c("names", "kml", "gpx"))) {
    stop("Invalid type argument, ", type, "must be names or integer")
  }
  
  sym_df <- data.frame(names = c('circle2', 'square2', 'diamond', "star", 'fish1', 'fish2', 'fish3', 'fish4', 'shrimp', 'crab', 'wreck1', 'food', 'gas', 'anchor', 'warning', 'speaker', 'flag1', 'skull', 'house', 'x', 'banner1', 'bell', 'martini', 'boat2', 'boat1', 'reef', 'mark3', 'mark2', 'mark1', 'wind', 'waterski', 'fish5', 'triangle2', 'circle1', 'square1', 'triangle1', 'mark4', 'noanchor', 'bird2', 'bird1', 'circle3', 'circle4', 'crosshairs1', 'crosshairs3', 'marker', 'asterisk', 'crosshairs2', 'wreck2'),
                       gpx = c(0:31, 50:57, 60:67),
                       kml = paste0("http://www.maxsea.fr/TimeZero/Images/Icons/MaxSea_Rec_",  
                                    c(paste0("0", c(0:9)), 10:31, 50:57, 60:67),
                                    ".png")
  )
  
  if(!is.null(values)) {
    
    if(class(values) == "character") {
      
      if(!all(values %in% sym_df$names)) {
        stop("globe_sym_pal: Invalid symbol passed to values argument: ", 
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



#' OpenCPN symbol palette
#' 
#' @param n Optional. Number of shapes to return.
#' @param values Shapes to return by gpx value (integer), kml value (URL as a character) or symbol name (see examples for how to show valid names names).
#' @param color Optional. Color for OpenCPN symbols to replace black. Passed to opencpn_pal().
#' @param type Type of value to return (gpx, kml, names).
#' @param ... Ignored
#' @export

opencpn_sym_pal <- function(n = NULL, values = NULL, color = NULL, type = "names", ...) {
  
  .check_extra_args(...)
  
  stopifnot("Only provide  'n' or 'values'. " = (is.null(n) + is.null(values)) == 1)
  
  if(!(type %in% c("names", "gpx"))) {
    stop("Invalid type argument, ", type, "must be names or integer")
  }
  
  sym_df <- data.frame(names = c('anchor', 'anchor1', 'anchor2', 'circle1', 'diamond', 'warning', 'circle2',  'circle3', 'crosshairs1', 'question', 'triangle1', 'square1', 'star',  'marker1', 'triangle2', 'x', 'marker', "asterisk"),
                       gpx = c("Symbol-Anchor1", "Symbol-Anchor2", "Symbol-Anchor3", "Symbol-Circle-Black", "Symbol-Diamond-Black", "Symbol-Exclamation-Black", "Symbol-Glow-LargeBlack", "Symbol-Glow-Small-Black", "Symbol-Pin-Black", "Symbol-Question-Black", "Symbol-Spot-Black", "Symbol-Square-Black", "Symbol-Star-Black", "Symbol-Tick-Black", "Symbol-Triangle", "Symbol-X-Large-Black", "Symbol-X-Small-Black", "Symbol-X-Large-Black"))
  
  if(!is.null(values)) {
    
    if(class(values) == "character") {
      
      if(!all(values %in% sym_df$names)) {
        stop("opencpn_sym_pal: Invalid symbol passed to values argument: ", 
             paste(values[!(values %in% sym_df$names)], collapse = ", ") 
             ,". List valid symbol names using opencpn_sym_pal(n = Inf, type = 'names')")
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
  
  if(!is.null(color) & type == "gpx") {
    out <- gsub(x = out,
                pattern = "Black", 
                replacement = opencpn_pal(values = color, 
                                          type = "gpx", 
                                          fill_missing_color = TRUE))
  }
  
  return(out)
  
}



#' Find closest color
#' 
#' Find the color that's closest to a user-provided input color in RGB space.
#' 
#' @param input_color Input color name as a character vector.
#' @param valid_color_names Chracter vector of valid colors.
#' @examples find_closest_color(input_color = "blue", valid_color_names = c("green", "red", "deepskyblue2"))
#' find_closest_color(input_color = c("magenta", "darkgreen"), valid_color_names = c("green", "red", "deepskyblue2"))
#' @export

find_closest_color <- function(input_color, valid_color_names) {
  
  nn <- length(input_color)
  
  # Find RGB color
  color_df <- data.frame(color = valid_color_names, 
                         rgb = t(grDevices::col2rgb(valid_color_names)))
  
  # Convert input color to RGB
  input_rgb <- grDevices::col2rgb(input_color)
  
  color_vec <- character(length = nn)
  
  for(ii in 1:nn) {
    # Calculate the euclidean distance between the input color and each predefined color
    distances <- apply(color_df[, 2:4], 1, function(x) {
      sqrt(sum((as.numeric(input_rgb[,ii]) - x)^2))
    })
    
    color_vec[ii] <- color_df$color[which.min(distances)]
    
  }

  return(color_vec)
}
