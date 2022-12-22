#' TimeZero default color palette
#' 
#' @param n Number of colors to return
#' @param values Color values as numeric or character of colors to choose. Valid color names: "tan", "yellow", "magenta", "red", "purple", "lightgreen", "darkgreen", "cyan", "blue", "darkorange", "darkgrey", "black", "maroon", "white"
#' @param type Type of value to return (hex, gpx, kml, names).
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
#' @param values Shapes to return by integer index value (0-55) or symbol name (see examples for how to show valid names names).
#' @param type Type of value to return (integer, names).
#' @examples 
#' # List symbol names
#' globe_sym_pal(n = Inf, type = "names")
#' 
#' # Globe integer values for triangle1, cirle1, and
#' globe_sym_pal(values = c("triangle1", "circle1", "star"), type = "integer")
#' @export 

globe_sym_pal <- function(n = NULL, values = NULL, type = "integer") {
  
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

tz_sym_pal <- function(n = NULL, values = NULL, type = "names") {
  
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
             ,". List valid symbol names using tz_sym_pal(n = Inf, type = 'names')")
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
