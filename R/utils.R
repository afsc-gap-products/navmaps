#' TimeZero default color palette
#' 
#' @param n Number of colors to return
#' @param full_code Should the full eight digit hex code (e.g., "#ffffff00") be returned? If not, returns the six digit hex equivalent (without transparency; e.g. "#ffff00").
#' @export

tz_pal <- function(n, full_code = TRUE) {
  
  stopifnot("Number of colors (n) must be less than 12." = n < 12)
  if(full_code) {
    out <- c(
      "ffe6d8ad",
      "ffffff00",
      "ffff00ff",
      "ffff0000",
      "ffd30094",
      "ff90ee90",
      "ff008000",
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
      "#00a5ff",
      "#0000ff",
      "#5d5d7f",
      "#000000")[1:n]
  }
  return(out)
}
