% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_tz_to_gps.R
\name{convert_tz_coords}
\alias{convert_tz_coords}
\title{Transform TimeZero coordinates to WGS84}
\usage{
convert_tz_coords(x)
}
\arguments{
\item{x}{A data.frame containing "Date", "X", and "Y"}
}
\value{
A data frame with transformed "X" and "Y" coordinates
}
\description{
This function converts the X and Y coordinates from a .tzdb database to WGS84 coordinates.
}
