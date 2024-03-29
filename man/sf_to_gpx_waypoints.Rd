% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sf_to_gpx_waypoints.R
\name{sf_to_gpx_waypoints}
\alias{sf_to_gpx_waypoints}
\title{Write sf point data to a gpx file}
\usage{
sf_to_gpx_waypoints(
  x,
  file,
  name_col,
  description_col,
  time_col = NULL,
  color_col,
  shape_col,
  software_format = "timezero",
  ...
)
}
\arguments{
\item{x}{data.frame containing latitude, longitude, name, color, and shape columns.}

\item{file}{Output file with a .gpx extension.}

\item{name_col}{Name of the column containing names.}

\item{description_col}{Description column.}

\item{time_col}{Time column name. Optional. If not provided, writes the system time.}

\item{color_col}{Name of the column containing integer colors.}

\item{shape_col}{Name of the column containing integer shapes.}

\item{software_format}{"timezero" or "opencpn"}

\item{...}{Ignored}
}
\description{
Write sf POINT geometries to a gpx waypoint file.
}
