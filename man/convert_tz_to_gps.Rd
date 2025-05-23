% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_tz_to_gps.R
\name{convert_tz_to_gps}
\alias{convert_tz_to_gps}
\title{Read data from a TimeZero database and convert to a .gps file}
\usage{
convert_tz_to_gps(
  path_tzdb,
  output_file = NULL,
  vessel,
  cruise,
  haul,
  start = NULL,
  end = NULL
)
}
\arguments{
\item{path_tzdb}{A specific path to the TimeZero database file (.tzdb).}

\item{output_file}{Output file path as a character vector.}

\item{vessel}{RACE vessel number}

\item{cruise}{RACE cruise number}

\item{haul}{RACE haul number}

\item{start}{Optional start time as character vector POSIXct object for filtering data from the TimeZero database.}

\item{end}{Optional end time as character vector or POSIXct object for filtering ddata from the TimeZero database.}
}
\value{
A data frame containing the columns "Date", "X", and "Y" from the Timezero database.
}
\description{
This function retrieves time and position data from a TimeZero database and converts it to a .gps
file format.
}
\examples{
\dontrun{
# Path to a valid TimeZero database file
path <- system.file("extdata", "OwnShipRecorder.tzdb", package = "navmaps")

convert_tz_to_gps(path_tzdb = path,
                  output_file = NULL,
                  vessel = 176,
                  cruise = 202401,
                  haul = 22,
                  start = "01/25/2024 14:30:00",
                  end = "01/25/2024 14:33:15")
}
}
