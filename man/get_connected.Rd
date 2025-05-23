% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{get_connected}
\alias{get_connected}
\title{Create a database connection using RODBC}
\usage{
get_connected(channel = NULL, schema = NA, check_connections = TRUE)
}
\arguments{
\item{channel}{Open channel- used internally to handle connections.}

\item{schema}{Data source name (DSN) as a character vector.}

\item{check_connections}{If TRUE, checks that the user has access to the necessary tables.}
}
\value{
An RODBC class ODBC connection.
}
\description{
A function that accepts a data source name, username, and password to establish returns an Oracle DataBase Connection (ODBC) as an RODBC class in R.
}
