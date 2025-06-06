% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iotc_base_common_data_factors.R
\name{all_wps}
\alias{all_wps}
\title{Extracts all details of the \code{[meta].WORKING_PARTIES} table as available on \code{IOTDB}}
\usage{
all_wps(connection = DB_IOTDB())
}
\arguments{
\item{connection}{A connection to the \code{\link{IOTDB}}}
}
\value{
a data frame with multiple rows (one for each working party) and the required columns to model the structure of the table
}
\description{
Extracts all details of the \code{[meta].WORKING_PARTIES} table as available on \code{IOTDB}
}
