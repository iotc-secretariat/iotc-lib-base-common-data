% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iotc_base_common_data_factors.R
\name{factorize_IUCN_status}
\alias{factorize_IUCN_status}
\title{Factorizes the IUCN status codes in a data frame}
\usage{
factorize_IUCN_status(to_factorize, connection = DB_IOTDB())
}
\arguments{
\item{to_factorize}{The data frame whose \code{IUCN_STATUS} column should be factorized}

\item{connection}{A connection to \code{\link{IOTDB}}}
}
\value{
the original data frame with the \code{IUCN_STATUS} factorized according to the default sequence of IUCN status codes
}
\description{
Factorizes the IUCN status codes in a data frame
}
