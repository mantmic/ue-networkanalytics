% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_functions.R
\name{get_zone_substation_voltage_histogram_data}
\alias{get_zone_substation_voltage_histogram_data}
\title{Function to get zone substation histogram data}
\usage{
get_zone_substation_voltage_histogram_data(zone_substation, interval_ts)
}
\arguments{
\item{zone_substation}{The zone substation label}

\item{interval_ts}{Timestamp to get zone substation histogram}
}
\value{
Dataframe
}
\description{
Function to get zone substation histogram data
}
\examples{
get_zone_substation_voltage_histogram_data(zone_substation = 'CDA', interval_ts = as.POSIXct('2018-05-01 12:30:00'))
}
