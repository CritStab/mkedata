% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crime_functions.R
\name{crimes_by_group_year}
\alias{crimes_by_group_year}
\title{crimes_by_group_year}
\usage{
crimes_by_group_year(spdf)
}
\arguments{
\item{spdf}{A SpatialPointsDataFrame of WIBRS data.}
}
\value{
A SpatialPointsDataFrame in CRS NAD27.
}
\description{
crimes_by_group_year aggregates munged geo-processed WIBRS data by offense
group and by year.
}
\examples{
data(crimes.munged)
agg <- crimes_by_group_year(crimes.munged)
}

