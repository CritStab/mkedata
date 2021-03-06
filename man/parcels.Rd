% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasets.R
\docType{data}
\name{parcels}
\alias{parcels}
\title{parcels dataset}
\format{An object of class \code{SpatialPolygonsDataFrame} with 160708 rows and 6 columns.}
\usage{
parcels
}
\description{
This contains the parcelbase shapefile downloaded and processed to a
SpatialPolygonsDataFrame on December 19, 2016. Projected using State Plane
South NAD 27 (ESPG 32054).
}
\examples{
data(parcels)
sp::plot(tail(parcels, 30))

}
\author{
Matthew Schumwinger
}
\keyword{datasets}

