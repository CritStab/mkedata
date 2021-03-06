% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spatial.R
\name{get_bids}
\alias{get_bids}
\title{get_bids}
\usage{
get_bids()
}
\value{
A large SpatialPolygonsDataFrame.
}
\description{
get_bids is a function that retreives the City of Milwaukee's Business
Improvement District (BID) kml file from the City's \href{http://maps2.milwaukee.gov/ArcGIS/rest/services/planning/special_districts/MapServer/3/query}{GIS Services REST API}. Projection is transformed to
the State Plane South NAD 27 (ESPG 32054) datum.
}
\examples{
\dontrun{
bids <- get_bids()
summary(bids@data)
plot(mkeoutline)
plot(test, col = "red",add = T)
}

}

