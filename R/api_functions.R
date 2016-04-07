# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This document contains functions that acquire and assemble government data
# concerning Milwaukee, Wisconsin.
#
# The primary functions include:
#  o get_mai
#  o get_wibrs
#
# require(xlsx)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++





#' get_mai
#'
#' get_mai retrieves the Master Index file from the City of Milwaukee's server. This speeds up geocoding of large files.
#'
#' @importFrom geosphere destPoint
#'
#' @param longitude Standard Longitude in range -/+180
#' @param latitude Standard latitude in range -/+90
#' @param radius Radius in kilometers of the large circle to be covered by small cricles
#' @param max_radius_km Radius in kilometers of the small cricles (e.g. 40km)
#' @return A data frame of the (longitude,latitude) pairs for the centres of the small circles which cover the large circle
#' @export
#' @examples
#' centre_table <- createCentroidTable(-122,37,400,40)
#' plot(centre_table,pch=18)
#'
createCentroidTable <- function(longitude,latitude,radius,max_radius_km) {

}

