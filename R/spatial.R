# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This document contains functions that retreive spatial data for Milwaukee,
# Wisconsin.
#
# The primary functions include:
#  o get_parcelbase
#  o get_mprop (TODO)
#
# http://city.milwaukee.gov/ImageLibrary/Public/GIS/MapMilwaukee_GISServices.pdf
#
# __Geographic boundaries__ -- Business Improvement District (BID) and neighborhood
# boundaries were acquired from the City of Milwaukee [GIS Services REST API]
# (http://maps.milwaukee.gov/ArcGIS/rest/services/planning/Special_Districts/MapServer).
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#' get_parcels
#'
#' get_parcelbase is a function that retreives the City of Milwaukee's parcel shapefile
#' \href{http://city.milwaukee.gov/DownloadMapData3497.htm}{from the
#' city's download map data page}.
#'
#' The City of Milwaukee parcel shapefile accommodates condominium records
#' (multiple tax identifiers -- "taxkeys" -- assigned to one parcel).  Multiple
#' records (one for each taxkey) exist for condominium parcels. As a result, a
#' direct join can be made between parcelbase.shp and MPROP (or other
#' taxkey-based dataset), and condominium data will be accurately joined.
#'
#' @importFrom rgdal readOGR
#'
#' @param url Endpoint URL. Default = \url{http://itmdapps.milwaukee.gov/gis/mapdata/parcelbase.zip}
#' @return A large SpatialPolygonsDataFrame.
#' @export
#' @examples
#' \dontrun{
#' parcels <- get_parcels()
#' head(parcels@data)
#' }
#'
get_parcels <- function(url = parcel_url){
  message("This retrieves the parcelbase shapefile, which is approximately 16MB and may
          take awhile to download.")
  message("Downloading . . . ")

  retrieved <- Sys.Date()
  parcel_url <- "http://itmdapps.milwaukee.gov/gis/mapdata/parcelbase.zip" # as of Apr 15 2016
  mainDir <- getwd()
  subDir <- "temp_downloads"
  path <- paste(mainDir, subDir, sep = "/")

  temp <- tempfile()
  download.file(parcel_url, temp)
  dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
  unzip(temp, exdir = path)

  message("Download complete . . . ")
  message("Transforming shapefile to SpatialPolygonsDataFrame. . . .")
  spdf <- rgdal::readOGR(path, "parcelbase")
  unlink(temp)
  unlink(path, recursive = T)
  message("Removing `/temp_downloads` directory . . .")
  spdf
}



#' get_bids
#'
#' get_bids is a function that retreives the City of Milwaukee's Business
#' Improvement District (BID) kml file from the City's \href{http://maps.milwaukee.gov/ArcGIS/rest/services/planning/Special_Districts/MapServer/3/query}{GIS Services REST API}.
#'
#' @importFrom rgdal readOGR
#' @importFrom rvest html_session html_form submit_form
#'
#' @return A large SpatialPolygonsDataFrame.
#' @export
#' @examples
#' \dontrun{
#' bids <- get_bids()
#' sp::plot(bids)
#' head(bids@data)
#' }
#'
get_bids <- function(){

  # create session; populate login form
  start <- "http://maps.milwaukee.gov/ArcGIS/rest/services/planning/Special_Districts/MapServer/3/query"
  pgsession <- rvest::html_session(start)
  pgform    <- rvest::html_form(pgsession)

  names(pgform) <- "foo" # form must be named to compute on it
  # names(pgform$foo$fields)[10] <- "GET"
  # pgform$foo$fields$GET$name <- "GET"
  pgform$foo$fields$text$value <- "%"   # this is the SQL wildcard, which retreives all BIDS
  pgform$foo$fields$returnCountOnly$value <- "false" # necesseary for KMD submission
  pgform$foo$fields$returnIdsOnly$value <- "false"   # necesseary for KMD submission
  pgform$foo$fields$f$value <- "kmz"

  # returns the zipped (binary) KMZ file
  mykmz <- rvest::submit_form(pgsession, pgform$foo)

  # convert to spatial object
  zz <- file("testbin.zip", "wb")
  writeBin(t$response$content, zz)
  close(zz)
  tt <- unzip("mykmz.zip")
  spdf <- rgdal::readOGR("doc.kml", "Business Improvement Districts (BID)")
  unlink("mykmz.zip")
  unlink("doc.kml")
  spdf
}
