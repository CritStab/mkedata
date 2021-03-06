# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This document contains functions that retreive spatial data for Milwaukee,
# Wisconsin.
#
# The primary functions include:
#  o get_parcels
#  o get_bids
#  o get_mprop (TODO)
#
# Convenience functions:
#  o to_nad27
#  o to_wgs84
#
# Business Improvement District (BID) and neighborhood
# boundaries were acquired from the City of Milwaukee [GIS Services REST API]
# (http://maps2.milwaukee.gov/ArcGIS/rest/services/planning/special_districts/MapServer).
# http://city.milwaukee.gov/ImageLibrary/Public/GIS/MapMilwaukee_GISServices.pdf
# http://city.milwaukee.gov/ImageLibrary/Public/GIS/MMNewsletters/MapMilwaukee201610_MAPS2.pdf
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
#' plot(mkeoutline)
#' plot(head(parcels, 500), col= "red", border = "red", add = T)
#' }
#'
get_parcels <- function(url = parcel_url){
  message("This retrieves the parcelbase shapefile, which is approximately 15MB and may
          take awhile to download.")
  message("Downloading . . . ")

  retrieved <- Sys.Date()
  parcel_url <- "http://itmdapps.milwaukee.gov/gis/mapdata/parcelbase.zip" # as of Dec 15 2016
  mainDir <- getwd()
  subDir <- "temp_downloads"
  path <- paste(mainDir, subDir, sep = "/")

  temp <- tempfile()
  download.file(parcel_url, temp)
  dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
  unzip(temp, exdir = path)

  message("Download complete . . . ")
  message("Transforming shapefile to SpatialPolygonsDataFrame. . . .")
  spdf <- rgdal::readOGR(path, "parcelbase", stringsAsFactors = FALSE)
  unlink(temp)
  unlink(path, recursive = T)
  message("Removing `/temp_downloads` directory . . .")
  spdf
}



#' get_bids
#'
#' get_bids is a function that retreives the City of Milwaukee's Business
#' Improvement District (BID) kml file from the City's \href{http://maps2.milwaukee.gov/ArcGIS/rest/services/planning/special_districts/MapServer/3/query}{GIS Services REST API}. Projection is transformed to
#' the State Plane South NAD 27 (ESPG 32054) datum.
#'
#' @importFrom rgdal readOGR
#' @importFrom rvest html_session html_form submit_form
#'
#' @return A large SpatialPolygonsDataFrame.
#' @export
#' @examples
#' \dontrun{
#' bids <- get_bids()
#' summary(bids@data)
#' plot(mkeoutline)
#' plot(test, col = "red",add = T)
#' }
#'
get_bids <- function(){

  # create session; populate login form
  start <- "http://maps2.milwaukee.gov/ArcGIS/rest/services/planning/special_districts/MapServer/3/query"
  pgsession <- rvest::html_session(start)
  pgform    <- rvest::html_form(pgsession)

  names(pgform) <- "foo" # form must be named to compute on it
  pgform$foo$fields$text$value <- "%"   # this is the SQL wildcard, which retreives all BIDS
  pgform$foo$fields$returnCountOnly$value <- "false" # necesseary for KMZ submission
  pgform$foo$fields$returnIdsOnly$value <- "false"   # necesseary for KMZ submission
  pgform$foo$fields$returnDistinctValues$value <- "false"   # necesseary for KMZ submission
  pgform$foo$fields$f$value <- "kmz"

  # returns the zipped (binary) KMZ file
  mykmz <- rvest::submit_form(pgsession, pgform$foo)

  # convert to spatial object
  zz <- file("mykmz.kmz", "wb")
  writeBin(mykmz$response$content, zz)
  close(zz)
  tt <- unzip("mykmz.kmz")
  spdf <- rgdal::readOGR("doc.kml", "Business Improvement Districts (BID)")
  unlink("mykmz.zip")
  unlink("doc.kml")
  spdf <- to_nad27(spdf) # change projection to NAD27

  # plot as side effect for verification purposes
  plot(spdf)

  return(spdf)
}

#' to_nad27
#'
#' to_nad27 is a convenience function that transforms the CRS of an sp class
#' object to the State Plane South NAD 27 (ESPG 32054) datum.
#'
#' @importFrom sp spTransform
#'
#' @return An sp class object.
#' @export
#' @examples
#' \dontrun{
#' # plot ZIP codes in and around Milwuakee
#' wi_zipcodes <- to_nad27(wi_zipcodes)
#' plot(mkeoutline, border = "red", lwd = 5)
#' plot(wi_zipcodes, add = T)
#' }
#'
to_nad27 <- function(x){
  crs <- CRS("+proj=lcc +lat_1=42.73333333333333 +lat_2=44.06666666666667 +lat_0=42 +lon_0=-90 +x_0=609601.2192024385 +y_0=0 +datum=NAD27
+units=us-ft +no_defs +ellps=clrk66 +nadgrids=@conus,@alaska,@ntv2_0.gsb,@ntv1_can.dat ")
  spTransform(x, crs)
}

#' to_wgs84
#'
#' to_wgs84 is a convenience function that transforms the CRS of an sp class
#' object to the WGS 1984 (EPSG:4326) coordinate system.
#'
#' @importFrom sp spTransform
#'
#' @return An sp class object.
#' @export
#' @examples
#' \dontrun{
#' # plot 2015 homicides on web contextual layer
#' h2015 <- subset(crimes.munged, OFFENSE1 == "HOMICIDE" & year == "2015")
#' h2015 <- to_wgs84(h2015) # projection used for webmaps
#' m <- data.frame(slot(h2015, "coords"), slot(h2015, "data"))
#' names(m)[1:2] <- c("lon", "lat")
#' }
#'
to_wgs84 <- function(x){
  crs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  spTransform(x, crs)
}

#' get_mprop
#'
#' get_mprop is a funtion that retreives the City of Milwaukee
#' \href{http://itmdapps.milwaukee.gov/gis/mprop/Documentation/mprop.pdf}{Master
#' Property Database (MPROP)}. This retrieves the MS Excel verion, which is approximately
#' 59MB, and may take awhile to download. Once download is complete, the file
#' is transformed to a data frame.
#'
#' @importFrom readxl read_excel
#'
#' @param url Endpoint URL. Default = \url{http://itmdapps.milwaukee.gov/xmldata/Get_mprop_xsl}
#' @return A data frame.
#' @export
#' @examples
#' dat <- get_mprop()
#' str(dat)
#' hist(as.numeric(dat$BEDROOMS), breaks = 200)
#'
get_mprop <- function(url = url){
  retrieved <- Sys.Date()
  url <- "http://itmdapps.milwaukee.gov/xmldata/Get_mprop_xsl" # as of Dec 15 2016
  message("This retrieves the Excel version, which is approximately 59MB and may
          take awhile to download.")
  readline(prompt="Press [enter] to continue")
  message("Downloading . . . ")

  temp <- tempfile(fileext = ".xlsx")
  download.file(url, temp)
  data <- readxl::read_excel(temp)
  unlink(temp)

  message("Transforming to data frame . . . ")
  data
}
