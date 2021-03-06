# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This documents datasets relevent to Milwaukee, Wisconsin:
#  o mai_04042016 dataset (acquired using get_mai 04/04/2016)
#  o raw_wibrs_2005thru2015 dataset (last updated 04/13/2016)
#  o crimes.munged dataset (last updated 05/13/2016)
#  o mprop_12192016 dataset (acquired using get_mprop 012/19/2016)
# Spatial shapes:
#  o parcels dataset (acquired using get_parcels 12/19/2016)
#  o bids dataset (acquired using get_bids 12/15/2016)
#  o hoods dataset (last updated Feb 26, 2015)
#  o mkeoutline dataset (last updated Feb 26, 2015)
#  o mainroads dataset (Dec 3, 2012)
#  o wi_zipcodess dataset (2010 ZIP Code shapes for Wisconsin)
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#' mai_04042016 dataset
#'
#' This contains the Master Address Index file downloaded and processed to a
#' data frame on April 4, 2016.
#'
#' @examples
#' data(mai_04042016)
#' head(mai_04042016)
#'
#' @author Matthew Schumwinger
"mai_04042016"


#' mprop_12192016 dataset
#'
#' This contains the City of Milwaukee \href{http://itmdapps.milwaukee.gov/gis/mprop/Documentation/mprop.pdf}{MasterProperty Database (MPROP)}. The file was downloaded and processed to a data frame on December 19, 2016.
#'
#' @examples
#' head(mprop_12192016)
#' # join to parcels spdf
#' combo <- merge(parcels, mprop_12192016, by = TAXKEY, all.x = T)
#' # how much vacant land in Milwaukee?
#' v <- subset(combo, LAND_USE_GP == "13")
#' sum(as.numeric(v$LOT_AREA), na.rm = T) / sum(as.numeric(combo$LOT_AREA), na.rm = T)
#' # plot vacant land
#' plot(mkeoutline)
#' plot(subset(v, add = T, border = "brown")
#'
#' @author Matthew Schumwinger
"mprop_12192016"


#--------------
# WIBRS crime datasets
#--------------

#' raw_wibrs_2005thru2015 dataset
#'
#' This contains all WIBRS crime incident records for the entire City of
#' Milwukee, from Jan 1, 2005 through Dec 31 2015. Records were downloaded from
#' the COMPASS website. 2015 data were retreived on 04/13/2016.
#'
#' @examples
#' data(raw_wibrs_2005thru2015)
#' head(raw_wibrs_2005thru2015)
#'
#' @author Matthew Schumwinger
"raw_wibrs_2005thru2015"

#' crime.geo dataset
#'
#' This contains all WIBRS crime incident records for the entire City of
#' Milwukee, from Jan 1, 2005 through Dec 31 2015. Records were downloaded from
#' the COMPASS website. 2015 data were retreived on 04/13/2016. Data was then
#' geocoded with full_geocode(). X/Y coordinates are in the NAD27 datum. Raw
#' WIBRS data is in raw_wibrs_2005thru2015 dataset.
#'
#' @examples
#' data(crime.geo)
#' head(crime.geo)
#'
#' @author Matthew Schumwinger
"crime.geo"

#' crimes.munged dataset
#'
#' This contains all WIBRS crime incident records for the entire City of
#' Milwukee, from Jan 1, 2005 through Dec 31 2015. Records were downloaded from
#' the COMPASS website. 2015 data were retreived on 04/13/2016. Data was then
#' geocoded with full_geocode(). X/Y coordinates are in the NAD27 datum.
#'
#' The dataframe was then processed with munge_wibrs() and transformed into a
#' SpatialPointsDataFrame ready for analysis and visualization. Raw
#' WIBRS data is in raw_wibrs_2005thru2015 dataset.
#'
#' @examples
#' data(crimes.munged)
#' plot(crimes.munged)
#'
#' @author Matthew Schumwinger
"crimes.munged"

#--------------
# Spatial Shapes
#--------------

#' parcels dataset
#'
#' This contains the parcelbase shapefile downloaded and processed to a
#' SpatialPolygonsDataFrame on December 19, 2016. Projected using State Plane
#' South NAD 27 (ESPG 32054).
#'
#' @examples
#' data(parcels)
#' sp::plot(tail(parcels, 30))
#'
#' @author Matthew Schumwinger
"parcels"

#' bids dataset
#'
#' This contains the Milwaukee Business Improvement Distict (BID) shapefile
#' downloaded on December 15, 2016 and processed to a SpatialPolygonsDataFrame.
#' Projection transformed to State Plane South NAD 27 (ESPG 32054).
#'
#' @examples
#' data(bids)
#' sp::plot(bids)
#'
#' @author Matthew Schumwinger
"bids"

#' hoods dataset
#'
#' This contains the City of Milwaukee neighborhood boundary shapefile
#' downloaded on Feb 26, 2015 and processed to a SpatialPolygonsDataFrame.
#' Projected using State Plane South NAD 27 (ESPG 32054).
#'
#' @examples
#' data(hoods)
#' sp::plot(hoods)
#'
#' @author Matthew Schumwinger
"hoods"

#' mkeoutline dataset
#'
#' This contains the corporate boundary of the City of Milwaukee shapefile
#' downloaded on Feb 26, 2015 and processed to a SpatialPolygonsDataFrame.
#' Projection transformed to State Plane South NAD 27 (ESPG 32054).
#'
#' @examples
#' data(mkeoutline)
#' sp::plot(mkeoutline)
#'
#' @author Matthew Schumwinger
"mkeoutline"

#' mainroads dataset
#'
#' This contains the Open StreetMap main roads for metro Milwaukee shapefile
#' downloaded on Dec 3, 2012 and processed to a SpatialPolygonsDataFrame.
#' Mercator propjection.
#'
#' @examples
#' data(mainroads)
#' sp::plot(mainroads)
#'
#' @author Matthew Schumwinger
"mainroads"

#' wi_zipcodes dataset
#'
#' This contains the 2010 ZIP Code shapes for Wisconsin, in NAD83.
#'
#' @examples
#' data(wi_zipcodes)
#' sp::plot(wi_zipcodes)
#'
#' @author Matthew Schumwinger
"wi_zipcodes"
