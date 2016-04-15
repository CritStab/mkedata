# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This documents datasets relevent to Milwaukee, Wisconsin:
#  o mai_04042016 dataset (acquired using get_mai 04/04/2016)
#  o parcels dataset (acquired using get_parcels 04/15/2016)
#  o raw_wibrs_2005thru2015 dataset (last updated 04/13/2016)
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

#' parcels dataset
#'
#' This contains the parcelbase shapefile downloaded and processed to a
#' SpatialPolygonsDataFrame on April 15, 2016.
#'
#' @examples
#' data(parcels)
#' head(parcels@data)
#'
#' @author Matthew Schumwinger
"parcels"



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
