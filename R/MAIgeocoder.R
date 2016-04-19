# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This document contains functions that geocode addresses in Milwaukee, Wisconsin.
#
# The primary functions include:
#  o geocode_api
#  o get_mai
#  o geoprocess_mai
#  o match_mai
#  o mai_04042016 dataset (acquired using get_mai 04/04/2016)
#
# require("RCurl")
# require("jsonlite")
# require("stringr")
# require("XML")
# raw <- read.xlsx2("~/Dropbox/Analytics-Consulting/Non-client_projects/MKE data/crime/2014_WIBR_Radius99K_accessed091015.xls", 1) # accessed 9/10/15, 130 additinal records
# devtools::use_data(raw)
# data("raw") # 2014 raw data
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' get_mai
#'
#' get_mai is a funtion that retreives the City of Milwaukee
#' \href{http://itmdapps.milwaukee.gov/gis/mai/Documentation/mai.pdf}{Master
#' Address Index (MAI)}. This retrieves the XML verion, which is approximately
#' 5.5MB, and may take awhile to download. Once download is complete, the file
#' is transformed to a data frame (this may take a while).
#'
#' @import jsonlite
#' @importFrom XML xmlParse
#'
#' @param url Endpoint URL. Default = \url{http://itmdapps.milwaukee.gov/xmldata/Get_mai_xml}
#' @return A data frame.
#' @export
#' @examples
#' \dontrun{
#' mai <- get_mai()
#' head(mai)
#' }
#'
get_mai <- function(url = mai_url){
  retrieved <- Sys.Date()
  mai_url <- "http://itmdapps.milwaukee.gov/xmldata/Get_mai_xml" # as of Apr 4 2016
  message("This retrieves the XML verion, which is approximately 5.5MB and may
          take awhile to download.")
  readline(prompt="Press [enter] to continue")
  message("Downloading . . . ")
  root <- XML::xmlParse(mai_url)
  message("Download complete . . . ")
  message("Transforming XML to data frame . . . this could take several hours.")
  # readline(prompt="Press [enter] to continue")
  root_df <- xmlToDataFrame(root, homogeneous = TRUE, stringsAsFactors = TRUE) # confirm new params work
  root_df
  }


#' geoprocess_mai
#'
#' geoprocess_mai geoprocesses the City of Milwaukee
#' \href{http://itmdapps.milwaukee.gov/gis/mai/Documentation/mai.pdf}{Master
#' Address Index (MAI)}. It does this by joining the MAI to the parcelbase file
#' by TAXKEY field to associate lat/lon values with each MAI record. It also
#' adds a new "ADDRESS" field which is a concatination of existing fields.
#'
#' @importFrom sp CRS spTransform coordinates
#'
#' @param mai A dataframe of the raw Master Address Index.
#' @param parcels A SpatialPolygonsDataFrame of parcelbase file.
#' @return A data frame.
#' @export
#' @examples
#' \dontrun{
#' data("mai_04042016")
#' data("parcels")
#' geo_mai <- geoprocess_mai(mai_04042016, parcels)
#' head(geo_mai)
#' }
#'
geoprocess_mai <- function(mai, parcels){

  # promote centroid of each parcel
  parcels@data$y <- sp::coordinates(parcels)[ , 2]
  parcels@data$x <- sp::coordinates(parcels)[ , 1]

  # join lat/lon to MAI
  parcels <- parcels@data
  parcels <- parcels[!is.na(parcels$TAXKEY), ] # remove rows with NA values
  parcels <- subset(parcels, select = c("TAXKEY", "x", "y"))
  mai <- merge(mai, parcels, by = "TAXKEY", all.x=T)

  # create new address string field
  mai$STREET <- trimws(mai$STREET, which = "right")
  mai$ADDRESS <- paste(mai$HSE_NBR, mai$DIR, mai$STREET, mai$STTYPE)

  mai
}


#' geocode_api
#'
#' geocode_api is a funtion that uses City of Milwaukee Public geocode API to
#' geocode addresses. (Specifically, the \href{http://maps.milwaukee.gov/ArcGIS/rest/services/
#' geocode/MAIthenDIME_geocode/GeocodeServer}{MAI-then-DIME} method.)
#'
#' @import RCurl jsonlite stringr
#'
#' @param batch Data frame to be geocoded.
#' @param fields Character vector of batch fields containing street address.
#' @return A data frame of the original batch data frame, with new geocoding fields added.
#' @export
#' @examples
#' \dontrun{
#' raw_geo <- geocode_api(head(raw_wibrs_2005thru2015), "LOCATION")
#' raw_geo
#' }
#'
geocode_api <- function(batch, fields){
  prefix <- "http://maps.milwaukee.gov/ArcGIS/rest/services/geocode/MAIthenDIME_geocode/GeocodeServer/findAddressCandidates?Street="
  suffix <-  "&SingleLine=&outFields=Loc_name&outSR=&f=json"
  #batch[, fields] = apply(data.frame(batch[, fields]), 2, as.character) #fields must be character, data.frame/matrix
  dummy <- "error"
  e1 <- "null or bad html returned from geocoder"
  psuedo <- data.frame(dummy,
                       data.frame(dummy, dummy),
                       e1,
                       data.frame(dummy))
  psuedo[, c(1:5)] = apply(psuedo[, c(1:5)], 2, function(x) as.character(x))


  for (i in 1:dim(batch)[1]){
    result <- list()
    result$candidates <- psuedo

    add <- paste(batch[i, fields], collapse="+")
    add <- str_replace_all(add, "[[:punct:]]", " ") # remove punctuation, except " "
    add <- str_replace_all(add, "[ ]+", "+") # to make sure all spaces are single "+"
    url <- paste(prefix, add, suffix, sep="")

    print(paste("calling url for record", i, ". URL snip: [", add, "]"))
    html <- getURL(url)           # !need to handle the null address field case

    print(paste("parsing html..."))
    try(result <- fromJSON(html))   # consider using tryCatch to capture particular error message

    if(is.null(result$candidates)){
      message("geocoder error: no candidates returned")
      result$candidates <- psuedo
    } else if(!is.data.frame(result$candidates)){
      message("bad response from geocoder: candidates not a dataframe")
      result$candidates <- psuedo
    } else if(!is.list(result)){
      message("bad response from geocoder: result not a list")
      result$candidates <- psuedo
    }

    batch$geoAdd[i] <-  result$candidates[1,1]
    batch$x[i] <-  result$candidates[1,2][1]
    batch$y[i] <-  result$candidates[1,2][2]
    batch$locator[i] <-  result$candidates[1,4]
    batch$score[i] <-  result$candidates[1,3]
    print(paste("geocode score: ", batch$score[i]))
  }

  message(paste( sum(!is.na(batch$geoAdd)== TRUE)-sum(batch$geoAdd == "error"), " of ", dim(batch)[1],
                 " records successfully geocoded. (",
                 round((sum(!is.na(batch$geoAdd)== TRUE)-sum(batch$geoAdd == "error"))/(dim(batch)[1])*100, 2),
                 "%)", sep = "" ))
  message(paste(dim(subset(batch, score == 100))[1]),
          " records matched with 'score = 100' accuracy. (",
          round((dim(subset(batch, score == 100))[1])/(dim(batch)[1])*100, 2)
          , "%)", sep="")
  batch
}
