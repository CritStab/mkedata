# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This document contains a draft function that geocodes addresses in Milwaukee,
# Wisconsin. This draft version geocodes crime data, first by matching against
# geoprocessed MAI, then using a low-level function to access the City geocode API.
#
# Note: this is high volume; send to geocoder API only those sets not matched by
# MAI.
# Note: any MAI score less than 95 could be suspect, especially on street
# direction error, DIME over 90 look good
# records with score 100 about 21% ; records with score >= 95 about 29%
#
# TODO:
#  o generalize to any type of records; use field param
#  o include instuctions on preparing `field` param to geocode on
#  o improve example, use more wieldy data, plot
#  o consider using  http://www.mapquestapi.com/geocoding/

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' full_geocode
#'
#' full_geocode is a function that geocodes addresses in Milwaukee,
#' Wisconsin. Draft version geocodes crime data, first by matching against
#' geoprocessed MAI, then using low_level funciton to access City geocode API.
#'
#' @importFrom dplyr summarise
#' @import data.table
#'
#' @param df A dataframe with City of Milwaukee records to geocode.
#' @param field (NOT IMPLEMENTED)Character string of single column name containing street address
#' information to geocode. (Note: you may need to concatenate prior to using
#' this function).
#' @param MAI A data frame containing the Master Address Index, already
#' geoprocessed against parcel centroids.
#' @return A data frame.
#' @export
#' @examples
#' \dontrun{
#' data("raw_wibrs_2005thru2015")
#' crime_subset <- raw_wibrs_2005thru2015[sample(nrow(raw_wibrs_2005thru2015), 30), ]
#' data("geo_mai")
#' geo_crime <- full_geocode(crime_subset, "LOCATION", geo_mai)
#' head(geo_crime)
#' }
#'
full_geocode <- function(df, field, MAI){

  # TODO: generalize these out
  # raw_wibrs_2005thru2015 <- dataframe
  # geo_mai <- MAI

  #####################
  ## MATCH TO MAI    ##
  #####################

  # merge MAI geo data to crime records where possible
  crime <- data.table::as.data.table(df)
  crime$INCIDENT_N <- as.character(crime$INCIDENT_N ) # these are mostly unique
  mai <- data.table::as.data.table(MAI)
  data.table::setkey(crime, LOCATION)
  data.table::setkey(mai, ADDRESS)
  mai.u <- unique(mai)
  crimeMAI <- mai.u[crime] # left outer join

  # direct match rate to MAI: ~ 78%
  # TODO: investigate differences between below calcs
  table(!is.na(crimeMAI$x))[2] / nrow(crime) # compare to:
  table(crime$LOCATION %in% mai$ADDRESS)[2] / nrow(crime)
  paste0(nrow(crime[crime$LOCATION == "", ]), " records without LOCATION data (mostly sex crimes)")
  paste0("Direct match rate to the MAI: ",
         round(table(!is.na(crimeMAI$x))[2] / nrow(crime), 4)*100, "%")
  crimeMAI <- crimeMAI[, c(16:27), with = FALSE] # remove unessessry columns #TODO generalize

  ################################
  ## PREPARE FOR API GEOCODER   ##
  ################################

  # unique unmatched addresses
  data.table::setkey(crimeMAI, ADDRESS)
  u <- unique(crimeMAI) # unique addresses
  uu <- u[is.na(u$x), ] # and unmatched

  # cull bad stuff known to fail geocoder
  paste0(length(grep("/", uu$ADDRESS)), " intersection locations in set; not sent to geocode API")
  uu <- uu[!grep("/", uu$ADDRESS), ] # intersections generally fail in City API

  # split by district to feed chunks to geocoder
  # d <- uu$DISTRICT
  # uu.split <- split(as.data.frame(uu), d, drop=TRUE)
  # #test <- lapply(uu.split, head, 2)

  #####################
  ## GEOCODE DATA    ##
  #####################

  begin <- Sys.time()
  message("Use City MAI-then-DIME REST API to match remaining addresses")
  message("NOTE: this will take approximately ", round(nrow(uu)/120/60), " hours to complete
          (roughly 1 minute per 120 records).")
  # test <- uu[1200:1220, ]
  geo.data <- tryCatch(geocode_api(uu, "ADDRESS"))
  # geo.data <- tryCatch(lapply(uu.split, geocode, "ADDRESS")) # original call if spliting into lists
  end <- Sys.time()
  message("time elapsed for API calls: ", round(end-begin, 2), " hours")

  ######################
  ## POSTPROCESS DATA ##
  ######################
  # geo.data <- do.call("rbind", geo.data) # return list to one standard data table
  tt <- geo.data
  tt$x <- as.numeric(tt$x)
  tt$y <- as.numeric(tt$y)
  tt$locator <- as.character(tt$locator)
  tt <- subset(tt, select = c("ADDRESS", # remove unessessry columns
                              "x",
                              "y",
                              "geoAdd",
                              "locator",
                              "score"))
  tt <- data.table::as.data.table(tt)
  data.table::setkey(tt, ADDRESS)

  # left outer join to full crime dataset
  merged <- tt[crimeMAI]

  # consolidate data, columns
  merged <- as.data.frame(merged)
  nrow(merged[is.na(merged$x), ])
  merged$x[is.na(merged$x)] <- merged$i.x[is.na(merged$x)]
  merged$y[is.na(merged$y)] <- merged$i.y[is.na(merged$y)]
  nrow(merged[is.na(merged$x), ])
  merged$i.x <- NULL
  merged$i.y <- NULL

  # standardize score, locator
  merged[!is.na(merged$x) & is.na(merged$geoAdd), "score"] <- "100"
  merged[!is.na(merged$x) & is.na(merged$geoAdd), "locator"] <- "direct match"
  merged[grep("/", merged$ADDRESS), "locator"] <- "BA-intersection"
  merged[merged$ADDRESS == "", "locator"] <- "BA-redacted"
  merged$locator <- as.factor(merged$locator)

  crime.geo <- merged

  ######################
  ## DIAGNOSTICS      ##
  ######################

  # TODO: make score numeric calculate % "good" scores
  dplyr::summarise(dplyr::group_by(crime.geo, locator),
                   records = length(locator),
                   percent = round(length(locator)/nrow(crime.geo)*100, 2))

  paste0("Percent records with no or unusable locations: ",
         round(nrow(crime.geo[grep("BA", crime.geo$locator), ]) / nrow(crime), 4)*100, "%")
  paste0("Geocoding match rate ",
         round(
           # geocode matches
           nrow(subset(crime.geo, locator == "direct match" |
                         locator == "MAI_geocode" |
                         locator == "DIME_geocode")) /
             # total, less BAs
             (nrow(crime) -  nrow(crime.geo[grep("BA", crime.geo$locator), ])),
           6)*100, "%")
  paste0("Geocoding match rate with perfect (100) scores: ",
         round(nrow(subset(crime.geo, score == "100")) /
                 # total, less BAs
                 (nrow(crime) -  nrow(crime.geo[grep("BA", crime.geo$locator), ])),
               6)*100, "%")
  paste0("Percent of orginial dataset geocoded: ",
         round(
           # geocode matches
           nrow(subset(crime.geo, locator == "direct match" |
                         locator == "MAI_geocode" |
                         locator == "DIME_geocode")) /
             nrow(crime), 6)*100, "%")

  ######################
  ## RETURN DF        ##
  ######################
  return(crime.geo)
}
