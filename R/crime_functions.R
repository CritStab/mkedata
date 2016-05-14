# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This document contains functions that acquire and assemble WIBRS crime data
# concerning Milwaukee, Wisconsin.
#
# The primary functions include:
#  o get_wibrs (DEBUG)
#  o append_wibrs
#  o munge_wibrs

#
# require(xlsx)
# List of City of Milwaukee open data: http://city.milwaukee.gov/DownloadTabularData3496.htm#AddressIndex

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' append_wibrs
#'
#' append_wibrs appends new, raw WIBRS data to the historical, raw WIBRS dataset.
#' It assumes that the new data frame came from an Excel file downloaded from
#' the City of Milwaukee's COMPASS server (see get_wibrs) and therefore trims
#' the last three rows of the new data frame.
#'
#' @importFrom xlsx read.xlsx2
#' @param old A data frame of WIBRS data.
#' @param new A data frame of WIBRS data.
#' @return A data frame.
#' @export
#' @examples
#' data(raw_wibrs_2005thru2015)
#' wibrs2015 <- xlsx::read.xlsx2("~/Dropbox/Analytics-Consulting/Non-client_projects/MKE data/crime/2015_WIBR_Radius99K_accessed041316.xls", 1) # accessed 04/13/16
#' # Note: the following example results in dupes
#' combo <- append_wibrs(raw_wibrs_2005thru2015, wibrs2015)

append_wibrs <- function(old, new){
  c.list <- list(old, new)
  # remove meta lines at end of Excel file TODO: generalize to rows where ID is null
  trim <- function(x){
    x[1:(nrow(x)-3), ]
  }

  c.list[2] <- lapply(c.list[2], trim)
  result <- do.call("rbind", c.list)

  # TODO: convert this to a test
  print(nrow(result) == nrow(old) + nrow(new) - 3)

  result
}



#' get_wibrs
#'
#' get_wibrs retrieves WIBRS data from the City of Milwaukee's server.
#'
#' @import rvest
#' @import magrittr
#'
#' @param url Target url.
#' @param radius_ft Search radius (in feet). Default is 99000 to cover entire city.
#' @param center Centroid address of radius. Default is address of City Hall.
#' @param date_range Character vector of length 2. Default is c("01/01/2015",
#' "12/31/2015").

#' @return A data frame.
#' @export
#' @examples
#' \dontrun{
#' city2015 <- get_wibrs()
#' head(city2015)
#' }
get_wibrs <- function(url = wibrs_url, radius_ft = 99000, center = cityhall, data_range = yr2015) {
  itmdapps <- "http://itmdapps.milwaukee.gov/publicApplication_QD/queryDownload/login.faces"
  radius <- "http://itmdapps.milwaukee.gov/publicApplication_QD/queryDownload/radiusfm.faces"
  cityhall <- "200 E Wells ST"
  yr2015 <- c("01/01/2015", "12/31/2015")

  # create session; get and populate login form
  start <- itmdapps
  pgsession <-html_session(start)
  pgform    <-html_form(pgsession)[[2]]
  filled_form <- set_values(pgform,
                          `loginform:userNameId` = "address",
                          `loginform:passwordSecretId` = "user")

  # login; get and populate radius query form
  login <- submit_form(pgsession, filled_form)
  pgform_radius <- login %>%
    jump_to(radius) %>%
    html_form() # pull radius query form
    pgform_radius <- pgform_radius[[1]]
  # TODO populate from arguments
    # see how PWS_history handles date parameters
    filled_form_radius <- set_values(pgform_radius,
                                 `formQuery:textNbrId` = "200",
                                 `formQuery:menuDirId` = "E",
                                 `formQuery:menuNameTypeId` = "Wells",
                                 `formQuery:textRadiusId` = "99000",
                                 `dateFromCrime` = "01/01/2015",
                                 `dateToCrime` = "12/31/2015",
                                 `formQuery:radioFormat` = "excel",
                                 `formQuery:selectRad` = "incidentLevel")
  filled_form_radius$url <- "http://itmdapps.milwaukee.gov/publicApplication_QD/queryDownload/radiusfm.faces"

  ## submit radius form
  ## DEBUG: submit_form doesn't carry the correct credentials?
  query <- login %>%
    jump_to(radius) %>%
    html_nodes("title") %>%
    submit_form(filled_form_radius)
}


#' munge_wibrs
#'
#' munge_wibrs munges geo-processed WIBRS data, producing a
#' SpatialPointsDataFrame ready for analysis and visualization. Note: this
#' function does not exclude records with low or null geocode scores.
#'
#' @importFrom lubridate parse_date_time
#' @importFrom sp CRS coordinates proj4string plot
#' @param df A data frame of WIBRS data.
#' @return A SpatialPointsDataFrame in CRS NAD27.
#' @export
#' @examples
#' data(crime.geo)
#' crimes <- munge_wibrs(crime.geo)

munge_wibrs <- function(df) {
  ## TODO: convert to spacetime class; add parameter to prune by geo score

  # new variable: map MPD RMS class to WIBRS OFFENSE1 type, per Nancy Olson email
  df$group <- "other"
  df$group[df$OFFENSE1 == "AGGRAVATED ASSAULT"
           | df$OFFENSE1 == "SIMPLE ASSAULT"
           | df$OFFENSE1 == "INTIMIDATION"] <- "Assault"
  df$group[df$OFFENSE1 == "ARSON"] <- "Arson"
  df$group[df$OFFENSE1 == "DESTRUCTION/DAMAGE/VANDALISM OF PROPERTY"] <-
    "Criminal damage"
  df$group[df$OFFENSE1 == "BURGLARY/BREAKING AND ENTERING"] <- "Burglary"
  df$group[df$OFFENSE1 == "HOMICIDE"] <- "Homicide"
  df$group[df$OFFENSE1 == "THEFT FROM MOTOR VEHICLE"] <- "Locked vehicle"
  df$group[df$OFFENSE1 == "ROBBERY"] <- "Robbery"
  df$group[df$OFFENSE1 == "FORCIBLE RAPE"
           | df$OFFENSE1 == "FORCIBLE SODOMY"
           | df$OFFENSE1 == "SEXUAL ASSAULT WITH AN OBJECT"
           | df$OFFENSE1 == "FORCIBLE FONDLING"
           | df$OFFENSE1 == "INCEST"
           | df$OFFENSE1 == "STATUTORY RAPE"] <- "Sex offenses"
  df$group[df$OFFENSE1 == "POCKET PICKING"
           | df$OFFENSE1 == "PURSE SNATCHING"
           | df$OFFENSE1 == "SHOPLIFTING"
           | df$OFFENSE1 == "THEFT FROM BUILDING"
           | df$OFFENSE1 == "THEFT FROM COIN-OPPERATED MACHINES"
           | df$OFFENSE1 == "THEFT OF MOTOR VEHICLE PARTS/ACCESSORIES"
           | df$OFFENSE1 == "ALL OTHER LARCENY"] <- "Theft"
  df$group[df$OFFENSE1 == "MOTOR VEHICLE THEFT"] <- "Vehicle theft"
  df$group <- as.factor(df$group)

  # psuedo categories
  theft_types <- c(
    "PURSE SNATCHING",
    "SHOPLIFTING",
    "POCKET PICKING",
    "THEFT FROM BUILDING",
    "THEFT FROM COIN-OPPERATED MACHINES",
    "THEFT OF MOTOR VEHICLE PARTS/ACCESSORIES",
    "ALL OTHER LARCENY"
  )

  # "parking lot"" crimes
  parking <- c(
    "THEFT FROM MOTOR VEHICLE",
    "THEFT OF MOTOR VEHICLE PARTS/ACCESSORIES",
    "MOTOR VEHICLE THEFT"
  )

  property <-
    c("Vehicle theft",
      "Theft",
      "Locked vehicle",
      "Burglary",
      "Criminal damage",
      "Arson")

  # create new datetime field
  df$datetime <- paste(df$CDATE, df$CTIME)
  df$datetime <- lubridate::parse_date_time(df$datetime,
                                            "%m%d%y %I%M %p")

  # new year column
  df$CDATE <- as.Date(df$CDATE, "%m/%d/%Y")
  df$year <- as.factor(format(df$CDATE, "%Y"))

  # transform to sp class and set CRS
  NAD27 <- sp::CRS("+proj=lcc +lat_1=42.73333333333333 +lat_2=44.06666666666667 +lat_0=42 +lon_0=-90 
             +x_0=609601.2192024384 +y_0=0 +datum=NAD27 +units=us-ft +no_defs +ellps=clrk66 
                   +nadgrids=@conus,@alaska,@ntv2_0.gsb,@ntv1_can.dat")

  crimes <- df
  crimes <- subset(crimes, !is.na(y))
  coords <- cbind(as.numeric(crimes$x), as.numeric(crimes$y))
  sp::coordinates(crimes) <- coords
  sp::proj4string(crimes) <- NAD27
  crimes$x <- as.character(crimes$x) # this allows the writeOGR() to shapefile
  crimes$y <- as.character(crimes$y)
  crimes$score <- as.numeric(crimes$score)

  ## remove unmatached and low scoring records
  # crimes <- subset(crimes, score >= 95 | is.na(score)) # standard used in Gateway analysis

  # confirm with plot
  sp::plot(
    crimes[sample(1:length(crimes), 1000), ],
    add = F,
    col = "light blue",
    main = paste0("SpatialPointsDataFrame in NAD27" , "\nN = ",
                  dim(crimes@data)[1])
  )

  return(crimes)
}


#' crimes_by_group_year
#'
#' crimes_by_group_year aggregates munged geo-processed WIBRS data by offense
#' group and by year.
#'
#' @importFrom data.table data.table melt dcast
#' @importFrom sp CRS coordinates proj4string plot SpatialPointsDataFrame
#' @param spdf A SpatialPointsDataFrame of WIBRS data.
#' @return A SpatialPointsDataFrame in CRS NAD27.
#' @export
#' @examples
#' data(crimes.munged)
#' agg <- crimes_by_group_year(crimes.munged)

crimes_by_group_year <- function(spdf){

  # ## group by address, and sum by group  (all years)
  # out <- data.table::data.table(spdf@data)
  # cols <- c("ADDRESS","x","y","group")
  #
  # s.all <- subset(out, select=cols)
  # l.all <- data.table::melt(s.all, id.vars=c("ADDRESS","x","y"))
  # w.all <- data.table::dcast(l.all, ADDRESS + x + y ~ value,
  #                            fun.aggregate = length, margins=F)
  # w.all <- data.frame(w.all)
  # w.all$total <- apply(w.all[4:13], 1, sum)

  ## group by address and year, and sum by group
  spdf <- crimes.munged
  projection <- spdf@proj4string
  out <- data.table::data.table(spdf@data)
  cols <- c("ADDRESS","x","y","group", "year")
  s.all <- subset(out, select=cols)
  l.all <- data.table::melt(s.all, id.vars=c("ADDRESS","x","y","year"))
  w.all <- data.table::dcast(l.all, ADDRESS + x + y + year ~ value,
                             fun.aggregate = length, margins=F)
  w.all <- data.frame(w.all)
  w.all$total <- apply(w.all[5:14], 1, sum)


  w.all$x <- as.numeric(w.all$x)
  w.all$y <- as.numeric(w.all$y)
  newspdf <- sp::SpatialPointsDataFrame(coords = w.all[, c("x", "y")], data = w.all, proj4string = projection)

  # confirm with plot
  sp::plot(
    newspdf[sample(1:length(newspdf), 1000), ],
    add = F,
    col = "light blue",
    main = paste0("SpatialPointsDataFrame in NAD27" , "\nN = ",
                  dim(newspdf@data)[1])
  )

  return(newspdf)
}
