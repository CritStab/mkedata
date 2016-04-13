# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This document contains functions that acquire and assemble WIBRS crime data
# concerning Milwaukee, Wisconsin.
#
# The primary functions include:
#  o get_wibrs (DEBUG)
#  o append_wibrs

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
