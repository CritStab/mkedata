# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This document contains functions that acquire and assemble WIBRS crime data
# concerning Milwaukee, Wisconsin.
#
# The primary functions include:
#  o get_wibrs
#
# require(xlsx)
# List of City of Milwaukee open data: http://city.milwaukee.gov/DownloadTabularData3496.htm#AddressIndex

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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

#' @return A data frame.
#' @export
#' @examples
#' \dontrun{
#' raw_geo <- geocode(head(raw), "LOCATION")
#' raw_geo
#' }
get_wibrs <- function(url = wibrs_url, radius_ft = 99000, center = cityhall) {
  itmdapps <- "http://itmdapps.milwaukee.gov/publicApplication_QD/queryDownload/login.faces"
  radius <- "http://itmdapps.milwaukee.gov/publicApplication_QD/queryDownload/radiusfm.faces"
  cityhall <- "200 E Wells ST"

}

## http://stackoverflow.com/questions/28418770/using-rvest-or-httr-to-log-in-to-non-standard-forms-on-a-webpage
itmdapps <- "http://itmdapps.milwaukee.gov/publicApplication_QD/queryDownload/login.faces"
radius <- "http://itmdapps.milwaukee.gov/publicApplication_QD/queryDownload/radiusfm.faces"
start <- itmdapps
pgsession <-html_session(start)             ## create session
pgform    <-html_form(pgsession)[[2]]       ## pull form from session
filled_form <- set_values(pgform,
                          `loginform:userNameId` = "address",
                          `loginform:passwordSecretId` = "user")

## login and get and populate radius form
login <- submit_form(pgsession, filled_form)
pgform_radius <- login %>%
  jump_to(radius) %>%
  # html()
  html_form() # pull radius query form
pgform_radius <- pgform_radius[[1]]

filled_form_radius <- set_values(pgform_radius,
                                 `formQuery:textNbrId` = "200",
                                 `formQuery:menuDirId` = "E",
                                 `formQuery:menuNameTypeId` = "Wells",
                                 `formQuery:textRadiusId` = "99000",
                                 `dateFromCrime` = "01/01/2015",
                                 `dateToCrime` = "12/31/2015",
                                 `formQuery:radioFormat` = "excel",
                                 `formQuery:selectRad` = "incidentLevel")

## submit radius form
query <- login %>%
  jump_to(radius) %>%
  submit_form(filled_form_radius) # pull radius query form

