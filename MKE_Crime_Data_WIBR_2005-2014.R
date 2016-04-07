## build MKE master WIBR crime dataset 2005 through 2014
## from COMPASS website (year, radius 99K)
## 8/7/14, 1/29/15, 1/30/15, 2/18/15, 2/24/15, 9/10/15
## 2013 and 2014 data accessed 9/10/15 to replace possibly "un-frozen" provisionary data from prior access date
# download URL: http://itmdapps.milwaukee.gov/publicApplication_QD/queryDownload/radiusfm.faces
# choose 99000 ft radius from 2438 n humboldt bl
# for info on read.xlsx() vs read.xlsx2(): http://www.milanor.net/blog/?p=779

setwd("~/Dropbox/Analytics-Consulting/Non-client projects/MKE data/crime")
library(xlsx)
library(plyr)
library(data.table)
library(XML)
source("~/Dropbox/Analytics-Consulting/Non-client projects/MKE data/MAI/MAIgeocoder.r")

###############
## LOAD DATA ##
###############
mai <- read.csv("~/Dropbox/Analytics-Consulting/Non-client projects/MKE data/MAI/MAI_geo_as_WKT.csv", colClasses = "character") # load MAI table

c05 <- read.xlsx2("2005_WIBR_Radius99K.xls", 1) # load annual crime data
c06 <- read.xlsx2("2006_WIBR_Radius99K.xls", 1)
c07 <- read.xlsx2("2007_WIBR_Radius99K.xls", 1)
c08 <- read.xlsx2("2008_WIBR_Radius99K.xls", 1)
c09 <- read.xlsx2("2009_WIBR_Radius99K.xls", 1)
c10 <- read.xlsx2("2010_WIBR_Radius99K.xls", 1)
c11 <- read.xlsx2("2011_WIBR_Radius99K.xls", 1)
c12 <- read.xlsx2("2012_WIBR_Radius99K.xls", 1)
c13 <- read.xlsx2("2013_WIBR_Radius99K.xls", 1)
#c14 <- read.xlsx2("2014_WIBR_Radius99K.xls", 1)
#c13n <- read.xlsx2("2013_WIBR_Radius99K_accessed091015.xls", 1) # accessed 9/10/15, only 1 additinal record
c14 <- read.xlsx2("2014_WIBR_Radius99K_accessed091015.xls", 1) # accessed 9/10/15, 130 additinal records

c.list <- list(c05,c06,c07,c08,c09,c10,c11,c12,c13,c14) #remove meta lines at end; e.g. tail(c05, 10)
trim <- function(x){
  x[1:(nrow(x)-3), ]
} 
c.list <- lapply(c.list, trim)

crime <- do.call("rbind", c.list) # combine years
sts <- as.data.frame(table(crime$LOCATION)) # table of unique address strings, for validation

#####################
## PREPROCESS DATA ##
#####################
# note: this is high volume and may be rejected by City API
# send to geocoder API only those sts not matched by MAI
# note any MAI score less than 95 could be suspect, especially on street direction error, DIME over 90 look good
# records with score 100 about 21% ; records with score >= 95 about 29%

# merge MAI geo data to crime records where possible
crime <- as.data.table(crime) 
mai <- as.data.table(mai)
setkey(crime, LOCATION)
setkey(mai, ADDRESS)
mai.u <- unique(mai)
crimeMAI <- mai.u[crime] # left outer join
table(is.na(crimeMAI$WKT)) # compare to:
table(crime$LOCATION %in% mai$ADDRESS) # direct match rate to MAI: ~ 78% 
crimeMAI <- crimeMAI[, c(1:3,18:28), with=F] # remove unessessry columns

# unique unmatched addresses
setkey(crimeMAI, ADDRESS)
u <- unique(crimeMAI) # unique addresses (should match str)
uu <- u[is.na(u$WKT), ] # and unmatched

# cull bad stuff known to fail geocoder
uu <- uu[!grep("/", uu$ADDRESS), ] # intersections generally fail in City API

# split by district to feed chunks to geocoder
d <- uu$DISTRICT
uu.split <- split(as.data.frame(uu), d, drop=TRUE)
#test <- lapply(uu.split, head, 2)


#####################
## GEOCODE DATA    ##
#####################
# use City MAI-then-DIME REST API to match remaining addresses
## ! NOTE: this takes 6 plus hours to run; use load() below to skip this step

#geo.data <- tryCatch(lapply(uu.split, geocode, "ADDRESS")) 
#save(geo.data, file="crime-geocode-results.RData")
load(file="crime-geocode-results.RData")

######################
## POSTPROCESS DATA ##
######################
geo.data <- do.call("rbind", geo.data) # return to one standard data table
tt <- geo.data
i <- sapply(tt, is.list)
tt[i] <- lapply(tt[i], as.character)
tt <- as.data.table(tt)
setkey(tt, ADDRESS)
tt.s <- subset(tt, select = c(1,15:19)) # remove unessessry columns

# left outer join to full crime dataset
merged <- tt.s[crimeMAI] 

# populate x and y with POINT_X and POINT_Y
merged <- as.data.frame(merged)
nrow(merged[is.na(merged$x), ]) 
merged$x[is.na(merged$x)] <- merged$POINT_X[is.na(merged$x)]
merged$y[is.na(merged$y)] <- merged$POINT_Y[is.na(merged$y)]
nrow(merged[is.na(merged$x), ])
crime.geo <- merged

###############
## DUMP DATA ##
###############
        # write out master file for other uses
#load("full-geocoded-WIBRS-05-14.RData")
save(crime.geo, file="full-geocoded-WIBRS-05-14.RData")
write.csv(crime.geo, paste("full-geocoded-WIBRS-05-14", format(Sys.time(), "%b_%d_%Y"),".csv", sep=""),
          row.names = FALSE)


# -------------- OLD ---------------------------
# import master WIBRS file that had been joined to Milwaukee Master Address Index
# in QGIS using this method: http://allaroundgis.wordpress.com/2014/06/05/qgis-how-to-join-multiple-records-to-single-feature/
# note: 78% match rate
crimeGeo <- read.csv("MasterWIBRS_geo.csv")

