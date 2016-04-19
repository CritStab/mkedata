# geocode address routine

# load data
data("raw_wibrs_2005thru2015")
data("geo_mai")



# note: this is high volume and may be rejected by City API
# send to geocoder API only those sts not matched by MAI
# note any MAI score less than 95 could be suspect, especially on street direction error, DIME over 90 look good
# records with score 100 about 21% ; records with score >= 95 about 29%

#####################
## MATCH TO MAI    ##
#####################

# merge MAI geo data to crime records where possible
crime <- data.table::as.data.table(raw_wibrs_2005thru2015)
mai <- data.table::as.data.table(geo_mai)
data.table::setkey(crime, LOCATION)
data.table::setkey(mai, ADDRESS)
mai.u <- unique(mai)
crimeMAI <- mai.u[crime] # left outer join

# direct match rate to MAI: ~ 78%
# TODO: investigate differences between below calcs
table(!is.na(crimeMAI$lat))[2] / nrow(crime) # compare to:
table(crime$LOCATION %in% mai$ADDRESS)[2] / nrow(crime)
paste0("Direct match rate to the MAI: ",
       round(table(!is.na(crimeMAI$lat))[2] / nrow(crime), 4)*100, "%")
crimeMAI <- crimeMAI[, c(16:27), with = FALSE] # remove unessessry columns #TODO generalize


################################
## PREPARE FOR API GEOCODER   ##
################################

# unique unmatched addresses
data.table::setkey(crimeMAI, ADDRESS)
u <- unique(crimeMAI) # unique addresses
uu <- u[is.na(u$lat), ] # and unmatched

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
message("Use City MAI-then-DIME REST API to match remaining addresses")
message("NOTE: this will take approximately ", round(nrow(uu)/100/60), " hours to complete
        - roughly 1 minute per 100 records.")
test <- uu[1000:1020, ]
geo.data <- tryCatch(lapply(test, geocode_api, "ADDRESS"))


# geo.data <- tryCatch(lapply(uu.split, geocode, "ADDRESS")) # original call


