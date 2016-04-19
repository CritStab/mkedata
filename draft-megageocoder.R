# geocode address routine

## sex crimes have NULL ADDRESS field

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
message("Use City MAI-then-DIME REST API to match remaining addresses")
message("NOTE: this will take approximately ", round(nrow(uu)/100/60), " hours to complete
        (roughly 1 minute per 100 records).")
test <- uu[1200:1220, ]
geo.data <- tryCatch(geocode_api(test, "ADDRESS"))
# geo.data <- tryCatch(lapply(uu.split, geocode, "ADDRESS")) # original call if spliting into lists

######################
## POSTPROCESS DATA ##
######################
# geo.data <- do.call("rbind", geo.data) # return list to one standard data table
tt <- geo.data
tt$x <- as.numeric(tt$x)
tt$y <- as.numeric(tt$y)
tt$locator <- as.character(tt$locator)
tt <- subset(tt, select = c("x",
                              "y",
                              "ADDRESS",
                              "INCIDENT_N",
                              "geoAdd",
                              "locator",
                              "score")) # remove unessessry columns
tt <- data.table::as.data.table(tt)
data.table::setkey(tt, ADDRESS)

# left outer join to full crime dataset
merged <- tt[crimeMAI]

# populate x and y with POINT_X and POINT_Y
# TODO: check this
merged <- as.data.frame(merged)
nrow(merged[is.na(merged$x), ])
merged$x[is.na(merged$x)] <- merged$i.x[is.na(merged$x)]
merged$y[is.na(merged$y)] <- merged$i.y[is.na(merged$y)]
nrow(merged[is.na(merged$x), ])
crime.geo <- merged
# TODO: add diagnostics
paste0("New match rate to the MAI: ",
       round(table(!is.na(crime.geo$x))[2] / nrow(crime), 4)*100, "%")

