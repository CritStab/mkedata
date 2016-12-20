## join mprop to parcels

library(mkedata)
data("parcels")
data("mprop_12192016")


mprop <- mprop_12192016
combo <- merge(parcels, mprop_12192016, by = TAXKEY, all.x = T)
table(combo$BLDG_TYPE) # 1 = ranch style

sample <- sample(nrow(combo), 5000)
ss <- combo[sample,]
# handle parcel_typ

v <- subset(combo, LAND_USE_GP == "13")
tot <- sum(as.numeric(combo$LOT_AREA), na.rm = T)
totV <- sum(as.numeric(v$LOT_AREA), na.rm = T)
totV/tot


## select parcels by owner name
own_subset <- function(pattern, spdf){
  ind <- grep(pattern, spdf$OWNER_NAME_1)
  spdf[ind,]
}

t <- own_subset("LLC", combo)
plot(mkeoutline)
plot(t, col = "red", add = T)

use_package("rvest")
