
crimes_by_group_year <- function(spdf){

# ## group by address, and sum by group  (all years)
# out <- data.table::data.table(crimes.munged@data)
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
newspdf <- SpatialPointsDataFrame(coords = w.all[, c("x", "y")], data = w.all, proj4string = projection)

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

# example
# agg <- crimes_by_group_year(data("crimes.munged"))
