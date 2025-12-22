#### BeeWalk site coordinates


beewalk <- read.csv("BeeWalk data 2008-24 09012025.csv", header = TRUE, stringsAsFactors = FALSE)

beewalk_sites <- unique(beewalk[c("GridReference", "Year", "Transect.lat", "Transect.long", "Precision")])

write.csv(beewalk_sites, "BeeWalk_sites.csv", row.names = FALSE)

summary(beewalk$Precision)
table(beewalk$Precision)
