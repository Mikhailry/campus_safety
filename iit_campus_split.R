load("iit.rda")

iit2 <- iit

iit2$SECTOR <- NULL
iit2$sector <- ""

grid_1 <- which((iit2$LATITUDE >= 41.8276) & (iit2$LATITUDE < 41.831225) & (iit2$LONGITUDE >= -87.6330) & (iit2$LONGITUDE < -87.6250))
iit2[grid_1,"SECTOR"] <- 1

grid_2 <- which((iit2$LATITUDE >= 41.8276) & (iit2$LATITUDE < 41.831225) & (iit2$LONGITUDE >= -87.6250) & (iit2$LONGITUDE < -87.6170))
iit2[grid_2,"SECTOR"] <- 2

grid_3 <- which((iit2$LATITUDE >= 41.831225) & (iit2$LATITUDE < 41.83485 ) & (iit2$LONGITUDE >= -87.6330) & (iit2$LONGITUDE < -87.6250))
iit2[grid_3,"SECTOR"] <- 3

grid_4 <- which((iit2$LATITUDE >= 41.831225) & (iit2$LATITUDE < 41.83485) & (iit2$LONGITUDE >= -87.6250) & (iit2$LONGITUDE < -87.6170))
iit2[grid_4,"SECTOR"] <- 4

grid_5 <- which((iit2$LATITUDE >= 41.83485) & (iit2$LATITUDE < 41.838475) & (iit2$LONGITUDE >= -87.6330) & (iit2$LONGITUDE < -87.6250))
iit2[grid_5,"SECTOR"] <- 5

grid_6 <- which((iit2$LATITUDE >=41.83485 ) & (iit2$LATITUDE < 41.838475) & (iit2$LONGITUDE >= -87.6250) & (iit2$LONGITUDE < -87.6170))
iit2[grid_6,"SECTOR"] <- 6

grid_7 <- which((iit2$LATITUDE >= 41.838475) & (iit2$LATITUDE < 41.8421) & (iit2$LONGITUDE >= -87.6330) & (iit2$LONGITUDE < -87.6250))
iit2[grid_7,"SECTOR"] <- 7

grid_8 <- which((iit2$LATITUDE >= 41.838475) & (iit2$LATITUDE < 41.8421) & (iit2$LONGITUDE >= -87.6250) & (iit2$LONGITUDE < -87.6170))
iit2[grid_8,"SECTOR"] <- 8

iit3 <- iit2[which(!is.na(iit2$SECTOR)),]
iit3$sector<-NULL

save(iit3, file="iit_only.rda")
