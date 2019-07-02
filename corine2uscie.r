

library(data.table)
library(raster)
library(rgdal)


### Reading in data: USCIE, CORINE ####

#CORINE raster

corine_dir <- "\\\\ies-ud01.jrc.it\\D5_agrienv\\Data\\Corine_Land_Cover\\clc2018_v20_incl_turkey\\7ac95361f9ac3cecdf37785bc183ff02dd765a16\\clc2018_clc2018_v2018_20_raster100m/"
corine100m <- paste0(corine_dir, "/CLC2018_CLC2018_V2018_20.tif")
corine100m <- raster(corine100m)

corine100m
unique(corine100m@data@attributes)

corine_cats <- read.csv(paste0(corine_dir, "/CLC2018_CLC2018_V2018_20.txt"), header = FALSE)
corine_cats



#USCIE 
uscie_dir <- "\\\\ies\\d5\\agrienv\\Data\\uscie\\uscie_raster_FSU"
uscie1km <- paste0(uscie_dir, "/refras_FSU_land.tif")
uscie1km <- raster(uscie1km)
uscie1km


# clipping corine

corine100m_crop <- crop(corine100m, uscie1km)
#corine100m_crop_kk <- corine100m_crop
#corine100m_crop <- corine100m_crop_kk
#plot(corine100m_crop)

activate_this1 <- 0  # to make checkings
if(activate_this1 == 1){
  etnt <- extent(corine100m_crop)
  etnt@xmin <- 4000000 
  etnt@xmax <- 4500000
  etnt@ymin <- 2500000
  etnt@ymax <- 3000000
    
  corine100m_crop <- crop(corine100m_crop, etnt)
  plot(corine100m_crop)
}



### Aggregating corine to 1km: NoGo and FOREST ####

### nogo1: built-up areas
#
##View(corine_cats[, c(1,6)])
#rcl_mat <- corine_cats[, 1:2]
#
#rcl_mat[, 2] <- 0
#rcl_mat[1:11, 2] <- 1
##rcl_mat[nrow(rcl_mat), 2] <- NA
#
#corine100m_crop_recl <- reclassify(corine100m_crop, rcl_mat)
#
#corine1km <- aggregate(corine100m_crop_recl, fact = 10, fun = sum)
##corine1km
##sum(freq(corine1km)[91:101 , 2])
#
#
#rcl_mat1 <- matrix(nrow = 2, ncol = 3, c(0, 90, 0, 90, 100, 1), byrow = TRUE)
#corine1km_nogo_urb <- reclassify(corine1km, rcl_mat1, right = FALSE, include.lowest = TRUE)
#
#
##plot(corine1km_nogo_urb)
##corine1km_nogo_urb
##freq(corine1km_nogo_urb)
#
#
### nogo2: water
#
##View(corine_cats[, c(1,6)])
#rcl_mat <- corine_cats[, 1:2]
#
#rcl_mat[, 2] <- 0
#rcl_mat[40:44, 2] <- 1
##rcl_mat[nrow(rcl_mat), 2] <- NA
#
#corine100m_crop_recl <- reclassify(corine100m_crop, rcl_mat)
#
#corine1km <- aggregate(corine100m_crop_recl, fact = 10, fun = sum)
##corine1km
##sum(freq(corine1km)[91:101 , 2])
##plot(corine1km)
#
#
#rcl_mat1 <- matrix(nrow = 2, ncol = 3, c(0, 90, 0, 90, 100, 1), byrow = TRUE)
#corine1km_nogo_wtr <- reclassify(corine1km, rcl_mat1, right = FALSE, include.lowest = TRUE)
#
#
##plot(corine1km_nogo_wtr)
##corine1km_nogo_wtr
##freq(corine1km_nogo_wtr)
#
#
#
### nogo3: Bare Land
#
##View(corine_cats[, c(1,6)])
#rcl_mat <- corine_cats[, 1:2]
#
#rcl_mat[, 2] <- 0
#rcl_mat[c(30, 31, 34, 38, 39), 2] <- 1
##rcl_mat[nrow(rcl_mat), 2] <- NA
#
#corine100m_crop_recl <- reclassify(corine100m_crop, rcl_mat)
#
#corine1km <- aggregate(corine100m_crop_recl, fact = 10, fun = sum)
##corine1km
##sum(freq(corine1km)[91:101 , 2])
##plot(corine1km)
#
#
#rcl_mat1 <- matrix(nrow = 2, ncol = 3, c(0, 90, 0, 90, 100, 1), byrow = TRUE)
#corine1km_nogo_bre <- reclassify(corine1km, rcl_mat1, right = FALSE, include.lowest = TRUE)
#
#
##plot(corine1km_nogo_bre)
##corine1km_nogo_bre
##freq(corine1km_nogo_bre)


## All nogo's toghether

#View(corine_cats[, c(1,6)])
rcl_mat <- corine_cats[, 1:2]

rcl_mat[, 2] <- 0
rcl_mat[c(1:11, 30, 31, 34, 38, 39, 40:44), 2] <- 1
#rcl_mat[nrow(rcl_mat), 2] <- NA

corine100m_crop_recl <- reclassify(corine100m_crop, rcl_mat)

corine1km <- aggregate(corine100m_crop_recl, fact = 10, fun = sum)
#corine1km
#sum(freq(corine1km)[91:101 , 2])
#plot(corine1km)


rcl_mat1 <- matrix(nrow = 2, ncol = 3, c(0, 90, 0, 90, 100, 1), byrow = TRUE)
corine1km_nogo_all <- reclassify(corine1km, rcl_mat1, right = FALSE, include.lowest = TRUE)


#plot(corine1km_nogo_all)
#corine1km_nogo_all
#freq(corine1km_nogo_all)



### Saving raster NOGO at 1km ####

dir2save <- "E:\\FSUs/final"

# raster with shares of NoGo
writeRaster(corine1km, filename = paste0(dir2save, "/nogo_shares_1km.tif"), format = "GTiff", overwrite = TRUE)

# raster with NoGo (threshhold = 90%)
writeRaster(corine1km_nogo_all, filename = paste0(dir2save, "/nogo_1km.tif"), format = "GTiff", overwrite = TRUE)






### FOREST shares ####

#View(corine_cats[, c(1,6)])
rcl_mat <- corine_cats[, 1:2]

rcl_mat[, 2] <- 0
rcl_mat[c(23:25), 2] <- 1
#rcl_mat[nrow(rcl_mat), 2] <- NA

corine100m_crop_recl <- reclassify(corine100m_crop, rcl_mat)

corine1km_FOR <- aggregate(corine100m_crop_recl, fact = 10, fun = sum)
#corine1km_FOR
#sum(freq(corine1km_FOR)[91:101 , 2])
#plot(corine1km_FOR)


rcl_mat1 <- matrix(nrow = 2, ncol = 3, c(0, 90, 0, 90, 100, 1), byrow = TRUE)
corine1km_forest_only <- reclassify(corine1km_FOR, rcl_mat1, right = FALSE, include.lowest = TRUE)


#plot(corine1km_forest_only)
#corine1km_forest_only
#freq(corine1km_forest_only)


### Saving raster FOREST at 1km ####

dir2save <- "E:\\FSUs/final"

# raster with shares of NoGo
writeRaster(corine1km_FOR, filename = paste0(dir2save, "/forest_shares_1km.tif"), format = "GTiff", overwrite = TRUE)

# raster with NoGo (threshhold = 90%)
writeRaster(corine1km_forest_only, filename = paste0(dir2save, "/forest_only_1km.tif"), format = "GTiff", overwrite = TRUE)






### Reading in data: DEM ####


dem_dir <- "E:\\\\DEM_copernicus\\90b8668f52552b7f9202fc885807420e219e1c3b\\"
dem25m <- paste0(dem_dir, "/eudem_dem_3035_europe.tif")
dem25m <- raster(dem25m)
#dem25m


# clipping DEM

dem25m_crop <- crop(dem25m, uscie1km)
#dem25m_crop_kk <-dem25m_crop
#dem25m_crop <- dem25m_crop_kk
#plot(dem25m_crop)

#writeRaster(dem25m_crop, filename = "dem_crop_kk.tif", format = "GTiff", overwrite = TRUE)


activate_this1 <- 0 # to make checkings
if(activate_this1 == 1){
  etnt <- extent(dem25m_crop)
  etnt@xmin <- 4000000 
  etnt@xmax <- 4500000
  etnt@ymin <- 2500000
  etnt@ymax <- 3000000
  
  dem25m_crop <- crop(dem25m_crop, etnt)
  plot(dem25m_crop)
}

#dem25m_crop

# 1st quartile
dem1km_1quart <- aggregate(dem25m_crop, fact = 40, fun = function(i, ...) quantile(i, probs = 0.25, na.rm = TRUE))
dem1km_1quart@data@names <- "quart25"
  
#dem1km_1quart
#values(dem1km_1quart)[1]

#cc <- c()
#for(i in 1:40){
#  cc <- c(cc, getValues(dem25m_crop, row = i)[1:40])
#}
#quantile(cc)

# 2nd quartile
dem1km_2quart <- aggregate(dem25m_crop, fact = 40, fun = function(i, ...) quantile(i, probs = 0.5, na.rm = TRUE))
dem1km_2quart@data@names <- "quart50"
#values(dem1km_2quart)

# 3rd quartile
dem1km_3quart <- aggregate(dem25m_crop, fact = 40, fun = function(i, ...) quantile(i, probs = 0.75, na.rm = TRUE))
dem1km_3quart@data@names <- "quart75"
#values(dem1km_3quart)


# max
dem1km_4quart <- aggregate(dem25m_crop, fact = 40, fun = max)
dem1km_4quart
dem1km_4quart@data@names <- "quart100"

#values(dem1km_4quart)


# min
dem1km_0quart <- aggregate(dem25m_crop, fact = 40, fun = min)
dem1km_0quart@data@names <- "quart0"
dem1km_0quart


dem1km_quartiles <- stack(dem1km_0quart, dem1km_1quart, dem1km_2quart, dem1km_3quart, dem1km_4quart)
dem1km_quartiles

writeRaster(dem1km_quartiles, filename = paste0(dir2save, "/quartiles_1km.tif"), format = "GTiff", overwrite = TRUE)




