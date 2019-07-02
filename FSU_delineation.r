

### Reading in data: USCIE,  ####

# uscie spatial info
uscie <- raster("\\\\ies\\d5\\agrienv\\Data\\uscie\\uscie_raster_FSU\\refras_FSU_land.tif")
uscie

head(unique(values(uscie)))
length(values(uscie))
range(values(uscie), na.rm = TRUE)
sum(!is.na(values(uscie)))



# data for delineaion (uscie, admin, 10km id, etc)
data4delin <- read.csv("\\\\ies\\d5\\agrienv\\Data\\uscie\\uscie_raster_FSU\\refras_FSU_land_soil_10km_admin.csv", header = TRUE)
head(data4delin)
range(data4delin$USCIE_RC)
length(data4delin$USCIE_RC)
sum(is.na(data4delin$HSU2_CD_SO))
unique(data4delin$HSU2_CD_SO)
which(data4delin$HSU2_CD_SO == 2)


data4delin$HSU2_CD_SO <- as.character(paste0("000", data4delin$HSU2_CD_SO))
data4delin$HSU2_CD_SO <- substr(data4delin$HSU2_CD_SO, (nchar(data4delin$HSU2_CD_SO) - 3), nchar(data4delin$HSU2_CD_SO))


# Forest
forest <- raster(paste0("E:\\FSUs/final", "/forest_only_1km.tif"))

forest
sum(!is.na(values(forest)))
unique(values(forest))


forest_uscie <- crop(uscie, forest)
forest_uscie1 <- crop(forest, forest_uscie)

uscie_for <- data.frame(values(forest_uscie), values(forest_uscie1))
names(uscie_for) <- c("uscie", "forest")
uscie_for <- uscie_for[!is.na(uscie_for$uscie), ]
uscie_for <- uscie_for[!is.na(uscie_for$forest), ]
head(uscie_for)
nrow(uscie_for) #5834930
sum(is.na(uscie_for$forest)) #313
sum(uscie_for$forest) #653295



# NoGo
nogo <- raster(paste0("E:\\FSUs/final", "/nogo_1km.tif"))

nogo_uscie <- crop(uscie, nogo)
nogo_uscie1 <- crop(nogo, nogo_uscie)

uscie_nogo <- data.frame(values(nogo_uscie), values(nogo_uscie1))
names(uscie_nogo) <- c("uscie", "nogo")
uscie_nogo <- uscie_nogo[!is.na(uscie_nogo$uscie), ]
uscie_nogo <- uscie_nogo[!is.na(uscie_nogo$nogo), ]
head(uscie_nogo)
nrow(uscie_nogo) #5834930
sum(is.na(uscie_nogo$nogo)) #313
sum(uscie_nogo$nogo) #172338



# NoGO + Forest

uscie_nogo_forest <- merge(uscie_nogo, uscie_for, by = "uscie", all = TRUE)
nrow(uscie_nogo_forest)
head(uscie_nogo_forest)
range(uscie_nogo_forest$uscie)   #3365046, 911355964




# FSU_delim: data for delineaion + NOGO + FOREST
data4delin
head(data4delin)
nrow(data4delin)
names(data4delin)
unique(data4delin$HSU2_CD_SO)
range(data4delin$HSU2_CD_SO, na.rm = TRUE)
apply(data4delin, 2, function(x) sum(is.na(x)))

FSU_delim_all <- merge(data4delin, uscie_nogo_forest, by.x = "USCIE_RC", by.y = "uscie", all = TRUE)
nrow(FSU_delim_all)
head(FSU_delim_all)
apply(FSU_delim_all, 2, function(x) sum(is.na(x)))
View(FSU_delim_all[is.na(FSU_delim_all$nogo), ])


# go FSUs
FSU_delim_go <- FSU_delim_all[FSU_delim_all$nogo == 0, ]
FSU_delim_go <- FSU_delim_go[!is.na(FSU_delim_go$nogo), ]
nrow(FSU_delim_go)
head(FSU_delim_go)
View(FSU_delim_go[is.na(FSU_delim_go$nogo), ])
unique(FSU_delim_go$FSUADM2_ID)

FSU_delim_go$FSU <- paste0(FSU_delim_go$FSUADM2_ID, FSU_delim_go$INSP10_ID, FSU_delim_go$HSU2_CD_SO, FSU_delim_go$forest)
head(FSU_delim_go)
apply(FSU_delim_go, 2, function(x) sum(is.na(x)))


# nogo FSUs
FSU_delim_nogo <- FSU_delim_all[FSU_delim_all$nogo == 1, ]
FSU_delim_nogo <- FSU_delim_nogo[!is.na(FSU_delim_nogo$nogo), ]
nrow(FSU_delim_nogo)
head(FSU_delim_nogo)

FSU_delim_nogo$FSU <- paste0(FSU_delim_nogo$FSUADM2_ID, FSU_delim_nogo$INSP10_ID)
head(FSU_delim_nogo)
apply(FSU_delim_nogo, 2, function(x) sum(is.na(x)))



# FSU_delim

FSU_delim <- rbind(FSU_delim_go, FSU_delim_nogo)
nrow(FSU_delim)
head(FSU_delim)
apply(FSU_delim, 2, function(x) sum(is.na(x)))
head(FSU_delim[FSU_delim$nogo == 1, ], 20)
head(FSU_delim[grepl("NA", FSU_delim$FSU), ], 20)
View(FSU_delim[is.na(FSU_delim$HSU2_CD_SO), ])

write.csv(FSU_delim, "\\\\ies-ud01.jrc.it\\D5_agrienv\\Data\\FSU/FSU_delin.csv", row.names = FALSE)
