require(raster)

### Reading in data: USCIE,  ####

# uscie spatial info
uscie <- raster("\\\\ies\\d5\\agrienv\\Data\\uscie\\uscie_raster_FSU\\refras_FSU_land.tif")
uscie

# data for delineaion (uscie, admin, 10km id, etc)
data4delin <- read.csv("\\\\ies\\d5\\agrienv\\Data\\uscie\\uscie_raster_FSU\\refras_FSU_land_soil_10km_admin.csv", header = TRUE)

data4delin$HSU2_CD_SO <- as.character(paste0("000", data4delin$HSU2_CD_SO))
data4delin$HSU2_CD_SO <- substr(data4delin$HSU2_CD_SO, (nchar(data4delin$HSU2_CD_SO) - 3), nchar(data4delin$HSU2_CD_SO))


# Forest
forest <- raster(paste0("E:\\FSUs/final", "/forest_only_1km.tif"))

forest_uscie <- crop(uscie, forest)
forest_uscie1 <- crop(forest, forest_uscie)

uscie_for <- data.frame(values(forest_uscie), values(forest_uscie1))
names(uscie_for) <- c("uscie", "forest")
uscie_for <- uscie_for[!is.na(uscie_for$uscie), ]
uscie_for <- uscie_for[!is.na(uscie_for$forest), ]



# NoGo
dir2save <- "x:\\adrian/data/fsu"
nogo <- raster(paste0(dir2save, "/nogo_1km.tif"))

nogo_uscie <- crop(uscie, nogo)
nogo_uscie1 <- crop(nogo, nogo_uscie)

uscie_nogo <- data.frame(values(nogo_uscie), values(nogo_uscie1))
names(uscie_nogo) <- c("uscie", "nogo")
uscie_nogo <- uscie_nogo[!is.na(uscie_nogo$uscie), ]
uscie_nogo <- uscie_nogo[!is.na(uscie_nogo$nogo), ]



# NoGO + Forest

uscie_nogo_forest <- merge(uscie_nogo, uscie_for, by = "uscie", all = TRUE)



# FSU_delim: data for delineaion + NOGO + FOREST
data4delin

FSU_delim_all <- merge(data4delin, uscie_nogo_forest, by.x = "USCIE_RC", by.y = "uscie", all = TRUE)


# go FSUs
FSU_delim_go <- FSU_delim_all[FSU_delim_all$nogo == 0, ]
FSU_delim_go <- FSU_delim_go[!is.na(FSU_delim_go$nogo), ]

FSU_delim_go$FSU <- paste0(FSU_delim_go$FSUADM2_ID, FSU_delim_go$INSP10_ID, FSU_delim_go$HSU2_CD_SO, FSU_delim_go$forest)



# nogo FSUs
FSU_delim_nogo <- FSU_delim_all[FSU_delim_all$nogo == 1, ]
FSU_delim_nogo <- FSU_delim_nogo[!is.na(FSU_delim_nogo$nogo), ]

FSU_delim_nogo$FSU <- paste0(FSU_delim_nogo$FSUADM2_ID, FSU_delim_nogo$INSP10_ID)



# FSU_delim

FSU_delim <- rbind(FSU_delim_go, FSU_delim_nogo)

write.csv(FSU_delim, "\\\\ies-ud01.jrc.it\\D5_agrienv\\Data\\FSU/FSU_delin.csv", row.names = FALSE)
