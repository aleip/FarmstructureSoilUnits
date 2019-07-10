library(raster)
library(rgdal)
library(maptools)
library(data.table)
library(dplyr)


### Reading in data: USCIE,  ####

# uscie spatial info
uscie <- raster("\\\\ies\\d5\\agrienv\\Data\\uscie\\uscie_raster_FSU\\refras_FSU_land.tif")
uscie

# data for delineaion (uscie, admin, 10km id, etc)
data4delin <- read.csv("\\\\ies\\d5\\agrienv\\Data\\uscie\\uscie_raster_FSU\\refras_FSU_land_soil_10km_admin.csv", header = TRUE)

data4delin$HSU2_CD_SO <- as.character(paste0("000", data4delin$HSU2_CD_SO))
data4delin$HSU2_CD_SO <- substr(data4delin$HSU2_CD_SO, (nchar(data4delin$HSU2_CD_SO) - 3), nchar(data4delin$HSU2_CD_SO))

#This is only to solve the problem with wrong FSUADM2_ID
doit <- "y"
if(doit == "y"){
  nuts_codes <- read.csv("\\\\ies-ud01.jrc.it\\D5_agrienv\\Data\\FSU\\CAPRI_NUTS_RG_01M_2016_3035_LEVL_3_plus_BA_XK_final_wfoa\\CAPRI_NUTS_RG_01M_2016_3035_LEVL_3_plus_BA_XK_final_wfoa_FSUADM2_ID_corrected.csv", header=T)
  nuts_codes1 <- as.data.table(nuts_codes)
  head(nuts_codes1)
  length(unique(nuts_codes1$FSUADM2_ID))
  length(unique(nuts_codes1$FSUADM2_ID_corrected))
  length(unique(data4delin$FSUADM2_ID))
  length(unique(data4delin$FSUADM3_ID))
  length(unique(nuts_codes1$FSUADM3_ID))
  sum(is.na(unique(data4delin$FSUADM3_ID)))
  sum(is.na(unique(nuts_codes1$FSUADM3_ID)))
  
  unique(nuts_codes1$FSUADM3_ID)[!(unique(nuts_codes1$FSUADM3_ID) %in% unique(data4delin$FSUADM3_ID))]
  data4delin[c(1316, 1328, 2026), ]
  
  data4delin <- as.data.table(data4delin)
  head(data4delin)
  
  nuts_codes1_1 <- nuts_codes1[, c("FSUADM3_ID", "FSUADM2_ID_corrected")]
  length(unique(nuts_codes1_1$FSUADM3_ID))
  length(unique(nuts_codes1_1$FSUADM2_ID_corrected))

  data4delin1 <- merge(data4delin, nuts_codes1_1, by = "FSUADM3_ID", all.x = TRUE)
  View(data4delin1)
  
  data4delin <- as.data.frame(data4delin1)
  write.csv(data4delin, "\\\\ies\\d5\\agrienv\\Data\\uscie\\uscie_raster_FSU\\refras_FSU_land_soil_10km_admin_FSUADM2_ID_corrected.csv", row.names = FALSE)
  
}


# Forest
forest <- raster(paste0("\\\\ies-ud01.jrc.it\\D5_agrienv\\Data\\FSU\\Forest_NoGo", "/forest_only_1km.tif"))

forest_uscie <- crop(uscie, forest)
forest_uscie1 <- crop(forest, forest_uscie)

uscie_for <- data.frame(values(forest_uscie), values(forest_uscie1))
names(uscie_for) <- c("uscie", "forest")
uscie_for <- uscie_for[!is.na(uscie_for$uscie), ]
uscie_for <- uscie_for[!is.na(uscie_for$forest), ]



# NoGo
nogo <- raster(paste0("\\\\ies-ud01.jrc.it\\D5_agrienv\\Data\\FSU\\Forest_NoGo", "/nogo_1km.tif"))

nogo_uscie <- crop(uscie, nogo)
nogo_uscie1 <- crop(nogo, nogo_uscie)

uscie_nogo <- data.frame(values(nogo_uscie), values(nogo_uscie1))
names(uscie_nogo) <- c("uscie", "nogo")
uscie_nogo <- uscie_nogo[!is.na(uscie_nogo$uscie), ]
uscie_nogo <- uscie_nogo[!is.na(uscie_nogo$nogo), ]


# NoGO + Forest

uscie_nogo_forest <- merge(uscie_nogo, uscie_for, by = "uscie", all = TRUE)



# FSU_delim: data for delineaion + NOGO + FOREST

FSU_delim_all <- merge(data4delin, uscie_nogo_forest, by.x = "USCIE_RC", by.y = "uscie", all = TRUE)


# go FSUs
FSU_delim_go <- FSU_delim_all[FSU_delim_all$nogo == 1, ]
FSU_delim_go <- FSU_delim_go[!is.na(FSU_delim_go$nogo), ]

FSU_delim_go$FSU <- paste0(FSU_delim_go$FSUADM2_ID_corrected, FSU_delim_go$INSP10_ID, FSU_delim_go$HSU2_CD_SO, FSU_delim_go$forest)
#FSU_delim_go$FSU <- paste0(FSU_delim_go$FSUADM2_ID, FSU_delim_go$INSP10_ID, FSU_delim_go$HSU2_CD_SO, FSU_delim_go$forest)
#FSU_delim_go$FSU <- paste0(FSU_delim_go$FSUADM3_ID, FSU_delim_go$INSP10_ID, FSU_delim_go$HSU2_CD_SO, FSU_delim_go$forest)



# nogo FSUs
FSU_delim_nogo <- FSU_delim_all[FSU_delim_all$nogo == 0, ]
FSU_delim_nogo <- FSU_delim_nogo[!is.na(FSU_delim_nogo$nogo), ]

FSU_delim_nogo$FSU <- paste0(FSU_delim_nogo$FSUADM2_ID_corrected, FSU_delim_nogo$INSP10_ID)
#FSU_delim_nogo$FSU <- paste0(FSU_delim_nogo$FSUADM3_ID, FSU_delim_nogo$INSP10_ID)




# FSU_delim

FSU_delim <- rbind(FSU_delim_go, FSU_delim_nogo)
nrow(FSU_delim)
#FSU_delim_copy <- FSU_delim
#FSU_delim <- FSU_delim_copy


#Adding NUTS codes, etc

#nuts_codes <- paste0("", "CAPRI_NUTS_RG_01M_2016_3035_LEVL_3_plus_BA_XK_final_wfoa.shp")
#nuts_codes <- readOGR(dsn = "\\\\ies-ud01.jrc.it\\D5_agrienv\\Data\\FSU\\CAPRI_NUTS_RG_01M_2016_3035_LEVL_3_plus_BA_XK_final_wfoa", layer = "CAPRI_NUTS_RG_01M_2016_3035_LEVL_3_plus_BA_XK_final_wfoa")
#nuts_codes <- foreign:::read.dbf("\\\\ies-ud01.jrc.it\\D5_agrienv\\Data\\FSU\\CAPRI_NUTS_RG_01M_2016_3035_LEVL_3_plus_BA_XK_final_wfoa\\CAPRI_NUTS_RG_01M_2016_3035_LEVL_3_plus_BA_XK_final_wfoa.dbf")
#nuts_codes <- read.csv("E:\\FSUs\\CAPRI_NUTS_RG_01M_2016_3035_LEVL_3_plus_BA_XK_final_wfoa\\CAPRI_NUTS_RG_01M_2016_3035_LEVL_3_plus_BA_XK_final_wfoa.csv", header=T)
nuts_codes <- read.csv("\\\\ies-ud01.jrc.it\\D5_agrienv\\Data\\FSU\\CAPRI_NUTS_RG_01M_2016_3035_LEVL_3_plus_BA_XK_final_wfoa\\CAPRI_NUTS_RG_01M_2016_3035_LEVL_3_plus_BA_XK_final_wfoa_FSUADM2_ID_corrected.csv", header=T)
#kk <- unique(nuts_codes[, c(3,length(nuts_codes))])
#write.csv(kk, "E:\\FSUs\\FSUADM2_CAPRINUTS2.csv", row.names = FALSE)


#nuts_codes1 <- as.data.table(nuts_codes@data)
nuts_codes1 <- as.data.table(nuts_codes)
#nuts_codes1$FSUADM3_ID <- as.numeric(as.character(nuts_codes1$FSUADM3_ID))
FSU_delim <- as.data.table(FSU_delim)
apply(nuts_codes1, 2, function(x) length(unique(x)))
head(nuts_codes1)


FSU_delim_all <- merge(FSU_delim, nuts_codes1, by = "FSUADM3_ID", all = TRUE)
FSU_delim_all <- FSU_delim_all[!is.na(FSU_delim_all$FSU), ]
#View(FSU_delim_all[FSU_delim_all$FSU == "39710kmE510N21918541", ])


#FSU_delim_all <- FSU_delim_all[, !names(FSU_delim_all) %in% c("OBJECTID")]
write.csv(FSU_delim_all, "\\\\ies-ud01.jrc.it\\D5_agrienv\\Data\\FSU/USCIE_FSU_delin.csv", row.names = FALSE)
#FSU_delim_all <- as.data.table(read.csv("\\\\ies-ud01.jrc.it\\D5_agrienv\\Data\\FSU/USCIE_FSU_delin.csv", header = T))

FSU_delim_all1 <- FSU_delim_all
names(FSU_delim_all1)
FSU_delim_all1 <- FSU_delim_all1[, c("FSUADM2_ID_corrected.y", "FSUADM3_ID", "FSUADM2_ID.x", "HSU2_CD_SO", "USCIE_RC", "comm_FSS", "OBJECTID", "FSS10_N2ID", "FSSmodif", "INSP10_ID", "EU28_CNTR", "CTRYSTATUS", "NUTS2_2016", "NUTS2_2010", "AREA_HA", "CAPRImodif", "sort", "N2_NA_2016", "FSUADM2_ID.y", "CNTR_NAME.y",  "FSS10_N3ID", "NUTS3_2010", "NUTS3_2016", "N3_NA_2016") := NULL]
names(FSU_delim_all1) <- gsub(".x$", "", names(FSU_delim_all1))
FSU_delim_all1 <- as.data.table(FSU_delim_all1)


FSU_delim_aggr <- as.data.table(FSU_delim_all1 %>% group_by(FSU) %>% summarise(FSU_area = n()))

FSU_delim_aggr <- merge(FSU_delim_all1, FSU_delim_aggr, by = "FSU")

#FSU_delim_aggr <- FSU_delim_aggr[, !names(FSU_delim_aggr) %in% c("USCIE_RC", "OBJECTID", "AREA_HA", "FSUADM3_ID", )]
#FSU_delim_aggr <- FSU_delim_aggr[!duplicated(FSU_delim_aggr), ]
FSU_delim_aggr <- unique(FSU_delim_aggr)

head(FSU_delim_aggr)
nrow(FSU_delim_aggr)
length(unique(FSU_delim_aggr$FSU))
#View(FSU_delim_aggr[duplicated(FSU_delim_aggr[, 1]), ])
#View(FSU_delim_aggr[FSU_delim_aggr$FSU == "21210kmE510N46819150", ])
names(FSU_delim_aggr)

#FSU_delim_aggr <- FSU_delim_aggr_notSort
FSU_delim_aggr_notSort <- copy(FSU_delim_aggr)
key(FSU_delim_aggr)
setkeyv(FSU_delim_aggr, NULL)
FSU_delim_aggr <- FSU_delim_aggr[order(FSUADM2_ID_corrected, CNTR_CODE, CAPRINUTS2, nogo, forest), ] #order afterward by soil and 10kmgrid
#FSU_delim_aggr <- FSU_delim_aggr[order(CNTR_CODE, CAPRINUTS2, nogo, forest), ]
View(FSU_delim_aggr)

write.csv(FSU_delim_aggr, "\\\\ies\\d5\\agrienv\\Data\\FSU/FSU_delin.csv", row.names = FALSE)
#FSU_delim_aggr <- fread("\\\\ies\\d5\\agrienv\\Data\\FSU/FSU_delin.csv", header = T)



### Calculating shares of Forest and NoGo

# Forest
forest_shares_1km <- raster(paste0("\\\\ies-ud01.jrc.it\\D5_agrienv\\Data\\FSU\\Forest_NoGo", "/forest_shares_1km.tif"))

forest_shares_1km_uscie <- crop(uscie, forest_shares_1km)
forest_shares_1km_uscie1 <- crop(forest_shares_1km, forest_shares_1km_uscie)

uscie_for_shr <- data.frame(values(forest_shares_1km_uscie), values(forest_shares_1km_uscie1))
names(uscie_for_shr) <- c("uscie", "forest_share")
uscie_for_shr <- uscie_for_shr[!is.na(uscie_for_shr$uscie), ]
uscie_for_shr <- uscie_for_shr[!is.na(uscie_for_shr$forest_share), ]

head(uscie_for_shr)
range(uscie_for_shr$forest_share)

FSU_delim_all <- fread("\\\\ies-ud01.jrc.it\\D5_agrienv\\Data\\FSU/USCIE_FSU_delin.csv", header = TRUE)
head(FSU_delim_all)

FSU_delim_all_shares <- merge(FSU_delim_all, uscie_for_shr, by.x = "USCIE_RC", by.y = "uscie")
FSU_delim_all_shares <- FSU_delim_all_shares[, c("USCIE_RC", "FSU", "forest_share"), with=FALSE]

for_shr <- as.data.table(FSU_delim_all_shares %>% group_by(FSU) %>% summarise(fsu_forest_share = mean(forest_share)))
for_shr$fsu_forest_share <- for_shr$fsu_forest_share / 100
for_shr

FSU_delim_aggr <- merge(FSU_delim_aggr, for_shr, by = "FSU")
FSU_delim_aggr
#

# NoGo
nogo_shares_1km <- raster(paste0("\\\\ies-ud01.jrc.it\\D5_agrienv\\Data\\FSU\\Forest_NoGo", "/nogo_shares_1km.tif"))

nogo_shares_1km_uscie <- crop(uscie, nogo_shares_1km)
nogo_shares_1km_uscie1 <- crop(nogo_shares_1km, nogo_shares_1km_uscie)

uscie_nogo_shr <- data.frame(values(nogo_shares_1km_uscie), values(nogo_shares_1km_uscie1))
names(uscie_nogo_shr) <- c("uscie", "nogo_share")
uscie_nogo_shr <- uscie_nogo_shr[!is.na(uscie_nogo_shr$uscie), ]
uscie_nogo_shr <- uscie_nogo_shr[!is.na(uscie_nogo_shr$nogo_share), ]

head(uscie_nogo_shr)
range(uscie_nogo_shr$nogo_share)

FSU_delim_all_shares <- merge(FSU_delim_all, uscie_nogo_shr, by.x = "USCIE_RC", by.y = "uscie")
FSU_delim_all_shares <- FSU_delim_all_shares[, c("USCIE_RC", "FSU", "nogo_share"), with=FALSE]

nogo_shr <- as.data.table(FSU_delim_all_shares %>% group_by(FSU) %>% summarise(fsu_nogo_share = mean(nogo_share)))
#nogo_shr$fsu_nogo_share <- round(nogo_shr$fsu_nogo_share / 100, 2)
nogo_shr$fsu_nogo_share <- nogo_shr$fsu_nogo_share / 100
nogo_shr

FSU_delim_aggr <- merge(FSU_delim_aggr, nogo_shr, by = "FSU")
#FSU_delim_aggr_copy <- copy(FSU_delim_aggr)
FSU_delim_aggr


head(FSU_delim_aggr)
FSU_delim_aggr$forest <- gsub(1, 2, FSU_delim_aggr$forest)
FSU_delim_aggr$forest <- as.numeric(FSU_delim_aggr$forest)
FSU_delim_aggr$nogo_flag <- FSU_delim_aggr$nogo + (FSU_delim_aggr$nogo * FSU_delim_aggr$forest)

#unique(FSU_delim_aggr[FSU_delim_aggr$NoGo_flag == 0, ]$fsu_nogo_share)
#unique(FSU_delim_aggr[FSU_delim_aggr$NoGo_flag == 0, ]$for_plus_nogo)
#FSU_delim_aggr[FSU_delim_aggr$fsu_nogo_share == 0.87, ]
#
#FSU_delim_aggr$for_plus_nogo <- FSU_delim_aggr$fsu_forest_share + FSU_delim_aggr$fsu_nogo_share 
#max(unique(FSU_delim_aggr[FSU_delim_aggr$NoGo_flag == 0, ]$for_plus_nogo))
#FSU_delim_aggr[FSU_delim_aggr$for_plus_nogo > 1, ]
#
#FSU_delim_aggr$for_plus_nogo <- NULL


FSU_delim_aggr$forest <- gsub(2, 1, FSU_delim_aggr$forest)
FSU_delim_aggr$nogo_flag <- gsub(3, 2, FSU_delim_aggr$nogo_flag)

setkeyv(FSU_delim_aggr, NULL)
FSU_delim_aggr <- FSU_delim_aggr[order(FSUADM2_ID_corrected, CNTR_CODE, CAPRINUTS2, nogo, forest), ] #order afterwards by soil and 10kmgrid
View(FSU_delim_aggr)


FSU_delim_aggr$runID <- paste0("F", c(1:nrow(FSU_delim_aggr)))
FSU_delim_aggr <- FSU_delim_aggr[, names(FSU_delim_aggr)[!names(FSU_delim_aggr) %in% c("nogo", "forest")], with = FALSE]
FSU_delim_aggr <- FSU_delim_aggr[, c("runID", names(FSU_delim_aggr)[1:(length(FSU_delim_aggr) -1 )]), with = FALSE]


write.csv(FSU_delim_aggr, "\\\\ies\\d5\\agrienv\\Data\\FSU/FSU_delin.csv", row.names = FALSE)
#FSU_delim_aggr <- fread("\\\\ies\\d5\\agrienv\\Data\\FSU/FSU_delin.csv", header = TRUE)





#



## Dissolve new FSU polygons and giving spatial information ####

head(FSU_delim)

FSU <- FSU_delim[, c("USCIE_RC", "FSU")]
head(FSU)
nrow(FSU)
length(unique(FSU$FSU))


unique(as.data.frame(uscie))

uscie_poly <- rasterToPolygons(uscie)
names(uscie_poly@data) <- "uscie"
writeOGR(obj = uscie_poly, dsn = "E:\\FSUs\\uscie_polyg", layer = "uscie_poly", driver = "ESRI Shapefile", overwrite_layer = TRUE)
#uscie_poly <- readOGR(dsn = "E:\\FSUs\\uscie_polyg", layer = "uscie_poly")


#uscie_fsu <- merge(uscie_poly, FSU, by.x = "uscie", by.y = "USCIE_RC", all.x = TRUE)
uscie_fsu <- merge(uscie_poly, FSU, by.x = "uscie", by.y = "USCIE_RC", all.y = TRUE)
head(uscie_fsu)
head(uscie_fsu@data)
nrow(uscie_fsu@data)
uscie_fsu

#uscie_fsu@data <- na.omit(uscie_fsu@data)

#uscie_fsu@data <- uscie_fsu@data[!is.na(uscie_fsu$FSU), ]
sum(is.na(uscie_fsu@data$FSU))
sum(is.na(uscie_fsu$FSU))
sum(is.na(uscie_fsu$uscie))


fsu <- unionSpatialPolygons(SpP = uscie_fsu, IDs = uscie_fsu$FSU)
head(row.names(fsu))
row.names(fsu) <- as.character(1:length(fsu))


A partir d'aqui hi ha un problema amb els poligons que tenen NA (1). Fent el que segueix, l'eliminem a sac i no se com es col>loquen els seguents
Revisar-ho!!
  
  
  fsu_unq <- unique(uscie_fsu$FSU)
fsu_unq <- as.data.frame(fsu_unq)
colnames(fsu_unq) <- "FSU"
fsu_unq <- as.data.frame(fsu_unq[which(!is.na(fsu_unq$FSU)), ])

fsu1 <- SpatialPolygonsDataFrame(fsu, fsu_unq)

writeOGR(obj = fsu1, dsn = "\\\\ies-ud01.jrc.it\\D5_agrienv\\Data\\FSU", layer = "FSU", driver = "ESRI Shapefile", overwrite_layer = TRUE)

length(unique(uscie_fsu$uscie))
length(unique(uscie_fsu$FSU))

Revisar fins aqui

















