require(raster)
library(rgdal)
library(maptools)
library(data.table)
library(dplyr)


### Reading in data: USCIE,  ####

# uscie spatial info
uscie <- raster("\\\\ies\\d5\\agrienv\\Data\\uscie\\uscie_raster_FSU\\refras_FSU_land.tif")
uscie@data@names <- 'uscie'
uscie

# data for delineaion (uscie, admin, 10km id, etc)
data4delin <- read.csv("\\\\ies\\d5\\agrienv\\Data\\uscie\\uscie_raster_FSU\\refras_FSU_land_soil_10km_admin.csv", header = TRUE)

data4delin$HSU2_CD_SO <- as.character(paste0("000", data4delin$HSU2_CD_SO))
data4delin$HSU2_CD_SO <- substr(data4delin$HSU2_CD_SO, (nchar(data4delin$HSU2_CD_SO) - 3), nchar(data4delin$HSU2_CD_SO))

#This is only to solve the problem with wrong FSUADM2_ID
#
#  !!al201908 - Please better commenting what is 'wrong' and what is the outcome of the code:
#               Which column to be used as unique administrative region?? Why??
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
  
  data4delin <- as.data.table(data4delin1)
  save(data4delin, file="\\\\ies\\d5\\agrienv\\Data\\uscie\\uscie_raster_FSU\\refras_FSU_land_soil_10km_admin_FSUADM2_ID_corrected.rdata")
  write.csv(data4delin, "\\\\ies\\d5\\agrienv\\Data\\uscie\\uscie_raster_FSU\\refras_FSU_land_soil_10km_admin_FSUADM2_ID_corrected.csv", row.names = FALSE)
  
}


# Forest
dir2save <- "x:\\adrian/data/fsu"
dir2save <- "\\\\ies-ud01.jrc.it\\D5_agrienv\\Data\\FSU\\Forest_NoGo"
forest <- raster(paste0(dir2save, "/forest_only_1km.tif"))
uscie_for <- convertRaster2datatable(forest, uscie)

# NoGo
nogo <- raster(paste0(dir2save, "/nogo_1km.tif"))
uscie_nogo <- convertRaster2datatable(nogo, uscie)

# NoGO + Forest
# 
# al201908 - there is something strange. there can't be a nogo and forst unit at the same time.
#            but in the merge there are combinations 0-0, 0-1, and 1-1.
#            so it seems that something is reverted??
#            Maybe it was the 'by='uscie'?? 
uscie_nogo_forest <- merge(uscie_nogo, uscie_for, by = "uscie", all = TRUE)
uscie_nogo_forest <- as.data.table(uscie_nogo_forest)
names(uscie_nogo_forest) <- c("uscie", "nogo", "forest")
unique(uscie_nogo_forest[, 2:3])
save(uscie_nogo_forest, file="\\\\ies\\d5\\agrienv\\Data\\uscie\\uscie_raster_FSU\\uscie_map2nogo_and_forest.rdata")

checklago <- TRUE
if(checklago){
  
  etnt <- extent(nogo)
  etnt@xmin <- 4150000 
  etnt@xmax <- 4300000
  etnt@ymin <- 2500000
  etnt@ymax <- 2620000
  lagonogo <- crop(nogo, etnt)
  lagofore <- crop(forest, etnt)
  lagonogodt <- convertRaster2datatable(rast1=lagonogo, rast2=uscie)
  lagoforedt <- convertRaster2datatable(rast1=lagofore, rast2=uscie)
  lagodt <- merge(lagonogodt, lagoforedt, by=uscie@data@names)
  
}



# FSU_delim: data for delineaion + NOGO + FOREST
FSU_delim_all <- merge(data4delin, uscie_nogo_forest, by.x = "USCIE_RC", by.y = "uscie", all = TRUE)

# go FSUs
# al201908 - nogo_1km.tif saves nogo pixel as '0' and go pixel as '1'
FSU_delim_go <- FSU_delim_all[FSU_delim_all$nogo == 1, ]
FSU_delim_go <- FSU_delim_go[!is.na(FSU_delim_go$nogo), ]

FSU_delim_go$FSU <- paste0(FSU_delim_go$FSUADM2_ID_corrected, # Admin region at NUT2 level
                           FSU_delim_go$INSP10_ID,            # 10 km Inspire grid cell
                           FSU_delim_go$HSU2_CD_SO,           # Soil mapping unit
                           FSU_delim_go$forest+1)               # Forest
#FSU_delim_go$FSU <- paste0(FSU_delim_go$FSUADM2_ID, FSU_delim_go$INSP10_ID, FSU_delim_go$HSU2_CD_SO, FSU_delim_go$forest)
#FSU_delim_go$FSU <- paste0(FSU_delim_go$FSUADM3_ID, FSU_delim_go$INSP10_ID, FSU_delim_go$HSU2_CD_SO, FSU_delim_go$forest)

# nogo FSUs
# For nogos there is no need of saving / delineating by soil type and forest
FSU_delim_nogo <- FSU_delim_all[FSU_delim_all$nogo == 0, ]
FSU_delim_nogo <- FSU_delim_nogo[!is.na(FSU_delim_nogo$nogo), ]
FSU_delim_nogo$FSU <- paste0(FSU_delim_nogo$FSUADM2_ID_corrected, 
                             FSU_delim_nogo$INSP10_ID,
                             "0000", #Add four characters for soil (but ignore if HSU2_CD_SO is different as we don't want to split the nogo units by soil type),
                             "0"     #Add one character for 'nogo'
)
# Delete soil mapping unit from the nogo units so that they won't be split into several pieces...
FSU_delim_nogo$HSU2_CD_SO <- ""

# FSU_delim - combind first go units, then nogo units
FSU_delim <- rbind(FSU_delim_go, FSU_delim_nogo)
nrow(FSU_delim)
class(FSU_delim)
#FSU_delim_copy <- FSU_delim
#FSU_delim <- FSU_delim_copy
x <- as.data.table(FSU_delim)
save(x, file="FSU_delim_line117.rdata")
FSU_delim <- as.data.table(FSU_delim)

#Adding NUTS codes, etc

#nuts_codes <- paste0("", "CAPRI_NUTS_RG_01M_2016_3035_LEVL_3_plus_BA_XK_final_wfoa.shp")
#nuts_codes <- readOGR(dsn = "\\\\ies-ud01.jrc.it\\D5_agrienv\\Data\\FSU\\CAPRI_NUTS_RG_01M_2016_3035_LEVL_3_plus_BA_XK_final_wfoa", layer = "CAPRI_NUTS_RG_01M_2016_3035_LEVL_3_plus_BA_XK_final_wfoa")
#nuts_codes <- foreign:::read.dbf("\\\\ies-ud01.jrc.it\\D5_agrienv\\Data\\FSU\\CAPRI_NUTS_RG_01M_2016_3035_LEVL_3_plus_BA_XK_final_wfoa\\CAPRI_NUTS_RG_01M_2016_3035_LEVL_3_plus_BA_XK_final_wfoa.dbf")
#nuts_codes <- read.csv("E:\\FSUs\\CAPRI_NUTS_RG_01M_2016_3035_LEVL_3_plus_BA_XK_final_wfoa\\CAPRI_NUTS_RG_01M_2016_3035_LEVL_3_plus_BA_XK_final_wfoa.csv", header=T)
nuts_codes <- read.csv("\\\\ies-ud01.jrc.it\\D5_agrienv\\Data\\FSU\\admin_units\\CAPRI_NUTS_RG_01M_2016_3035_LEVL_3_plus_BA_XK_final_wfoa.csv", header=T)
nuts_codes1 <- as.data.table(nuts_codes)
apply(nuts_codes1, 2, function(x) length(unique(x)))
head(nuts_codes1)
FSU_delim_all <- merge(FSU_delim[, .(USCIE_RC, FSU, FSUADM3_ID, FSUADM2_ID_corrected, 
                                     OBJECTID, HSU2_CD_SO, INSP10_ID, nogo, forest)], 
                       nuts_codes1, by = "FSUADM3_ID", all = TRUE)
FSU_delim_all <- FSU_delim_all[!is.na(FSU_delim_all$FSU), ]
#View(FSU_delim_all[FSU_delim_all$FSU == "39710kmE510N21918541", ])

#uscie4fsu_delimdata <- as.data.table(FSU_delim_all)
uscie4fsu_delimdata <- as.data.table(FSU_delim_all)
save(uscie4fsu_delimdata, file="\\\\ies-ud01.jrc.it\\D5_agrienv\\Data\\FSU/uscie4fsu_delimdata.rdata")
writeuscie4fsu <- function(x){
  wfile <- file("\\\\ies-ud01.jrc.it\\D5_agrienv\\Data\\FSU/uscie4fsu_delimdata_readme.txt", open="w")
  hlines <- paste0("#Data used for the delineation of the Farm Structure Units (FSU)",
                    "\n#Date generated: 19/08/2019 - Adrian Leip",
                    "\n#Script: FSU_delineation_uscie.r - Repository https://github.com/aleip/FarmstructureSoilUnits",
                    "\n#\n#Content of the data set:",
                    "\n#FSUADM3_ID OBJECTID USCIE_RC HSU2_CD_SO    INSP10_ID     CNTR_NAME FSUADM2_ID FSUADM2_ID_corrected",
                    "\n#    --> see ies-ud01.jrc.it/D5_agrienv/Data/uscie/uscie_raster_FSU/readme_refras_FSU_land_soil_10km_admin_csv.txt",
                    "\n#USCIE_RC	USCIE coding - Canary Islands Azores and Madeira added.",
                    "\n#HSU2_CD_SO	HSU2?0?to?4?digits?code?for?the?soil?mapping?unit This is the soil mapping unit code to use for the FSU delineation",
                    "\n#INSP10_ID	ID of the 10km INSPIRE grid: This is the 10km grid unit to use for the FSU delineation",
                    "\n#NUTS3_2016	Nuts3 code",
                    "\n#FSUADM2_ID_corrected	NUTS2 code: This is the administrative unit to use for the FSU delineation",
                    "\n#nogo:   Code determining >90% 'nogo' Corine Classes or >90% Corine forests", 
                    "\n#        Corine used: \\\\ies-ud01.jrc.it\\D5_agrienv\\Data\\Corine_Land_Cover\\clc2018_v20_incl_turkey\\7ac95361f9ac3cecdf37785bc183ff02dd765a16\\clc2018_clc2018_v2018_20_raster100m/", 
                    "\n#                     CLC2018_CLC2018_V2018_20.tif.ovr",
                    "\n#        Script used: https://github.com/aleip/FarmstructureSoilUnits/blob/master/corine2uscie.r ",
                    "\n#        List of nogo classes: 111: Continuous_urban_fabric - 112: Discontinuous_urban_fabric - 121: Industrial_or_commercial_units - 122: Road_and_rail_networks_and_associated_land - 123: Port_areas - 124: Airports - 131: Mineral_extraction_sites - 132: Dump_sites - 133: Construction_sites - 141: Green_urban_areas - 142: Sport_and_leisure_facilities - 331: Beaches_dunes_sands - 332: Bare_rocks - 335: Glaciers_and_perpetual_snow - 422: Salines - 423: Intertidal_flats - 511: Water_courses - 512: Water_bodies - 521: Coastal_lagoons - 522: Estuaries - 523: Sea_and_ocean",
                    "\n#forest: Code determining >90% Corine forests",
                    "\n#FSU string determining unique FSU",
                    "\n#    -  3 characters FSUADM2_ID numeric values between 100 and 435 ",
                    "\n#    - 12 characters INSP10_ID composed from string 10km and geographic position E and N (3 digits each)",
                    "\n#    -  4 characters HSU2_CD_SO",
                    "\n#    -  1 character  nogo: 0 = nogo unit 1 = go unit 2 = forest unit")
  #Don't save all admin names here as it will becomes too large
  writeLines(hlines, wfile)
  close(wfile)
  wfile <- file("\\\\ies-ud01.jrc.it\\D5_agrienv\\Data\\FSU/uscie4fsu_delimdata.csv", open="w")
  writeLines(hlines, wfile)
  x <- x[, .(fsuID, USCIE_RC, FSUADM2_ID_corrected, NUTS3_2016, HSU2_CD_SO, INSP10_ID, nogo, forest, FSU)]
  write.csv(x, row.names = FALSE, wfile)
  close(wfile)
  
}
writeuscie4fsu(uscie4fsu_delimdata)

