#### 
#### AGGREGATE from USCIE to FSU
#### 
#### -- Follow up of FSU_delineation_uscie.r
#### 
####    Here the data table for uscies is loaded and aggregated to FSUs
#### 
#### 

#FSU_delim_all <- fread("\\\\ies-ud01.jrc.it\\D5_agrienv\\Data\\FSU/USCIE_FSU_delin.csv", header = T) # one line per uscie
load("//ies-ud01.jrc.it/D5_agrienv/Data/FSU/uscie4fsu_delimdata.rdata")
FSU_delim_all1 <- uscie4fsu_delimdata
names(FSU_delim_all1)

## Aggregate and calculate the FSU area as sum of USCIEs per FSU
FSU_delim_aggr <- FSU_delim_all1[, .N, by = .(FSU, nogo, forest, INSP10_ID, FSUADM2_ID_corrected, HSU2_CD_SO, 
                                              CNTR_CODE, CNTR_NAME, CAPRINUTS0, CAPRINUTS2)]
setnames(FSU_delim_aggr, "N", "FSU_area")
names(FSU_delim_aggr) <- gsub(".x$", "", names(FSU_delim_aggr))


#FSU_delim_aggr <- FSU_delim_aggr[, !names(FSU_delim_aggr) %in% c("USCIE_RC", "OBJECTID", "AREA_HA", "FSUADM3_ID", )]
#FSU_delim_aggr <- FSU_delim_aggr[!duplicated(FSU_delim_aggr), ]
head(FSU_delim_aggr)
nrow(FSU_delim_aggr)
length(unique(FSU_delim_aggr$FSU))
#View(FSU_delim_aggr[duplicated(FSU_delim_aggr[, 1]), ])
#View(FSU_delim_aggr[FSU_delim_aggr$FSU == "21210kmE510N46819150", ])
names(FSU_delim_aggr)

## Modify nogo flag such that:
##  - go = 1
##  - forest = 2
##  - nogo = 0
## al201908 - don't understand formula below: why multiply with nogo??  
## FSU_delim_aggr$nogo_flag <- FSU_delim_aggr$nogo + (FSU_delim_aggr$nogo * FSU_delim_aggr$forest)
FSU_delim_aggr <- FSU_delim_aggr[, go := nogo + forest]
unique(FSU_delim_aggr[, .(nogo, forest, go)])
FSU_delim_aggr <- FSU_delim_aggr[, -c("nogo", "forest"), with=FALSE]


#FSU_delim_aggr <- FSU_delim_aggr_notSort
FSU_delim_aggr_notSort <- copy(FSU_delim_aggr)
key(FSU_delim_aggr)
setkeyv(FSU_delim_aggr, NULL)
FSU_delim_aggr <- FSU_delim_aggr[order(FSUADM2_ID_corrected, CNTR_CODE, CAPRINUTS2, go), ] #order afterward by soil and 10kmgrid
#FSU_delim_aggr <- FSU_delim_aggr[order(CNTR_CODE, CAPRINUTS2, nogo, forest), ]
View(FSU_delim_aggr)


FSU_delim_aggr$fsuID <- paste0("F", c(1:nrow(FSU_delim_aggr)))
#FSU_delim_aggr <- FSU_delim_aggr[, names(FSU_delim_aggr)[!names(FSU_delim_aggr) %in% c("nogo", "forest")], with = FALSE]
fsucols <- names(FSU_delim_aggr)
fsustart <- c("fsuID", "CAPRINUTS2", "FSU_area", "go")
fsuort <- c(fsustart, setdiff(fsucols, fsustart))

fsu_delimdata <- FSU_delim_aggr[, fsuort, with = FALSE]


write.csv(fsu_delimdata, "\\\\ies\\d5\\agrienv\\Data\\FSU/fsu_delimdata.csv", row.names = FALSE)
save(fsu_delimdata, file="\\\\ies\\d5\\agrienv\\Data\\FSU/fsu_delimdata.rdata")
wfile <- file("\\\\ies-ud01.jrc.it\\D5_agrienv\\Data\\FSU/fsu_delimdata_readme.txt", open="w")
writeLines(paste0("#Farm Structure SOil Units (FSU) and delineating date",
                  "\n#Date generated: 19/08/2019 - Adrian Leip",
                  "\n#Script: FSU_delineation.r - Repository https://github.com/aleip/FarmstructureSoilUnits - https://github.com/aleip/FarmstructureSoilUnits/blob/master/FSU_delineation.r",
                  "\n#\n#Content of the data set:",
                  "\n#fsuID - ID of FSU with 'F'. The FSU were ordered as follows order(FSUADM2_ID_corrected, CNTR_CODE, CAPRINUTS2, go)",
                  "\n#FSU_area: Area of FSU [km2]",
                  "\n#go: 0=nogo area excluded from agricultural use. ",
                  "\n#      nogo:   Code determining >90% 'nogo' Corine Classes or >90% Corine forests", 
                  "\n#              Corine used: \\\\ies-ud01.jrc.it\\D5_agrienv\\Data\\Corine_Land_Cover\\clc2018_v20_incl_turkey\\7ac95361f9ac3cecdf37785bc183ff02dd765a16\\clc2018_clc2018_v2018_20_raster100m/", 
                  "\n#                          CLC2018_CLC2018_V2018_20.tif.ovr",
                  "\n#              Script used: https://github.com/aleip/FarmstructureSoilUnits/blob/master/corine2uscie.r ",
                  "\n#              List of nogo classes: 111: Continuous_urban_fabric - 112: Discontinuous_urban_fabric - 121: Industrial_or_commercial_units - 122: Road_and_rail_networks_and_associated_land - 123: Port_areas - 124: Airports - 131: Mineral_extraction_sites - 132: Dump_sites - 133: Construction_sites - 141: Green_urban_areas - 142: Sport_and_leisure_facilities - 331: Beaches_dunes_sands - 332: Bare_rocks - 335: Glaciers_and_perpetual_snow - 422: Salines - 423: Intertidal_flats - 511: Water_courses - 512: Water_bodies - 521: Coastal_lagoons - 522: Estuaries - 523: Sea_and_ocean",
                  "\n#    1=go area can be used for agricultural land",
                  "\n#    2=forest area  >90% Corine forests",
                  "\n#FSU string determining unique FSU",
                  "\n#    -  3 characters FSUADM2_ID numeric values between 100 and 435 ",
                  "\n#    - 12 characters INSP10_ID composed from string 10km and geographic position E and N (3 digits each)",
                  "\n#    -  4 characters HSU2_CD_SO",
                  "\n#    -  1 character  nogo: 0 = nogo unit 1 = go unit 2 = forest unit",
                  "\n#HSU2_CD_SO	HSU2 0 to 4 digits code for the soil mapping unit This is the soil mapping unit code to use for the FSU delineation",
                  "\n#INSP10_ID	ID of the 10km INSPIRE grid: This is the 10km grid unit to use for the FSU delineation",
                  "\n#Admin regions: ",
                  "\n#FSUADM2_ID	NUTS2 code: This is the administrative unit to use for the FSU delineation",
                  "\n#    --> see ies-ud01.jrc.it/D5_agrienv/Data/uscie/uscie_raster_FSU/readme_refras_FSU_land_soil_10km_admin_csv.txt"
                  ), wfile)
#Don't save all admin names here as it will becomes too large
close(wfile)
#FSU_delim_aggr <- fread("\\\\ies\\d5\\agrienv\\Data\\FSU/FSU_delin.csv", header = T)

fsuID2FSU <- FSU_delim_aggr[, .(fsuID, FSU)]
uscie2FSU <- FSU_delim_all[, USCIE_RC, FSU]
uscie2fsu <- merge(uscie2FSU, fsuID2FSU, by="FSU")
uscie2fsu <- uscie2fsu[, fsuNo := as.numeric(gsub("F", "", fsuID))]
setkey(uscie2fsu, "fsuNo")
uscie2fsu <- uscie2fsu[, .(fsuID, USCIE_RC)]
save(uscie2fsu, file="\\\\ies\\d5\\agrienv\\Data\\FSU/uscie2fsu.rdata")
write.csv(uscie2fsu, file="\\\\ies\\d5\\agrienv\\Data\\FSU/uscie2fsu.csv")
wfile <- file("\\\\ies-ud01.jrc.it\\D5_agrienv\\Data\\FSU/uscie2fsu_readme.txt", open="w")
writeLines(paste0("#Matching table between uscie and Farm Structure SOil Units (FSU)",
                  "\n#Date generated: 19/08/2019 - Adrian Leip",
                  "\n#Script: FSU_delineation.r - Repository https://github.com/aleip/FarmstructureSoilUnits - https://github.com/aleip/FarmstructureSoilUnits/blob/master/FSU_delineation.r",
                  "\n#For uscie see: \\\\ies-ud01.jrc.it/D5_agrienv/Data/FSU/uscie4fsu_delimdata_readme.txt",
                  "\n#For FSU see \\\\ies-ud01.jrc.it/D5_agrienv/Data/FSU/fsu_delimdata_readme.txt"
), wfile)
#Don't save all admin names here as it will becomes too large
close(wfile)

## Dissolve new FSU polygons and giving spatial information ####
addspatinfo <- FALSE
if(addspatinfo){
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
}
















