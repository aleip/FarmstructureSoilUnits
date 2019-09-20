#### 
#### AGGREGATE from USCIE to FSU
#### 
#### -- Follow up of FSU_delineation_uscie.r
#### 
####    Here the data table for uscies is loaded and aggregated to FSUs
#### 
#### 
library(data.table)
library(reshape2)
library(gdxrrw)
library(magrittr)

### Number of records 
### FSU_delim_all1:     5834930 (from FSU_delineation_uscie)
### usciedelim          5834930
### FSU_delim_aggr       267082 (at first aggregation)
### FSU_delim_aggr       267082 (also after UK and FI manipulation)
### fi1asplit            267082
### usciedelim          5834930
### uscie4fsu_delimdata 5834930 
### uscie2fsu           5834930


#FSU_delim_all <- fread("\\\\ies-ud01.jrc.it\\D5_agrienv\\Data\\FSU/USCIE_FSU_delin.csv", header = T) # one line per uscie
load("//ies-ud01.jrc.it/D5_agrienv/Data/FSU/uscie4fsu.rdata")
FSU_delim_all1 <- uscie4fsu_delimdata
## Modify nogo flag such that:
##  - go = 1
##  - forest = 2
##  - nogo = 0
## al201908 - don't understand formula below: why multiply with nogo??  
## FSU_delim_aggr$nogo_flag <- FSU_delim_aggr$nogo + (FSU_delim_aggr$nogo * FSU_delim_aggr$forest)
FSU_delim_all1 <- FSU_delim_all1[, go := nogo + forest]
unique(FSU_delim_all1[, .(nogo, forest, go)])
FSU_delim_all1 <- FSU_delim_all1[, -c("nogo", "forest"), with=FALSE]
names(FSU_delim_all1)
#uscie2FSU <- FSU_delim_all1[, USCIE_RC, FSU]

FSU_delim_all1 <- FSU_delim_all1[, FSU := paste0(FSU_delim_all1$FSUADM2_ID, 
                                                 FSU_delim_all1$INSP10_ID, 
                                                 FSU_delim_all1$HSU2_CD_SO_delim,
                                                 FSU_delim_all1$go)]
usciedelim <- FSU_delim_all1[, .(USCIE_RC, FSU, FSUADM2_ID = as.character(FSUADM2_ID), 
                                 INSP10_ID, HSU2_CD_SO_delim, go, FSUADM3_ID = as.character(FSUADM3_ID))]
str(usciedelim)
## Aggregate and calculate the FSU area as sum of USCIEs per FSU
fsucols <- c("FSU", "FSUADM2_ID", "INSP10_ID", "HSU2_CD_SO_delim", "go", "CNTR_CODE", "CNTR_NAME", "CAPRINUTS0", "CAPRINUTS2", "FSU_area")
FSU_delim_aggr <- FSU_delim_all1[, .N, by = .(FSU, FSUADM2_ID, INSP10_ID, HSU2_CD_SO_delim, go, 
                                              CNTR_CODE, CNTR_NAME, CAPRINUTS0, CAPRINUTS2)]
setnames(FSU_delim_aggr, "N", "FSU_area")
save(usciedelim, FSU_delim_aggr, file="line41.rdata")

dosplitUK <- TRUE
if(dosplitUK){
  #UKM and FI1A are very slow ... split??
  #UK units are too big and take too long - use 'real' nuts2
  fi2split <- c("FI1A0000", "FI130000", "FI130000")
  splitregions <- c("UKM00000")
  splitregions <- c("UKM00000", fi2split)
  
  checkIDs <- unique(FSU_delim_all1[, .(FSUADM2_ID, CAPRINUTS2, FSS10_N2ID, NUTS2_2016)])
  
  fsuUKM <- FSU_delim_all1[CAPRINUTS2=="UKM00000", .N, by = .(FSU, FSUADM2_ID, INSP10_ID, HSU2_CD_SO_delim, go,
                                                              CNTR_CODE, CNTR_NAME, CAPRINUTS0, NUTS2_2016)]
  # Generates 10940 FSU
  fsuFI1A <- FSU_delim_all1[CAPRINUTS2 %in% fi2split, .N, by = .(FSU, FSUADM2_ID, INSP10_ID, HSU2_CD_SO_delim, go,
                                                               CNTR_CODE, CNTR_NAME, CAPRINUTS0, FSUADM3_ID)]
  finuts <- unique(FSU_delim_all1[CNTR_CODE == "FI", .(NUTS2_2016, CAPRINUTS2, FSUADM2_ID, NUTS3_2016, FSUADM3_ID)])
  fin3 <- unique(FSU_delim_all1[CAPRINUTS2 %in% fi2split, .(FSUADM2_ID, NUTS3_2016, FSUADM3_ID)])
  # Need to split region into NUTS3. However, the FSU structure should not be changed. Therefore assign each FSU to one NUTS3
  
  # Generates 10557 rows = 383 less. fi1asplit has a length of 381
  fi1a <- dcast.data.table(fsuFI1A[, N, by=c("FSU", "FSUADM3_ID")], FSU ~ FSUADM3_ID, value.var="N", length, fill=0)
  fi1a <- fi1a[, nNuts := `1672` + `1673` + `1674` + `1675` + `1676` + `1689` + `1690` + `1691`]
  fi1asplit <- fi1a[nNuts>1, FSU]
  length(fi1asplit)
  # Find dominant NUTS for split-FSU
  #fi1anuts <- dcast.data.table(fsuFI1A[FSU %in% fi1asplit, .(FSU, FSUADM2_ID_corrected, NUTS3_2016, N)], 
  #                   FSU + FSUADM2_ID_corrected ~ NUTS3_2016, value.var="N", fill=0)
  fi1anuts <- dcast.data.table(fsuFI1A[FSU %in% fi1asplit, .(FSU, FSUADM3_ID, N)], 
                     FSU ~ FSUADM3_ID, value.var="N", fill=0)
  fi1anuts <- fi1anuts[, FSUADM3_ID := names(.SD)[max.col(.SD)], .SDcols = setdiff(names(fi1anuts), "FSU")]
  fi1anuts <- fi1anuts[, N := `1672` + `1673` + `1674` + `1675` + `1676` + `1689` + `1690` + `1691`]
  fi1anuts <- fi1anuts[, .(FSU, FSUADM3_ID, N)]
  
# Merge back and rename the FSUs to indicate the (dominant) NUTS3  
# Note fsuFI1A contains some FSUs in duplicates
#      the two vectors should have the same length 
  a <- unique(fsuFI1A[ FSU %in% fi1asplit, .(FSU, FSUADM2_ID, INSP10_ID, HSU2_CD_SO_delim, go, CNTR_CODE, CNTR_NAME, CAPRINUTS0)])
  b <- fsuFI1A[!  FSU %in% fi1asplit, .(FSU, FSUADM2_ID, INSP10_ID, HSU2_CD_SO_delim, go, CNTR_CODE, CNTR_NAME, CAPRINUTS0, FSUADM3_ID, N)]
  c <- merge(a, fi1anuts, by = "FSU", all.x = TRUE)
  fsuFI1A2 <- rbind(b, c)
  length(unique(fsuFI1A2$FSU))
  cfsu1 <- unique(fsuFI1A$FSU); length(cfsu1)
  cfsu2 <- unique(fsuFI1A2$FSU); length(cfsu2)
  #fsuFI1A2 <- merge(fsuFI1A2, fin3, by=c("FSUADM2_ID", "FSUADM3_ID"), all.x=TRUE)
  
# Update FSUADM2_ID in both fsudata and usciedata
# 6443 in fsuFI1A3
# 6584 in length(unique(usciedelim[grepl("^214", FSUADM2_ID)]$FSU))
  fsuFI1A3 <- fsuFI1A2[, FSUADM2_ID := paste0(FSUADM2_ID, FSUADM3_ID)]
  usciedelim <- usciedelim[FSU %in% unique(fsuFI1A3$FSU), FSUADM2_ID := paste0(FSUADM2_ID, FSUADM3_ID)]
  #Re-generate FSU and update usciedelim
  fsuFI1A3 <- fsuFI1A3[, FSUold := FSU]
  fsuFI1A3 <- fsuFI1A3[, FSU := paste0(fsuFI1A3$FSUADM2_ID, 
                                       fsuFI1A3$INSP10_ID, 
                                       fsuFI1A3$HSU2_CD_SO_delim,
                                       fsuFI1A3$go)]
  replaceFSU <- fsuFI1A3[, .(FSUold, FSUnew = FSU) ]
  usciedelim <- usciedelim[, FSUold := FSU]
  usciedelim <- merge(usciedelim, replaceFSU, by="FSUold", all.x=TRUE)
  usciedelim <- usciedelim[!is.na(FSUnew), FSU := FSUnew]
  usciedelim <- usciedelim[, .(USCIE_RC, FSU, FSUADM2_ID, INSP10_ID, HSU2_CD_SO_delim, go, FSUADM3_ID)]
  
  nrow(fsuFI1A3)
  length(unique(usciedelim[grepl("^21[24]", FSUADM2_ID)]$FSU))
  # Make sure the same # of FSUs are kept
  
    
  #newFSUs <- fsuFI1A3[, .(FSU, FSUn)]
  #updateuscie <- merge(newFSUs, uscie2FSU, by="FSU")
  #updateuscie <- updateuscie[, .(FSU = FSUn, USCIE_RC)]
  #olduscie <- uscie2FSU[! FSU %in% unique(newFSUs$FSU)]
  #uscie2FSU <- rbind(olduscie, updateuscie)
  finuts$FSUADM3_ID <- as.character(finuts$FSUADM3_ID)
  fsuFI1A3 <- merge(fsuFI1A3, finuts[, .(FSUADM3_ID, NUTS3_2016)], by="FSUADM3_ID")
  fsuFI1A3 <- fsuFI1A3[, CAPRINUTS2 := NUTS3_2016]
  setnames(fsuFI1A3, "N", "FSU_area")
  fsuFI1A3 <- fsuFI1A3[, fsucols, with=FALSE]
  # Only 4 digits in CAPRI, therefore remove the '1' for FI-NUTS3 regions
  fsuFI1A3 <- fsuFI1A3[, CAPRINUTS2 := gsub("1", "", CAPRINUTS2)]
  
# Rename to CAPRI style
  setnames(fsuUKM, "N", "FSU_area")
  setnames(fsuUKM, "NUTS2_2016", "CAPRINUTS2")
  
# Add zeros to adhere to CAPRI style
  fsuUKM <- fsuUKM[, CAPRINUTS2 := paste0(CAPRINUTS2, "0000")]
  fsuFI1A3 <- fsuFI1A3[, CAPRINUTS2 := paste0(CAPRINUTS2, "0000")]
  
# Combine split regions with non-splitregions
  fsusplit <- rbind(fsuUKM[, fsucols, with=FALSE], fsuFI1A3[, fsucols, with=FALSE])
  fsuNotSplit <- FSU_delim_aggr[! CAPRINUTS2 %in% splitregions]
  FSU_delim_aggr <- rbind(fsuNotSplit, fsusplit)
  
# Create link between capri countries, capri nuts2 and NUTS used for disaggregation
  splitUKM <- unique(FSU_delim_all1[CAPRINUTS2=="UKM00000", .(CAPRINUTS0, CAPRINUTS2, NUTS2_2016)])
  splitFI1A <- unique(FSU_delim_all1[CAPRINUTS2 %in% fi2split, .(CAPRINUTS0, CAPRINUTS2, NUTS3_2016)])
  setnames(splitUKM, "NUTS2_2016", "split")
  setnames(splitFI1A, "NUTS3_2016", "split")
  # Add zeros to adhere to CAPRI style
  splitUKM <- splitUKM[, split := paste0(split, "0000")]
  splitFI1A <- splitFI1A[, split := paste0(split, "000")]
  split <- rbind(splitUKM, splitFI1A)
  setsplit <- split[, set1 := paste0(CAPRINUTS0, " . ", split)]
  setsplit <- setsplit[, set2 := paste0(CAPRINUTS2, " . ", split)]
  
  
  fcon <- file("s_srnuts2_for_disagg.gms", open="w")
  writeLines("set s_splitregions(rall) 'CAPRI NUTS2 regions to split for disaggregation as they are requiring very long time' /", fcon)
  writeLines(paste(unique(split$CAPRINUTS2), collapse = ", "), fcon)
  writeLines("/;", fcon)
  writeLines("set s_disaggregions(*) 'New regions for disaggregation. UKM: NUTS2, FI1A: NUTS3' /", fcon)
  writeLines(paste(unique(split$split), collapse = ", "), fcon)
  writeLines("/;", fcon)
  writeLines("set m_splitregions(rall, *) 'Mapping CAPRINUTS to disagg regions' /", fcon)
  write.table(setsplit[, .(set2)], quote=FALSE, col.names=FALSE, row.names=FALSE, fcon)
  writeLines("/;", fcon)
  writeLines("set m_splitcountry(rall, *) 'Mapping CAPRINUTS to disagg regions' /", fcon)
  write.table(setsplit[, .(set1)], quote=FALSE, col.names=FALSE, row.names=FALSE, fcon)
  writeLines("/;", fcon)
  close.connection(fcon)
  
  
}
head(FSU_delim_aggr)
nrow(FSU_delim_aggr)
length(unique(FSU_delim_aggr$FSU))
names(FSU_delim_aggr)
save(FSU_delim_aggr, file="FSU_delim_aggr_after_split.rdata")



#FSU_delim_aggr <- FSU_delim_aggr_notSort
FSU_delim_aggr_notSort <- copy(FSU_delim_aggr)
key(FSU_delim_aggr)
setkeyv(FSU_delim_aggr, NULL)
FSU_delim_aggr <- FSU_delim_aggr[order(FSUADM2_ID, CNTR_CODE, CAPRINUTS2, go), ] #order afterward by soil and 10kmgrid

FSU_delim_aggr$fsuID <- paste0("F", c(1:nrow(FSU_delim_aggr)))
fsucolsd <- names(FSU_delim_aggr)
fsustart <- c("fsuID", "CAPRINUTS2", "FSU_area", "go")
fsuort <- c(fsustart, setdiff(fsucolsd, fsustart))
fsu_delimdata <- FSU_delim_aggr[, fsuort, with = FALSE]

uscie4fsu_delimdata <- merge(usciedelim[, .(FSU, USCIE_RC, FSUADM3_ID)], fsu_delimdata, by="FSU", all=TRUE)
uscie4fsu_delimdata[is.na(USCIE_RC)] #   0 FSU in fsu_delimdata without USCIE_RC
uscie4fsu_delimdata[is.na(fsuID)]    #   0 FSU in usciedelim    without fsuID

uscie4fsu_delimdata <- uscie4fsu_delimdata[, fsuID_nr := as.numeric(gsub("F", "", fsuID))]
uscie4fsu_delimdata <- uscie4fsu_delimdata[order(fsuID_nr, USCIE_RC)]
uscie4fsu_delimdata <- uscie4fsu_delimdata[, c("fsuID", "fsuID_nr", "USCIE_RC", fsucols, "FSUADM3_ID"), with=FALSE]
# NAs in the string of the soil code already in HSU2_CD_SO_delim
uscie4fsu_delimdata <- uscie4fsu_delimdata[grepl("NA", HSU2_CD_SO_delim), HSU2_CD_SO_delim := "0000"]
uscie4fsu_delimdata[grepl("NA", HSU2_CD_SO_delim)]

uscie2fsu <- uscie4fsu_delimdata[, .(fsuID, USCIE_RC)]


# Check start and end per NUTS
fsuBynuts <- fsu_delimdata[, .(fsuID, CAPRINUTS2, FSUADM2_ID)]
fsuBynuts <- fsuBynuts[CAPRINUTS2!=""]
fsuBynuts <- fsuBynuts[, fsuNo := as.numeric(gsub("F", "", fsuID))]
fsuBynuts <- fsuBynuts[, .( min = min(fsuNo), max=max(fsuNo), n=.N), by=c("CAPRINUTS2","FSUADM2_ID")]
write.csv(fsuBynuts, "\\\\ies\\d5\\agrienv\\Data\\FSU/fsu_delimdata_fsurangesBydelimNuts.csv", row.names = FALSE, quote=FALSE, na="")


write.csv(fsu_delimdata, "\\\\ies\\d5\\agrienv\\Data\\FSU/fsu_delimdata.csv", row.names = FALSE, quote=FALSE, na="")
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
                  "\n#HSU2_CD_SO	HSU2?0?to?4?digits?code?for?the?soil?mapping?unit This is the soil mapping unit code to use for the FSU delineation",
                  "\n#INSP10_ID	ID of the 10km INSPIRE grid: This is the 10km grid unit to use for the FSU delineation",
                  "\n#Admin regions: ",
                  "\n#FSUADM2_ID	NUTS2 code: This is the administrative unit to use for the FSU delineation",
                  "\n#    --> see ies-ud01.jrc.it/D5_agrienv/Data/uscie/uscie_raster_FSU/readme_refras_FSU_land_soil_10km_admin_csv.txt",
                  "\n#    NOTE that for two NUTS2 regions other 'disagg-regions' have been defined as they are very slow in disaggregating.",
                  "\n#            UKM: using NUTS2 (NUTS2_2016) = 'real NUTS2' regions"
#                  "\n#            FI1A: using NUTS2 (FSS). ATTENTION: for FI1A "
                  ), wfile)
#Don't save all admin names here as it will becomes too large
close(wfile)

save(uscie2fsu, file="\\\\ies\\d5\\agrienv\\Data\\FSU/uscie2fsu.rdata")
write.csv(uscie2fsu, file="\\\\ies\\d5\\agrienv\\Data\\FSU/uscie2fsu.csv", quote=FALSE, row.names=FALSE)
wfile <- file("\\\\ies-ud01.jrc.it\\D5_agrienv\\Data\\FSU/uscie2fsu_readme.txt", open="w")
writeLines(paste0("#Matching table between uscie and Farm Structure SOil Units (FSU)",
                  "\n#Date generated: 19/08/2019 - Adrian Leip",
                  "\n#Script: FSU_delineation.r - Repository https://github.com/aleip/FarmstructureSoilUnits - https://github.com/aleip/FarmstructureSoilUnits/blob/master/FSU_delineation.r",
                  "\n#For uscie see: \\\\ies-ud01.jrc.it/D5_agrienv/Data/FSU/uscie4fsu_delimdata_readme.txt",
                  "\n#For FSU see \\\\ies-ud01.jrc.it/D5_agrienv/Data/FSU/fsu_delimdata_readme.txt"
), wfile)
#Don't save all admin names here as it will becomes too large
close(wfile)

save(uscie4fsu_delimdata, file="//ies-ud01.jrc.it/D5_agrienv/Data/FSU/uscie4fsu_delimdata.rdata")
writeuscie4fsu(uscie4fsu_delimdata)

### additional tests
doaddtests <- FALSE
if(doaddtests){
  
  f <- file("//ies-ud01.jrc.it/D5_agrienv/Data/FSU/Liechtenstein_FSUs.txt", open="w")
  writeLines("fsu_delimdata[grepl('F26708[0-9]', fsuID)]", f)
  write.csv(fsu_delimdata[grepl("F26708[0-9]", fsuID)], f)
  close(f)
  f <- file("//ies-ud01.jrc.it/D5_agrienv/Data/FSU/Liechtenstein_FSUs.txt", open="a")
  writeLines("uscie4fsu_delimdata[grepl('F26708[0-9]', fsuID)]", f)
  write.csv(uscie4fsu_delimdata[grepl("F26708[0-9]", fsuID)], f)
  close(f)
  
}



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
















