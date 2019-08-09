#### 
#### AGGREGATE from USCIE to FSU
#### 
#### -- Follow up of FSU_delineation_uscie.r
#### 
####    Here the data table for uscies is loaded and aggregated to FSUs
#### 
#### 

#FSU_delim_all <- fread("\\\\ies-ud01.jrc.it\\D5_agrienv\\Data\\FSU/USCIE_FSU_delin.csv", header = T) # one line per uscie
load("//ies-ud01.jrc.it/D5_agrienv/Data/FSU/uscie4fsu.rdata")
FSU_delim_all1 <- FSU_delim_all
names(FSU_delim_all1)

## Aggregate and calculate the FSU area as sum of USCIEs per FSU
FSU_delim_aggr <- FSU_delim_all1[, .N, by = .(FSU, nogo, forest, INSP10_ID, FSUADM2_ID_corrected.x, HSU2_CD_SO, 
                                              CNTR_CODE, CNTR_NAME.x, CAPRINUTS0, CAPRINUTS2)]
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

FSU_delim_aggr <- FSU_delim_aggr[, fsuort, with = FALSE]


write.csv(FSU_delim_aggr, "\\\\ies\\d5\\agrienv\\Data\\FSU/FSU_delin.csv", row.names = FALSE)
save(FSU_delim_aggr, file="\\\\ies\\d5\\agrienv\\Data\\FSU/FSU_delin.rdata")
#FSU_delim_aggr <- fread("\\\\ies\\d5\\agrienv\\Data\\FSU/FSU_delin.csv", header = T)

fsuID2FSU <- FSU_delim_aggr[, .(fsuID, FSU)]
uscie2FSU <- FSU_delim_all[, USCIE_RC, FSU]
uscie2fsu <- merge(uscie2FSU, fsuID2FSU, by="FSU")
uscie2fsu <- uscie2fsu[, fsuNo := as.numeric(gsub("F", "", fsuID))]
setkey(uscie2fsu, "fsuNo")
uscie2fsu <- uscie2fsu[, .(fsuID, USCIE_RC)]
save(uscie2fsu, file="\\\\ies\\d5\\agrienv\\Data\\FSU/uscie2fsu.rdata")


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
















