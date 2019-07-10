library(data.table)
library(reshape2)
library(gdxrrw)

if(Sys.info()[4] == "D01RI1701864"){ #checks machine name
  gamspath<-"C:\\apps\\GAMS\\win64\\24.9"
  #workpath<-"C:/adrian/tools/rprojects/gisdata4caprihsu/"
  #capridat<-"C:/adrian/models/capri/trunk20160810/dat/capdis/hsu2/"
}

igdx(gamspath)


# function to export ####
# modified from hsu4capr_functions.r

export2gdx<-function(x2gdx, 
                     parn=parn,   # Parameter name in the gdx file
                     pardesc=NULL,# Description of the parameter
                     ndim=ndim,   # Dimension. Note that it will increased by one
                     statistics=1,
                     vars=NULL,   # Vector with variable names. If NULL the column names will be used
                     # Use only for statistics.
                     mydim1exp=NULL, #Explanatory text for 1st dim. If null assumes all spatial units
                     myvars=NULL,    #Explanatory text for other dims
                     myvarsexp=NULL, #Explanatory text for vars. If NULL assumes statistical parameters (mean,min, max,median)
                     varname=NULL,
                     myText= NULL){
  nm <- tolower(parn)
  if(is.null(pardesc)) pardesc<-paste0("Data for ", nm) 
  
  print(paste0("Exporting to a gdx file...  ", nm, ".gdx"))
  x2gdxloc <- x2gdx[complete.cases(x2gdx)] # to remove NAs
  #x2gdxloc <- droplevels(x2gdxloc)
  x2gdxloc <- as.data.frame(x2gdxloc)
  
  if(! is.null(vars)&ndim>1){
    # Rename columns
    print(names(x2gdxloc))
    print(letters[10:(10+ndim-2)])
    print(vars)
    oldn<-names(x2gdxloc)[which(names(x2gdxloc)%in%letters[10:(10+ndim-2)])]
    setnames(x2gdxloc,letters[10:(10+ndim-2)],vars)
  }
  
  
  symDim<-ndim + 1
  attr(x2gdxloc,"symName") <- nm
  attr(x2gdxloc, "ts") <- pardesc   #explanatory text for the symName
  #attr(x2gdxloc, "names") <- c("merda1")   #
  #str(x2gdxloc)
  
  #print(myvars)
  #if(is.null(mydim1exp)) mydim1exp<-"Spatial units: CAPRI-NUTS0, CAPRI-NUTS2, Gisco-NUTS3, HSU"
  #if(is.null(myvars)) myvars<-paste0("variables",c(1:(ndim-1)))
  #if(is.null(myvarsexp)) myvarsexp<-paste0("Statistics calculated on the basis of uscie (HSU) or HSU (regions). ",
  #                                         "For HSU value refers to the direct value if available or average over uscie. ",
  #                                         "For regions, value is the area-weighted average.")
  #myText<-c(mydim1exp,
  #          myvars
  #          , myvarsexp)

  if(is.null(varname)) varname = "s_statistics"
  
  if(ndim == 1){
    wgdx.lst(paste0(nm, ".gdx"), x2gdxloc)
  }else{
    lst <- wgdx.reshape(x2gdxloc, symDim, tName = varname, setsToo=TRUE, order=c(1:ndim,0), setNames = myText)   #to reshape the DF before to write the gdx. tName is the index set name for the new index position created by reshaping
    wgdx.lst(paste0(nm, ".gdx"), lst)
  }
  #
} #end of export2gdx




# FSU dataset ####

FSU_delim_aggr <- fread("\\\\ies\\d5\\agrienv\\Data\\FSU/FSU_delin.csv", header = TRUE)




# p_fsu_srnuts2.gdx ####

FSU_delim_aggr1 <- FSU_delim_aggr
names(FSU_delim_aggr1)[1] <- "fsu_all"
#row.names(FSU_delim_aggr1) <- FSU_delim_aggr$runID
FSU_delim_aggr1 <- FSU_delim_aggr1[, c(1, 7, 11)]
cols <- names(FSU_delim_aggr1)[1:2]
FSU_delim_aggr1 <- FSU_delim_aggr1[, (cols) := lapply(.SD, as.factor), .SDcols = cols]
FSU_delim_aggr1 <- FSU_delim_aggr1[, nogo_flag := lapply(.SD, as.numeric), .SDcols = "nogo_flag"]
str(FSU_delim_aggr1)

export2gdx(x2gdx = FSU_delim_aggr1, 
           ndim = 2, 
           parn = "p_fsu_srnuts2", 
           #statistics=0,
           pardesc = "Mapping between FSU and CAPRINUTS2 (and nogo_flag)",
           mydim1exp = "FSU",
           myvars = "CAPRINUTS2",
           myvarsexp = "NoGo FSU: 0 = No go; 1 = Go; 2 = Forest",
           varname = c("nogo_flag")
           )




# s_fsu_srnuts2.gdx ####

FSU_delim_aggr1 <- FSU_delim_aggr
names(FSU_delim_aggr1)[1] <- "fsu_all"
#row.names(FSU_delim_aggr1) <- FSU_delim_aggr$runID
FSU_delim_aggr1 <- FSU_delim_aggr1[, c(1, 7)]
cols <- names(FSU_delim_aggr1)[1:2]
FSU_delim_aggr1 <- FSU_delim_aggr1[,(cols):= lapply(.SD, as.factor), .SDcols = cols]
str(FSU_delim_aggr1)

export2gdx(x2gdx = FSU_delim_aggr1, 
           ndim = 1, 
           parn = "s_fsu_srnuts2", 
           #statistics=0,
           pardesc = "Mapping between FSU and CAPRINUTS2",
           mydim1exp = "FSU",
           myvars = "CAPRINUTS2",
           myvarsexp = "NoGo FSU: 0 = No go; 1 = Go; 2 = Forest",
           varname = c("nogo_flag")
           )



# s_fsu_nogo.gdx ####

FSU_delim_aggr1 <- FSU_delim_aggr
names(FSU_delim_aggr1)[1] <- "fsu_all"
#row.names(FSU_delim_aggr1) <- FSU_delim_aggr$runID
FSU_delim_aggr1 <- FSU_delim_aggr1[, c(1, 11)]
cols <- names(FSU_delim_aggr1)[1:2]
FSU_delim_aggr1 <- FSU_delim_aggr1[,(cols):= lapply(.SD, as.factor), .SDcols = cols]
str(FSU_delim_aggr1)

export2gdx(x2gdx = FSU_delim_aggr1, 
           ndim = 1, 
           parn = "s_fsu_nogo", 
           #statistics=0,
           pardesc = "FSU and NoGo",
           mydim1exp = "FSU",
           myvars = "CAPRINUTS2",
           myvarsexp = "NoGo FSU: 0 = No Go; 1 = Go; 2 = Forest",
           varname = c("_flag")
           )






# p_fsu_grid10n23.gdx ####

FSU_delim_all <- fread("\\\\ies-ud01.jrc.it\\D5_agrienv\\Data\\FSU/USCIE_FSU_delin.csv", header = T) # one line per uscie
FSU_delim_aggr1 <- FSU_delim_aggr           # one line per FSU
names(FSU_delim_aggr1)[1] <- "fsu_all"



FSU_delim_all[, INSP10_FSS10_N2ID := paste(FSS10_N2ID, INSP10_ID, sep = "_")]

INSP10_gridarea <- as.data.table(FSU_delim_all %>% group_by(INSP10_ID) %>% summarise(gridarea = n()))

FSU_delim_all <- merge(FSU_delim_all, INSP10_gridarea, by = "INSP10_ID", all.x = TRUE)

#fracFSU1 <- as.data.table(FSU_delim_all %>% group_by(INSP10_ID, FSU) %>% summarise(fracarea = n()))
#nrow(fracFSU1)
#View(fracFSU1[duplicated(fracFSU1$INSP10_ID), ])
#View(fracFSU1[fracFSU1$INSP10_ID == "10kmE108N257", ])
#View(FSU_delim_all[FSU_delim_all$INSP10_ID == "10kmE108N257", ])
#View(fracFSU1[duplicated(fracFSU1$INSP10_ID), ])
#
#fracFSU1 <- merge(fracFSU1, FSU_delim_aggr1[, .SD, .SDcols = c("FSU", "FSU_area")], by = "FSU", all.x = TRUE)
#fracFSU1[, fracFSU := (fracarea / FSU_area)]
#unique(fracFSU1$fracFSU)
#fracFSU1

FSU_delim_all$fracFSU <- 1 


FSU_delim_all <- merge(FSU_delim_all, FSU_delim_aggr1[, .SD, .SDcols = c("FSU", "fsu_all", "FSU_area", "fsu_forest_share", "fsu_nogo_share", "nogo_flag")], by = "FSU", all.x = TRUE)
setnames(FSU_delim_all, c("fracFSUforest", "fracFSUnogo"), c("fracforestFSU", "fracnogoFSU"))


#load("\\\\ies-ud01.jrc.it\\D5_agrienv\\Data\\uscie\\hsu2_database_update_2016_02orig\\uscie_hsu2_nuts_marsgrid.rdata", verbose = TRUE)
#head(uscie_hsu)
#head(marsgrid_hsu)

uscie_grid25 <- fread("\\\\ies-ud01.jrc.it\\D5_agrienv\\Data\\uscie\\hsu2_database_update_2016_02orig\\USCIE_PARAM.csv", header = TRUE)


fsu_grid25 <- merge(FSU_delim_all[, .SD, .SDcols = c("FSU", "INSP10_ID", "USCIE_RC", "FSU_area")], uscie_grid25[, .SD, .SDcols = c("USCIE_RC", "GRIDNO")], by = "USCIE_RC", all.x = TRUE)
length(unique(fsu_grid25[is.na(fsu_grid25$GRIDNO), ]$FSU ))    # 379 FSUs has GRIDNo = NA (mostly the ones that have '00NA' in soil data codes for delineation)
                                                               # The USCIE_RC range of 'refras_FSU_land_soil_10km_admin.csv' and 'USCIE_PARAM.csv' are a bit different

frac_FSU_grid25 <- as.data.table(fsu_grid25 %>% group_by(GRIDNO, FSU) %>% summarise(fracarea = n()))


fsuArea <- fsu_grid25[, .SD, .SDcols = c("FSU", "FSU_area")]
fsuArea <- fsuArea[!duplicated(fsuArea), ]


frac_FSU_grid25 <- merge(frac_FSU_grid25, fsuArea, by = "FSU", all.x = TRUE)
frac_FSU_grid25[, fracFSUgrid25 := (fracarea / FSU_area)]


FSU_delim_all <- merge(FSU_delim_all, uscie_grid25[, .SD, .SDcols = c("USCIE_RC", "GRIDNO")], by = "USCIE_RC", all.x = TRUE)
FSU_delim_all <- merge(FSU_delim_all, frac_FSU_grid25[, .SD, .SDcols = c("FSU", "GRIDNO", "fracFSUgrid25")], by = c("FSU", "GRIDNO"), all.x = TRUE)
setnames(FSU_delim_all, c("GRIDNO"), c("grid25km"))


#
FSU_delim_all_2gdx <- FSU_delim_all[, .SD, .SDcols = c("fsu_all", "INSP10_FSS10_N2ID", "grid25km", "FSU_area", "fracFSU", "fracforestFSU", "fracnogoFSU", "fracFSUgrid25")]
names(FSU_delim_all_2gdx) <- tolower(names(FSU_delim_all_2gdx))
cols <- names(FSU_delim_all_2gdx)[1:3]
FSU_delim_all_2gdx <- FSU_delim_all_2gdx[,(cols):= lapply(.SD, as.factor), .SDcols = cols]
str(FSU_delim_all_2gdx)

export2gdx(x2gdx = FSU_delim_all_2gdx, 
           ndim = 3, 
           parn = "p_fsu_grid10n23", 
           #statistics=0,
           pardesc = "fsu - 10kmgrid/nuts2 - meteogrid25",
           varname = c("fsu_10kmgrid_marsgrid25"),
           #myText <- 1 text explanation per each variable
           myText = c("FSU", 
                      "10kmGrid and NUTS2", 
                      "MeteoGrid 25km",
                      "FSU_area; farcFSU: Fraction of FSU in 10kmgrid cell (always 1 because 10kmgrid is part of the delineation); fracforestFSU/fracnogoFSU: fraction of forest/NoGo in the FSU; fracFSUgrid25: fraction of FSU in the Meteogrid25")
           
)














