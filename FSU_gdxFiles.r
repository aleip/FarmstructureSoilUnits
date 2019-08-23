library(data.table)
library(reshape2)
library(gdxrrw)
library(magrittr)
#library(dplyr)
#
source("../FarmstructureSoilUnits/raster_functions.r")
source("capriextract_functions_4mapping.r")

if(Sys.info()[4] == "D01RI1701864"){ #checks machine name
  gamspath<-"C:\\apps\\GAMS\\win64\\24.9"
  #workpath<-"C:/adrian/tools/rprojects/gisdata4caprihsu/"
  #capridat<-"C:/adrian/models/capri/trunk20160810/dat/capdis/hsu2/"
}

#igdx(gamspath)

# function to export ####
# modified from hsu4capr_functions.r

export2gdx <- function(x2gdx, 
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
  
  lst <- wgdx.reshape(x2gdxloc, symDim, tName = varname, setsToo=TRUE, order=c(1:ndim,0), setNames = myText)   #to reshape the DF before to write the gdx. tName is the index set name for the new index position created by reshaping
  if(ndim == 1){
    wgdx.lst(paste0(nm, ".gdx"), x2gdxloc)
    #For 2-dim parameters doesn't seem to work
    #wgdx.lst(paste0(nm, ".gdx"), lst)
  }else{
    wgdx.lst(paste0(nm, ".gdx"), lst)
  }
  #
} #end of export2gdx


# FSU dataset ####

#FSU_delim_aggr <- fread("\\\\ies\\d5\\agrienv\\Data\\FSU/FSU_delin.csv", header = TRUE)
#load("//ies-ud01.jrc.it/D5_agrienv/Data/FSU/FSU_delin.rdata")

p_fsu_srnuts2.gdx <- function(){
  
  FSU_delim_aggr1 <- FSU_delim_aggr
  names(FSU_delim_aggr1)[1] <- "fsu_all"
  #row.names(FSU_delim_aggr1) <- FSU_delim_aggr$runID
  #FSU_delim_aggr1 <- FSU_delim_aggr1[, c(1, 7, 11)]
  FSU_delim_aggr1 <- FSU_delim_aggr1[, .(fsu_all, CAPRINUTS2, go)]
  cols <- names(FSU_delim_aggr1)[1:2]
  FSU_delim_aggr1 <- FSU_delim_aggr1[, (cols) := lapply(.SD, as.factor), .SDcols = cols]
  FSU_delim_aggr1 <- FSU_delim_aggr1[, go := lapply(.SD, as.numeric), .SDcols = "go"]
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
  
  # Export Set of nogo-hsu
  # Note: edit manually header: set fsunogo (fsu_all) /
  #                   last row: /;
  nogoFSU <- unique(FSU_delim_aggr1[go==0]$fsu_all)
  write.table(nogoFSU, file="fsunogo.gms", quote=FALSE, row.names=FALSE)
  
  # s_fsu_srnuts2.gdx ####
  
  FSU_delim_aggr1 <- FSU_delim_aggr
  names(FSU_delim_aggr1)[1] <- "fsu_all"
  #row.names(FSU_delim_aggr1) <- FSU_delim_aggr$runID
  FSU_delim_aggr1 <- FSU_delim_aggr1[, .(fsu_all, CAPRINUTS2)]
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
}

p_fsu_grid10n23 <- function(){
  # FSU_delim_all <- fread("\\\\ies-ud01.jrc.it\\D5_agrienv\\Data\\FSU/USCIE_FSU_delin.csv", header = T) # one line per uscie
  load("//ies-ud01.jrc.it/D5_agrienv/Data/FSU/FSU_delin.rdata")
  FSU_delim_aggr1 <- FSU_delim_aggr
  names(FSU_delim_aggr1)[1] <- "fsu_all"
  
  p_fsu_grid <- FSU_delim_aggr1[, .(fsu_all, INSP10_ID, CAPRINUTS2, FSU_area, CNTR_NAME)]
  p_fsu_grid <- p_fsu_grid[fsu_all != ""]
  p_fsu_grid <- p_fsu_grid[, FSUADM2 := paste0(substr(CAPRINUTS2,1,4), "_")]
  
  A <- p_fsu_grid[FSUADM2!="_"]
  B <- p_fsu_grid[FSUADM2=="_"]
  B <- B[, FSUADM2 := paste0(substr(CNTR_NAME, 1,4), "_")]
  
  p_fsu_grid <- rbind(A, B)
  
  
  p_fsu_grid <- p_fsu_grid[, grid10n2 := paste0(FSUADM2, INSP10_ID)]
  
  # Export Link to NUTS2
  m_grid10n2 <- unique(p_fsu_grid[, .(grid10n2, CAPRINUTS2)])
  m_grid10n2 <- m_grid10n2[CAPRINUTS2 != ""]
  m_grid10n2 <- m_grid10n2[, map := paste0(grid10n2, " . ", CAPRINUTS2)]
  
  con <- file("//ies-ud01.jrc.it/D5_agrienv/Data/FSU/m_grid10n2.gms", open="w")
  writeLines("set m_grid10n2(*,*) 'Mapping between FSS 10 km grid at NUTS2 level' / ", con)
  write.table(m_grid10n2$map, quote=FALSE, row.names=FALSE, col.names=FALSE, con)
  writeLines("/;", con)
  close(con)
  
  
  p_fsu_grid <- p_fsu_grid[, .(fsu_all, grid10n2, FSU_area)]
  p_fsu_grid <- p_fsu_grid[, gridarea := sum(FSU_area), by="grid10n2"]
  setnames(p_fsu_grid, "FSU_area", "area")
  save(p_fsu_grid, file="//ies-ud01.jrc.it/D5_agrienv/Data/FSU/p_fsu_grid.rdata")
  
  
  # The FSU are completely part of one inspire10k grid - NUTS2 intersection
  # Keep this only to remain consistent with the CAPRI code as for the HSU 
  # the fraction was not alwasy 1
  p_fsu_grid$fracFSU <- 1 
  cols <- names(p_fsu_grid)[1:2]
  p_fsu_grid <- p_fsu_grid[,(cols):= lapply(.SD, as.factor), .SDcols = cols]
  cols <- names(p_fsu_grid)[3:5]
  p_fsu_grid <- p_fsu_grid[,(cols):= lapply(.SD, as.numeric), .SDcols = cols]
  str(p_fsu_grid)
  
  export2gdx(x2gdx = p_fsu_grid, 
             ndim = 2, 
             parn = "p_fsu_grid10n2", 
             #statistics=0,
             pardesc = "Mapping between FSU and FSS-10km grid + admin regions at NUTS2 level",
             varname = c("fssgridpars"),
             myText = c("FSU", 
                        "10kmGrid and NUTS2", 
                        "area: Area [km2] of the FSU; gridarea: area [km2] of Inspire10kmgrid-Nuts2 intersection; fracFSU: Fraction of FSU in 10kmgrid cell (always 1 because 10kmgrid is part of the delineation)")
  )
  
}

m_hsugrid_fsugrid <- function(){
  #
  #  A. Prepare 10km grid cell that have FSS2010 data
  #
  
  iespath <- "//ies-ud01.jrc.it/D5_agrienv/Data/uscie/gisdata4caprihsu_outputdata/"
  fsupath <- "//ies-ud01.jrc.it/D5_agrienv/Data/FSU/"
  fs=paste0(iespath,"/fss2010grid_gapfilled~201711.rdata")
  if(file.exists(fs)){
    load(fs)
  }else{
    p <- paste0(cenv$capr, cenv$datdir, "capdishsu/fssdata/fss2010grid_")
    p <- paste0(cenv$capr, cenv$datdir, "capdishsu/fssdata/")
    fls <- list.files(p, pattern="fss2010grid_.*")
    fssgridall <- Reduce(rbind, lapply(1:length(fls), function(x) 
      as.data.table(rgdx.param(gdxName=paste0(p, fls[x]), symName="results_gridunits", names=c("grid", "fssact")))))
    fssdata <- dcast.data.table(fssgridall, grid ~ fssact, value.var="value", sum)
    
    save(fssgridall, file=fs)
    fs=paste0(iespath, "fss2010grid_N12_10km~201711.rdata")
  }
  fs=paste0(iespath, "fss2010grid_N12_10km~201711.rdata")
  if(file.exists(fs)){
    load(fs)
  }else{
    fssgrid <- fssgridall
    fssgrid <- unique(fssgrid[, .(grid)])
    fssgrid <- fssgrid[, grid:= gsub("NO01_2_", "NO012_", grid)]
    # Get nuts, scale and lon/lat (in m) for each grid cell
    fssgrid <- fssgrid[, c("nuts", "scale", "E") := .(tstrsplit(grid, "_")[[1]], 
                                                      tstrsplit(tstrsplit(grid, "_")[[2]], "E")[[1]], 
                                                      tstrsplit(tstrsplit(grid, "_")[[2]], "E")[[2]])]
    fssgrid <- fssgrid[, c("E", "N") := .(tstrsplit(E, "N")[[1]], tstrsplit(E, "N")[[2]])]
    # Keep only NUTS2 grid cells (NUTS3 have 5 characters)
    fssgrid <- fssgrid[nchar(nuts) < 5]
    # Keep only 10 km grids
    fssgrid <- fssgrid[scale == "10km"]
    fssgrid <- merge(fssgrid, fssgridall, by="grid")
    save(fssgrid, file=fs)
  }
  #
  #  B. Prepare FSU grid cells
  #
  load(file="//ies-ud01.jrc.it/D5_agrienv/Data/FSU/p_fsu_grid.rdata")
  matchnuts <- fread("//ies-ud01.jrc.it/D5_agrienv/Data/FSU/admin_units/CAPRI_NUTS_RG_01M_2016_3035_LEVL_3_plus_BA_XK_final_wfoa.csv")
  matchnuts2 <- unique(matchnuts[, .(CAPRINUTS2, FSS10_N2ID)])
  matchnuts2 <- matchnuts2[, nuts4:=substr(CAPRINUTS2, 1, 4)]
  matchnuts2 <- matchnuts2[!FSS10_N2ID %in% unique(matchnuts2[duplicated(matchnuts2$FSS10_N2ID)]$FSS10_N2ID)]
  # There are some duplicate FSSNUTS (e.g. DE1) - remove them so that a merge is possible
  ufssgrid <- unique(fssgrid[, .(grid, nuts, scale, E, N)])
  matchnuts3 <- merge(ufssgrid, matchnuts2, by.x="nuts", by.y="FSS10_N2ID")
  matchnuts3 <- matchnuts3[, gridnew := gsub(nuts, nuts4, grid), by=c("nuts", "nuts4", "grid")]
  fssgridmatch <- matchnuts3[, .(gridnew, CAPRINUTS2, nuts4, scale, E, N)]
  
  fsugrid <- unique(p_fsu_grid[, .(grid10n2)])
  fsugrid <- fsugrid[, nutsfsu := tstrsplit(grid10n2, "_")[[1]]]
  
  bothgrids <- merge(fssgridmatch, fsugrid, by.x="gridnew", by.y="grid10n2", all=TRUE)
  
  bothgrids <- bothgrids[, missInHSU := is.na(nuts4)]
  bothgrids <- bothgrids[, missInFSU := is.na(nutsfsu)]
  bothgrids <- bothgrids[, .(gridnew, nuts4, nutsfsu, missInHSU, missInFSU)]
  
  # Check missing in new
  sfol <- paste0(fsupath, "/solve_mismatch_gapfilled_fss/")
  missinfsu <- bothgrids[missInFSU==TRUE & !grepl("^_", gridnew)]
  fwrite(bothgrids[missInFSU==TRUE], file=paste0(fsupath, "FSS10kmgrids_matching_gapfilldata_missInFSU.csv"))
  correct1 <- fread(paste0(sfol, "gapfilled_NUTS2GRIDID_not_matching.csv"))
  correct1 <- correct1[, .(old=FSS10_N2ID_10kmGRIDID_not_in_spatial_layer,new=FSS10_N2ID_10kmGRIDID_alternative)]
  # After looking at correct2, make some manual adjustments
  correct1 <- correct1[old=="ITC3_10kmE413N229", old:="IT13_10kmE413N229"]
  correct1 <- correct1[old=="ITC4_10kmE423N242", old:="IT20_10kmE423N242"]
  correct1 <- correct1[old=="UKF2_10kmE362N331", old:="UKF0_10kmE362N331"]
  correct1 <- correct1[old=="UKJ1_10kmE355N327", old:="UKJ0_10kmE355N327"]
  correct1 <- correct1[old=="UKJ4_10kmE369N313", old:="UKJ0_10kmE369N313"]
  correct2 <- merge(correct1, missinfsu, by.x="old", by.y="gridnew", all=TRUE)
  bothgrids <- merge(bothgrids, correct2[, .(old, new)], by.x="gridnew", by.y="old", all.x=TRUE)
  
  # Now check mistake in other direction (missing in HSU data)
  bothgrids <- bothgrids[, missInHSU := is.na(nuts4)]
  bothgrids <- bothgrids[, missInFSU := is.na(nutsfsu)]
  bothgrids <- bothgrids[, .(gridnew, nuts4, nutsfsu, missInHSU, missInFSU)]
  missinhsu <- bothgrids[missInHSU==TRUE & !grepl("^_", gridnew)]
  missingcntr <- unique(missinhsu$nutsfsu)
  missinhsu <- missinhsu[! grepl("AL", gridnew)]
  missinhsu <- missinhsu[! grepl("BA", gridnew)]
  missinhsu <- missinhsu[! grepl("Icel", gridnew)]
  missinhsu <- missinhsu[! grepl("Liec", gridnew)]
  missinhsu <- missinhsu[! grepl("HR", gridnew)]
  missinhsu <- missinhsu[! grepl("MK", gridnew)]
  missinhsu <- missinhsu[! grepl("TR", gridnew)]
  
  fwrite(bothgrids[missInFSU==TRUE], file=paste0(fsupath, "FSS10kmgrids_matching_gapfilldata_missInHSU.csv"))
  correct1 <- fread(paste0(sfol, "FSS_NUTS2_10kmGRIDempty_match_with_closest_gapfilled_polygon.csv"))
  concernednuts <- unique(correct1[, FSS10_N2ID_EMPTY])
  correct1 <- correct1[, .(empty=FSS10_N2ID_10kmGRIDID_EMPTY, 
                           fillby=NEAR_FSS10_N2ID_10kmGRIDID, 
                           clcempty=CLC2018_agrigrass_ha_for_FSS10_N2ID_10kmGRIDID_EMPTY,
                           clcfill=CLC2018_agrigrass_ha_for_NEAR_FSS10_N2ID_10kmGRIDID)]
  
  #
  #  C. Join data and export
  #
  
  fss10km <- merge(bothgrids[, .(grid=gridnew)], fssgrid, by = "grid", all.y=TRUE)
  
  # For each empty cell 
  # - if the empty cellsthey were large, then it is assumed that it is OK to be empty
  # - if they have no or very little Corine agricultural area, it is assumed it is OK to be empty
  # - otherwise the surrounding grids have been checked
  #   - the closest four cells are found and the nearest identified
  #   - the corine agriculure areas are calculated
  # Here the new data are calculated distributing activities acc to CLC agricultural area
  fsscorrect <- merge(fss10km, correct1, by.x="grid", by.y="fillby")
  fsscorrect <- fsscorrect[, updateval := value * clcfill/(clcfill + clcempty)]
  fsscorrect <- fsscorrect[, fillvalue := value * clcempty/(clcfill + clcempty)]
  fssempty <- fsscorrect[, .(grid=empty, fssact, valuenew=fillvalue)]
  fssupdate <- fsscorrect[, .(grid=grid, fssact, valuenew=updateval)]
  fssupdate <- rbind(fssupdate, fssempty)
  setkey(fssupdate, grid, fssact)
  fssupdate
  fss10kmfilled <- merge(fss10km, fssupdate, by=c("grid", "fssact"), all=TRUE)
  fss10kmfilled <- fss10kmfilled[is.na(valuenew), valuenew:=value]
  fss10kmfilled <- fss10kmfilled[!is.na(valuenew)]
  
  fss10km <- dcast.data.table(fss10kmfilled, grid ~ fssact, value.var="valuenew", sum)
  cols <- names(fss10km)[1]
  fss10km <- fss10km[,(cols):= lapply(.SD, as.factor), .SDcols = cols]
  cols <- names(fss10km)[2:length(fss10km)]
  fss10km <- fss10km[,(cols):= lapply(.SD, as.numeric), .SDcols = cols]
  str(fss10km)
  
  x2gdxloc <- fss10km[complete.cases(fss10km)] # to remove NAs
  x2gdxloc <- as.data.frame(x2gdxloc)
  attr(x2gdxloc,"symName") <- "p_fss2010" #Parameter name
  attr(x2gdxloc, "ts") <- "FSS2010 data gap-filled at NUTS2-10km grid level"   #explanatory text for the symName
  symDim <- 2
  lst <- wgdx.reshape(x2gdxloc, 
                      symDim, 
                      tName = "fsscode", 
                      setsToo=TRUE, 
                      order=c(1:(symDim-1),0), 
                      setNames = c("grid", "Fss Codes"))   #to reshape the DF before to write the gdx. tName is the index set name for the new index position created by reshaping
  wgdx.lst(paste0("p_fss2010", ".gdx"), lst)
}

p_fsu_area.gdx <- function(){
  
  load("//ies-ud01.jrc.it/D5_agrienv/Data/FSU/FSU_delin.rdata")
  fsuArea <- FSU_delim_aggr[, .(fsuID, FSU_area)]
  nuts2Area <- FSU_delim_aggr[, sum(FSU_area), by=c("CAPRINUTS2")]
  cntArea <- FSU_delim_aggr[, sum(FSU_area), by=c("CAPRINUTS0")]
  insp10area <- FSU_delim_aggr[, sum(FSU_area), by=c("INSP10_ID")]
  smuarea <- FSU_delim_aggr[, sum(FSU_area), by=c("HSU2_CD_SO")]
  
  fsugridarea <- FSU_delim_aggr[, .(fsuID, FSU_area, CAPRINUTS2, INSP10_ID, CNTR_NAME)]
  fsugridarea <- fsugridarea[, FSUADM2 := paste0(CAPRINUTS2, "_")]
  A <- fsugridarea[FSUADM2!="_"]
  B <- fsugridarea[FSUADM2=="_"]
  B <- B[, FSUADM2 := paste0(substr(CNTR_NAME, 1,8), "_")]
  
  fsugridarea <- rbind(A, B)
  fsugridarea <- fsugridarea[, grid10n2 := paste0(FSUADM2, INSP10_ID)]
  fsugridarea <- fsugridarea[, sum(FSU_area), by="grid10n2"]
  
  # For NUTS3 need to reload data for uscie's
  load("//ies-ud01.jrc.it/D5_agrienv/Data/FSU/uscie4fsu.rdata")
  nuts3area <- FSU_delim_all[, .N, by="NUTS3_2016"]
  
  hnames <- c("unit", "area")
  names(fsuArea) <- hnames
  names(nuts2Area) <- hnames
  names(cntArea) <- hnames
  names(insp10area) <- hnames
  names(fsugridarea) <- hnames
  names(nuts3area) <- hnames
  
  p_fsu_area <- rbind(fsuArea, nuts2Area, cntArea, insp10area, fsugridarea, nuts3area)
  # Some NUTS0 are also NUTS2, some Inspire grids are also Inspire+admin
  p_fsu_area <- unique(p_fsu_area)
  
  CheckDuplicates <- function(){
    length(p_fsu_area$unit)
    length(unique(p_fsu_area$unit))
    p_fsu_area$unit[duplicated(p_fsu_area$unit)]
    p_fsu_area[unit %in% p_fsu_area$unit[duplicated(p_fsu_area$unit)]]
    
    # Check 5 remaining on full data table - there are all inspire grids
    # There are all at the border between Austria, CH, and Liechtenstein (FSUADM2_ID_corrected 108, 187 and 435).
    # Once with total area of 100, once with less.
    for(i in 1:length(p_fsu_area$unit[duplicated(p_fsu_area$unit)])){
      print(FSU_delim_aggr[INSP10_ID %in% p_fsu_area$unit[duplicated(p_fsu_area$unit)][i]][, 1:10])
    }
    insp10area[unit %in% p_fsu_area$unit[duplicated(p_fsu_area$unit)]] #OK
    fsugridarea[unit %in% p_fsu_area$unit[duplicated(p_fsu_area$unit)]] #not OK
    # Corrected in above formula in using the country-name instead
  }
  
  
  #
  FSU_delim_all_2gdx <- p_fsu_area
  names(FSU_delim_all_2gdx) <- tolower(names(FSU_delim_all_2gdx))
  cols <- names(FSU_delim_all_2gdx)[1]
  FSU_delim_all_2gdx <- FSU_delim_all_2gdx[,(cols):= lapply(.SD, as.factor), .SDcols = cols]
  str(FSU_delim_all_2gdx)
  
  export2gdx(x2gdx = FSU_delim_all_2gdx, 
             ndim = 1, 
             parn = "p_area", 
             #statistics=0,
             mydim1exp = "FSU",
             varname = c("s_area"),
             pardesc = "Spatial units area (FSU, CAPRINUTS0, CAPRINUTS2, NUTS3(2016), Inspire10km grid cells, and FSS-grids)",
             myText = c("Spatial units", "area: Area [km2]")
             #varname = c("fsu_10kmgrid_marsgrid25"),
             #myText <- 1 text explanation per each variable
             #myText = c("FSU")
  )
  save(p_fsu_area, file = "//ies-ud01.jrc.it/D5_agrienv/Data/FSU/p_fsu_areas.rdata")
}


p_corine <- function(){
  # Add LandCover Shares to a new gdx file - keep this one for the mapping FSU and grid10k only
  load("//ies-ud01.jrc.it/D5_agrienv/Data/FSU/uscie2fsu.rdata")
  
  corinedir <- "//ies-ud01.jrc.it/D5_agrienv/Data/FSU/corine/"
  corine_cats <- read.csv(paste0(corine_dir, "/CLC2018_CLC2018_V2018_20.txt"), header = FALSE)
  rcl_mat <- corine_cats[, 1:2]
  rcl_mat[, 2] <- 0
  rcl_mat4nogo <- rcl_mat
  rcl_mat4nogo[c(1:11, 30, 31, 34, 38, 39, 40:44), 2] <- 1
  corineNonogo <- rcl_mat4nogo[rcl_mat4nogo$V2 == 0, "V1"]
  
  # Write out set of Corine classes
  corinenogo <- as.data.table(rcl_mat4nogo)
  corinenogo <- corinenogo[V2==1, class := "NOGO"]
  corinenogo <- corinenogo[is.na(class), class := as.character(V1)]
  clc <- as.data.table(merge(corine_cats, corinenogo, by="V1"))
  clc1 <- clc[V2.y == 1]
  clc <- clc[V2.y == 0]
  clc <- clc[, set := paste0(class, " '", V6, "'")]
  clc1 <- clc1[, set := paste0(V1, " '", V6, "'")]
  clc <- clc$set
  clc1 <- clc1$set
  clc <- c("NOGOs 'Nogo areas including uban, bare rock, beaches and dunes, salines, intertidal flats, and water'",
           "31forests 'Forest areas including 311 Broad-leaved forest, 312 Coniferous forest, 313 Mixed forests'",
           clc)
  fwr <- file("//ies-ud01.jrc.it/D5_agrienv/Data/FSU/corine2018classes.gms", open="w")
  writeLines("set CorineLandCoverClass 'Corine Land Cover Classes with aggregates NOGO and forests' /", fwr)
  write.csv(clc, row.names=FALSE, quote=FALSE, fwr)
  writeLines("/;\n\nset nogoClasses /", fwr)
  write.csv(clc1, row.names=FALSE, quote=FALSE, fwr)
  writeLines("/;", fwr)
  close(fwr)
  
  
  
  corine <- c("NOGOs", "31forests", corineNonogo[1:(length(corineNonogo)-1)])
  fls <- list.files(corinedir, "*rdata", full.names=TRUE)
  
  i <- 2
  
  for (i in 1:length(corine)){
    cldesc <- as.character(unlist(corine_cats[corine_cats$V1==corine[i], 6]))
    load(paste0(corinedir, "corineCLASS", corine[i], "_share100m_uscie.rdata"))
    c <- c("uscie", "CLC", "value")
    names(clcshare) <- c
    #clcshare[uscie=="24754286"]
    
    if(i==1){
      y <- clcshare
    }else{
      y <- rbind(y, clcshare)
    }
  }
  p_corineShares <- dcast.data.table(y, uscie ~ CLC, value.var="value")
  ccols <- setdiff(names(p_corineShares), "uscie")
  p_fsuCorineShar <- merge(p_corineShares, uscie2fsu, by.y="USCIE_RC", by.x="uscie", all=TRUE)
  p_fsuCorineArea <- p_fsuCorineShar[, lapply(.SD, sum), by=fsuID, .SDcols = ccols]
  p_fsuCorineArea <- p_fsuCorineArea[, (ccols) := lapply(.SD, function(x) x/100), .SDcols = ccols]
  p_fsuCorineArea <- p_fsuCorineArea[, fsuNo := as.numeric(gsub("F", "", fsuID))]
  setkey(p_fsuCorineArea, "fsuNo")
  p_fsuCorineArea <- p_fsuCorineArea[, 1:(length(p_fsuCorineArea)-1)]
  
  
  p_fsuCorineShares <- p_fsuCorineShar[, lapply(.SD, mean), by=fsuID, .SDcols = setdiff(names(p_fsuCorineShar), c("uscie", "fsuID"))]
  p_fsuCorineShares <- p_fsuCorineShares[, fsuNo := as.numeric(gsub("F", "", fsuID))]
  setkey(p_fsuCorineShares, "fsuNo")
  p_fsuCorineShares <- p_fsuCorineShares[, 1:(length(p_fsuCorineShares)-1)]
  names(p_fsuCorineShares)[1] <- "fsu_all"
  save(p_fsuCorineShares, p_fsuCorineArea, file="//ies-ud01.jrc.it/D5_agrienv/Data/FSU/p_fsuCorine.rdata")
  str(p_fsuCorineShares)
  str(p_fsuCorineArea)
  
  x2gdxloc <- p_fsuCorineShares[complete.cases(p_fsuCorineShares)] # to remove NAs
  x2gdxloc <- as.data.frame(x2gdxloc)
  attr(x2gdxloc,"symName") <- "p_fsuCorineShares" #Parameter name
  attr(x2gdxloc, "ts") <- "Share of Corine (clc2018_v20_incl_turkey) land cover classes incl all NOGOs and forest classes. Calculation based on Corine100 aggregated to uscie"   #explanatory text for the symName
  symDim <- 2
  lst <- wgdx.reshape(x2gdxloc, 
                      symDim, 
                      tName = "CorineLandCoverClass", 
                      setsToo=TRUE, 
                      order=c(1:(symDim-1),0), 
                      setNames = c("fsu_all", "CLC"))   #to reshape the DF before to write the gdx. tName is the index set name for the new index position created by reshaping
  wgdx.lst(paste0("p_fsuCorineShares", ".gdx"), lst)
  
  x2gdxloc <- p_fsuCorineArea[complete.cases(p_fsuCorineArea)] # to remove NAs
  x2gdxloc <- as.data.frame(x2gdxloc)
  attr(x2gdxloc,"symName") <- "p_fsuCorineArea" #Parameter name
  attr(x2gdxloc, "ts") <- "Area of Corine (clc2018_v20_incl_turkey) land cover classes incl all NOGOs and forest classes. Calculation based on Corine100 aggregated to uscie"   #explanatory text for the symName
  symDim <- 2
  lst <- wgdx.reshape(x2gdxloc, 
                      symDim, 
                      tName = "CorineLandCoverClass", 
                      setsToo=TRUE, 
                      order=c(1:(symDim-1),0), 
                      setNames = c("fsu_all", "CLC"))   #to reshape the DF before to write the gdx. tName is the index set name for the new index position created by reshaping
  wgdx.lst(paste0("p_fsuCorineArea", ".gdx"), lst)
  
  
  checkWhyNoforestinAustria <- function(){
    
    i <- 2
    cldesc <- as.character(unlist(corine_cats[corine_cats$V1==corine[i], 6]))
    load(paste0(corinedir, "corineCLASS", corine[i], "_share100m_uscie.rdata"))
    c <- c("uscie", corine[i])
    names(corinedt) <- c
    x <- melt.data.table(corinedt, id.vars=c[1], measure.vars=c[2], variable.name="CLC", value.name="share100m")
    y <- merge(x[, .(uscie, share100m)], uscie2fsu, by.y="USCIE_RC", by.x="uscie", all=TRUE)
    
    z <- y[, lapply(.SD, sum), by=fsuID, .SDcols = "share100m"]
    z <- z[, fsuNo := as.numeric(gsub("F", "", fsuID))]
    y <- y[, fsuNo := as.numeric(gsub("F", "", fsuID))]
    setkey(z, "fsuNo")
    zat11 <- z[fsuNo<306]
    yat11 <- y[fsuNo<306]
    yatuscies <- unique(yat11$uscie)
    test <- convertRaster2datatable(forr, uscie1km)
    test <- test[refras_FSU_land %in% yatuscies]
    test <- merge(test, uscie2fsu, by.x="refras_FSU_land", by.y="USCIE_RC")
    
    
    #Check individual uscie 24754286
    forr <- raster(paste0(dir2save, "corineCLASS31forests_share100m_uscie.tif"))
    fordt <- convertRaster2datatable(rast1=forr, rast2=uscie1km)
    x[uscie=="24754286"]
  }
  
  
}

p_pesetagrid <- function(){
  
  # From repository 'gisdata4caprihsu'
  # -> file hsu4capri_data.r
  # 
  usciedatapath<-"\\\\ies-ud01.jrc.it\\D5_agrienv\\Data\\uscie\\"
  usciedatapath<-paste0(usciedatapath,"hsu2_database_update_2016_02orig/")
  dopesetagrid<-1
  if(dopesetagrid==1){
    #xresult<-processdata(
    xfulln = paste0(usciedatapath,"uscie_peseta_grid.csv")
    xresult<-processdata(xfulln = xfulln,xvbles = "PESETAidgrid",parn="PESETAgrid_fraction")}
  
  # Do here
  peseta <- fread(xfulln)
  load("//ies-ud01.jrc.it/D5_agrienv/Data/FSU/uscie2fsu.rdata")
  fsupeseta <- merge(uscie2fsu, peseta, by="USCIE_RC")
  fsupeseta <- fsupeseta[, .N, by=c("fsuID", "PESETAidgrid")]
  fsupeseta <- fsupeseta[, nPeseta := sum(N), by=c("PESETAidgrid")]
  fsupeseta <- fsupeseta[, nFSU := sum(N), by=c("fsuID")]
  fsupeseta <- fsupeseta[, fractionfsu := N/nFSU]
  fsupeseta <- fsupeseta[, o := as.numeric(gsub("F", "", fsuID))]
  setkey(fsupeseta, o)
  p_pesetagrid_fractionfsu <- fsupeseta[, .(fsuID, PESETAidgrid, fractionfsu)]
  p_pesetagrid_fractionfsu <- fsupeseta[, .(fsu_all = fsuID, PESETAidgrid, fraction = fractionfsu)]
  cols <- names(p_pesetagrid_fractionfsu)[1:2]
  p_pesetagrid_fractionfsu <- p_pesetagrid_fractionfsu[,(cols):= lapply(.SD, as.factor), .SDcols = cols]
  str(p_pesetagrid_fractionfsu)
  x2gdxloc <- p_pesetagrid_fractionfsu[complete.cases(p_pesetagrid_fractionfsu)] # to remove NAs
  x2gdxloc <- as.data.frame(x2gdxloc)
  attr(x2gdxloc,"symName") <- "pesetagrid_fractionfsu" #Parameter name
  attr(x2gdxloc, "ts") <- "Fraction of FSU in PESETA grid cell. Sum over gridcells gives 1 per FSU"   #explanatory text for the symName
  symDim <- 3
  lst <- wgdx.reshape(x2gdxloc, 
                      symDim, 
                      tName = "fraction", 
                      setsToo=TRUE, 
                      order=c(1:(symDim-1),0), 
                      setNames = c("fsu_all", "PESETAidgrid"))   #to reshape the DF before to write the gdx. tName is the index set name for the new index position created by reshaping
  wgdx.lst(paste0("pesetagrid_fractionfsu", ".gdx"), lst)
  
  
  ## Irrigated Areas
  irridatapath<-"\\\\ies-ud01.jrc.it\\D5_agrienv\\Data\\irrigation\\siebert+2006"
  irri <- fread(paste0(irridatapath, "/USCIE_IRRIGATED_AREA_2000.csv"))
  fsuirri <- merge(uscie2fsu, irri, by="USCIE_RC")
  irrishar <- fsuirri[, .(share=mean(IRRIG_AREA_2000_PERC)), by = "fsuID"]
  #2check: irrishar[fsuID=="F171335"]
  #irrishar <- irrishar[, different := IRRIG_AREA_2000_PERC!=share]
  irrishar <- irrishar[, o := as.numeric(gsub("F", "", fsuID))]
  setkey(irrishar, o)
  irrishar <- irrishar[, .(fsu_all = fsuID, irri_share2000=share)]
  cols <- names(irrishar)[1]
  irrishar <- irrishar[,(cols):= lapply(.SD, as.factor), .SDcols = cols]
  str(irrishar)
  x2gdxloc <- irrishar[complete.cases(irrishar)] # to remove NAs
  x2gdxloc <- as.data.frame(x2gdxloc)
  attr(x2gdxloc,"symName") <- "irr_share2000" #Parameter name
  attr(x2gdxloc, "ts") <- "Fraction of FSU in PESETA grid cell. Sum over gridcells gives 1 per FSU"   #explanatory text for the symName
  symDim <- 2
  lst <- wgdx.reshape(x2gdxloc, symDim, tName = "irr_share")
  wgdx.lst(paste0("irr_share2000", ".gdx"), lst)
  
}
emepdeposition <- function(){
  # For gridcells, see 
  # \\ies-ud01.jrc.it\D5_agrienv\Data\uscie\hsu2_database_update_2016_02orig\USCIE_EMEP_HSU2.r
  # Generating (a) Fraction of FSU in emep grid (and vice versa)
  #            (b) Merging deposition data to FSU
  # \\ies-ud01.jrc.it\D5_agrienv\Data\uscie\hsu2_database_update_2016_02orig\USCIE_EMEP_FSU_LC.rdata
  # \\ies-ud01.jrc.it\D5_agrienv\Data\uscie\hsu2_database_update_2016_02orig\USCIE_EMEP_LC_FSU_W-DDP.rdata
  
  # Further processing
  # \\ies-ud01.jrc.it\D5_agrienv\Data\uscie\gisdata4caprihsu_outputdata\p_deposition.r
  
}


meteogrid0.25 <- function(){
  
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
  
  forr <- raster(paste0(dir2save, "corineCLASS31forests_share100m_uscie.tif"))
  
}








