plotsmallwindow <- function(x, wint="", plot2png=FALSE, param = "", country=NULL, lvl = 2){
  
  etnt <- extent(x)
  
  xy <- data.table(matrix(c("ispra", "Area around Ispra", "4200000", "4220000", "2520000", "2540000"), ncol = 6))
  xy <- rbind(xy, data.table(matrix(c("lago", "Area around Lago Maggiore", "4150000", "4300000", "2500000", "2660000"), ncol = 6)))
  xy <- rbind(xy, data.table(matrix(c("northitaly", "Alps - North Italy", "4000000", "4500000", "2500000", "3000000"), ncol = 6)))
  xy <- rbind(xy, data.table(matrix(c("small", "North Italy", "4150000", "4400000", "2450000", "2600000"), ncol = 6)))
  xy <- rbind(xy, data.table(matrix(c("eu", "European Union countries", "2550000", "6000000", "1300000", "5300000"), ncol = 6)))
  
  if(!exists("nuts23")) {
    n23_dir <- "\\\\ies\\d5\\agrienv\\Data\\GIS_basedata\\GISCO_2010_NUTS2_3"
    nuts23 <<- readOGR(dsn = n23_dir, layer = "GISCO_NUTS2_3_2010_with_attr_laea")
  }
  if(wint == "country"){
    
    if(is.null(country)){
      message("Please choose a country with argrument 'country'")
      return()
    }else{
      if(! exists("nuts23")){
        message("The country you indicated does not exist in the database. Please check spelling!")
        return()
      }
      curnuts3 <- nuts3[grepl(country, nuts3@data$NUTS3_2010), ]
    }
    
    xdiff <- extent(curnuts3)@xmax - extent(curnuts3)@xmin
    ydiff <- extent(curnuts3)@ymax - extent(curnuts3)@ymin
    exdiff <- xdiff - ydiff
    #Add to the smaller edge so that it becomes square
    nxmax <- extent(curnuts3)@xmax - min(0, exdiff/2)
    nxmin <- extent(curnuts3)@xmin + min(0, exdiff/2)
    nymax <- extent(curnuts3)@ymax + max(0, exdiff/2)
    nymin <- extent(curnuts3)@ymin - max(0, exdiff/2)
    xy <- rbind(xy, data.table(matrix(c("country", country, c(nxmin, nxmax, nymin, nymax)), ncol=6)))
    
  }
  
  names(xy) <- c("name", "desc", "xmin", "xmax", "ymin", "ymax")
  cols <- names(xy)[3:6]
  xy <- xy[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]
  
  if(!wint %in% xy$name){
    message("Please choose an area to plot. The following are available:")
    print(xy)
    return()
  }
  #View(xy)
  xname <- substitute(x)
  if(xname =="curr") xname <- ""
  if(wint=="country") {
    xname <- paste0(xname, country)
  }else{
    xname <- paste0(xname, wint)
  }
  if(names(x)!="uscie") xname <- names(x)
  
  
  etnt@xmin <- xy[name==wint, xmin]
  etnt@xmax <- xy[name==wint, xmax]
  etnt@ymin <- xy[name==wint, ymin]
  etnt@ymax <- xy[name==wint, ymax]
  #print(etnt)
  
  fx <- crop(x, etnt)
  curnuts2 <- crop(nuts2, etnt)
  curnuts0 <- crop(nuts0, etnt)
  
  save(list=objects(), file="P.rdata")
  
  
  if(plot2png) png(paste0(xname, "_", gsub(" ", "", param), ".png"))
  plot(fx, main = paste0(xy[name==wint, desc], "\n", param))
  if(wint == "country"){
  }
  
  # Add background color
  plot(curnuts0, add=TRUE, col="light grey")
  plot(curnuts0[is.na(curnuts0@data$EU28_CNTR),], add=TRUE, col="grey")
  plot(fx, add=TRUE, main = paste0(xy[name==wint, desc], "\n", param))
  curlwd <- 0.25
  
  #Omit curnuts3 borders for large scale
  if(extent(fx)@xmax-extent(fx)@xmin < 2000000){
    #plot(curnuts3, add=TRUE, lwd=curlwd, lty=2)
    plot(curnuts2, add=TRUE, lwd=4*curlwd)
    plot(curnuts0, add=TRUE, lwd=8*curlwd)
  }
  if(plot2png) dev.off()
  
}


convertRaster2datatable <- function(rast1, rast2=uscie){
  
  # Normally, rast1 is the raster with the values
  #           rast2 is the raster with the spatial unites (e.g. uscie)
  #           
  #           First, crop the spatial units to the extent of the data
  #           Then, fill it with the information
  n1 <- rast1@data@names
  n2 <- rast2@data@names
  
  r12 <- crop(rast2, rast1)
  rfin <- crop(rast1, r12)
  dt <- data.table(values(r12), values(rfin))
  dt <- dt[! is.na(V1)]
  dt <- dt[! is.na(V2)]
  
  names(dt) <- c(n2, n1)
  return(dt)
}


calculateCorineShare <- function(currast = corine100m_crop, cl, corine_cats, dir2save=NULL){
  
  if(is.null(dir2save)) {stop("No save directory is given!")}
  
  rcl_mat <- corine_cats[, 1:2]
  currclmat <- rcl_mat
  currclmat[, 2] <- 0
  currclmat[which(currclmat$V1==cl), 2] <- 1
  cldesc <- corine_cats[corine_cats$V1==cl, 6]
  
  cat("\nReclassify for Corine class ", cl)
  corineCLASS100m <- reclassify(currast, currclmat)
  cat("\nAggregate to uscie 1 km pixel for Corine class ", cl)
  corineCLASS1km <- aggregate(corineCLASS100m, fact = 10, fun = sum)
  cat("\nWrite out raster for Corine class ", cl)
  writeRaster(corineCLASS1km, 
              filename = paste0(dir2save, "/corineCLASS", cl, "_shares100m_uscie", ".tif"), 
              format = "GTiff", 
              overwrite = TRUE)
  
  cat("\nPlot examples for checking for Corine class ", cl)
  uscie_dir <- "\\\\ies\\d5\\agrienv\\Data\\uscie\\uscie_raster_FSU"
  uscie1km <- paste0(uscie_dir, "/refras_FSU_land.tif")
  uscie1km <- raster(uscie1km)
  uscie1km@data@names <- "uscie"
  
  clinpng <- paste0(cl, "_", cldesc)
  for(doplot in c("ispra", "lago", "eu")){
    
    plotsmallwindow(corineCLASS1km, wint = doplot, plot2png = TRUE, param = clinpng)
    file.rename(paste0("corineCLASS1km", doplot, ".png"), 
                paste0(dir2save, "/corineCLASS", clinpng, "_1km", doplot, ".png"))
    
    
  }
  
  cat("\nConvert to data table for later aggregation to FSU for Corine class ", cl)
  corinedt <- convertRaster2datatable(rast1 = corineCLASS1km, rast2 = uscie1km)
  names(corinedt)[2] <- paste0("clc", cl)
  save(corinedt, file=paste0(dir2save, "/corineCLASS", cl, "_share100m_uscie", ".rdata"))
  
  cat("\nClear memory to save disk space")
  removeTmpFiles(h=2)
  
}


doCorineSharecalc <- function(){
  
  library(data.table)
  library(raster)
  library(rgdal)
  
  
  ### Reading in data: USCIE, CORINE_cropped ####
  uscie_dir <- "\\\\ies\\d5\\agrienv\\Data\\uscie\\uscie_raster_FSU"
  corine_dir <- "\\\\ies-ud01.jrc.it\\D5_agrienv\\Data\\Corine_Land_Cover\\clc2018_v20_incl_turkey\\7ac95361f9ac3cecdf37785bc183ff02dd765a16\\clc2018_clc2018_v2018_20_raster100m/"
  
  #CORINE raster
  cp <- paste0(uscie_dir, "/CLC2018_CLC2018_V2018_20_cropped2uscie.tif")
  cp <- raster(cp)
  cp
  unique(cp@data@attributes)
  
  corine_cats <- read.csv(paste0(corine_dir, "/CLC2018_CLC2018_V2018_20.txt"), header = FALSE)
  corine_cats
  
  #USCIE 
  uscie1km <- paste0(uscie_dir, "/refras_FSU_land.tif")
  uscie1km <- raster(uscie1km)
  uscie1km@data@names <- "uscie"
  uscie1km
  
  
  #### Calculating shares of individual Corine Classes - except of NOGO classes
  rcl_mat <- corine_cats[, 1:2]
  rcl_mat[, 2] <- 0
  rcl_mat4nogo <- rcl_mat
  rcl_mat4nogo[c(1:11, 30, 31, 34, 38, 39, 40:44), 2] <- 1
  corineNonogo <- rcl_mat4nogo[rcl_mat4nogo$V2 == 0, "V1"]
  dir2save <- "C:/Users/leipadr/Documents"
  dir2save <- "\\\\tsclient/x/adrian/data/fsu"
  for(cl in corineNonogo[12:(length(corineNonogo)-1)]){
    
    cldesc <- as.character(unlist(corine_cats[corine_cats$V1==cl, 6]))
    cat("\n\nWorking on Corine Class ", cl, "(", cldesc, ")")
    
    calculateCorineShare(currast = cp, cl = cl, corine_cats = corine_cats, dir2save = dir2save)  
  }
  
  
}


replot_tifs <- function(){
  
  
  dir2save <- "x:/adrian/data/fsu"
  dir2save <- "\\\\ies-ud01.jrc.it\\D5_agrienv\\Data\\FSU/corine/"
  corine_dir <- "\\\\ies-ud01.jrc.it\\D5_agrienv\\Data\\Corine_Land_Cover\\clc2018_v20_incl_turkey\\7ac95361f9ac3cecdf37785bc183ff02dd765a16\\clc2018_clc2018_v2018_20_raster100m/"
  corine_cats <- read.csv(paste0(corine_dir, "/CLC2018_CLC2018_V2018_20.txt"), header = FALSE)
  rcl_mat <- corine_cats[, 1:2]
  rcl_mat[, 2] <- 0
  rcl_mat4nogo <- rcl_mat
  rcl_mat4nogo[c(1:11, 30, 31, 34, 38, 39, 40:44), 2] <- 1
  corineNonogo <- rcl_mat4nogo[rcl_mat4nogo$V2 == 0, "V1"]
  corineNonogo <- c("31forests", corineNonogo)
  
  doplots <- c("ispra", "lago", "northitaly")
  doplots <- c("eu")
  doplots <- c("AT")
  #for (i in 1:1){
  for (doplot in doplots){
    for (i in 1:(length(corineNonogo)-1)){
      
      fls <- list.files(dir2save, paste0(corineNonogo[i], ".*tif"), full.names=TRUE)
      cldesc <- as.character(unlist(corine_cats[corine_cats$V1==corineNonogo[i], 6]))
      cp <- raster(fls)
      outfile <- paste0("/corineCLASS_", corineNonogo[i], "_", cldesc, "_1km_", doplot, ".png")
      cat("\n", fls, outfile)
      plotsmallwindow(cp, wint=doplot, plot2png=TRUE, param=cldesc)
      file.copy(paste0("cp", doplot, ".png"), 
                paste0(dir2save, outfile), overwrite=TRUE)
      
    }
  }
  
}

# Re-export tifs to data tables
# This is required because the forest-dt was not OK, thus better redo all of them
reconvert_tifs <- function(){
  
  
  dir2save <- "x:/adrian/data/fsu"
  dir2save <- "\\\\ies-ud01.jrc.it\\D5_agrienv\\Data\\FSU/corine/"
  corine_dir <- "\\\\ies-ud01.jrc.it\\D5_agrienv\\Data\\Corine_Land_Cover\\clc2018_v20_incl_turkey\\7ac95361f9ac3cecdf37785bc183ff02dd765a16\\clc2018_clc2018_v2018_20_raster100m/"
  corine_cats <- read.csv(paste0(corine_dir, "/CLC2018_CLC2018_V2018_20.txt"), header = FALSE)
  rcl_mat <- corine_cats[, 1:2]
  rcl_mat[, 2] <- 0
  rcl_mat4nogo <- rcl_mat
  rcl_mat4nogo[c(1:11, 30, 31, 34, 38, 39, 40:44), 2] <- 1
  corineNonogo <- rcl_mat4nogo[rcl_mat4nogo$V2 == 0, "V1"]
  corineNonogo <- c("NOGOs", "31forests", corineNonogo[-length(corineNonogo)])

  
  for (i in 1:(length(corineNonogo))){
    
    fls <- list.files(dir2save, paste0(corineNonogo[i], ".*tif"), full.names=TRUE)
    cldesc <- as.character(unlist(corine_cats[corine_cats$V1==corineNonogo[i], 6]))
    cp <- raster(fls)
    dt <- convertRaster2datatable(rast1=cp, rast2=uscie1km)
    names(dt) <- c("uscie", corineNonogo[i])
    clcshare <- melt.data.table(dt, id.vars="uscie", 
                          measure.vars=corineNonogo[i], variable.name="clc")
    clcshare[uscie=="24754286"]
    outfile <- paste0(dir2save, "/corineCLASS", corineNonogo[i], "_share100m_uscie.rdata")
    save(clcshare, file=outfile)
  }
  
}





testadmindata <- FALSE
if(testadmindata){
  forest <- raster("x:\\adrian\\data\\fsu/corineCLASS313_shares100m_uscie.tif")
  italt <- getData("alt", country="Italy")
  itadm <- getData("GADM", country="Italy", level=2)
  
}