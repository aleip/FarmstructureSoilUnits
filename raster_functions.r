plotsmallwindow <- function(x, wint="", plot2png=FALSE, param = ""){
  etnt <- extent(x)
  
  xy <- data.table(matrix(c("ispra", "Area around Ispra", "4200000", "4220000", "2520000", "2540000"), ncol = 6))
  xy <- rbind(xy, data.table(matrix(c("lago", "Area around Lago Maggiore", "4150000", "4300000", "2500000", "2620000"), ncol = 6)))
  xy <- rbind(xy, data.table(matrix(c("northitaly", "Alps - North Italy", "4000000", "4500000", "2500000", "3000000"), ncol = 6)))
  xy <- rbind(xy, data.table(matrix(c("small", "North Italy", "4150000", "4400000", "2450000", "2600000"), ncol = 6)))
  xy <- rbind(xy, data.table(matrix(c("eu", "European Union countries", "2550000", "6000000", "1300000", "5300000"), ncol = 6)))
  
  names(xy) <- c("name", "desc", "xmin", "xmax", "ymin", "ymax")
  cols <- names(xy)[3:6]
  xy <- xy[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]
  
  if(!wint %in% xy$name){
    message("Please choose an area to plot. The following are available:")
    print(xy)
    return()
  }
  
  xname <- substitute(x)
  etnt@xmin <- xy[name==wint, xmin]
  etnt@xmax <- xy[name==wint, xmax]
  etnt@ymin <- xy[name==wint, ymin]
  etnt@ymax <- xy[name==wint, ymax]
  
  x <- crop(x, etnt)
  if(plot2png) png(paste0(xname, wint, ".png"))
  plot(x, main = paste0(xy[name==wint, desc], "\n", param))
  #maps("world", add=TRUE)
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
