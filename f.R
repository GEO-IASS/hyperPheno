#######################################################################
# Auxiliary functions for the hyperPheno R package. 
# 
# The hyperPheno is developed and maintained by Bijan Seyednasrollah.
# The main initial development was done in November, 2017.
#
# Most recent release: https://github.com/bnasr/hyperPheno
#######################################################################

library(jpeg)
library(abind)
library(raster)
library(lubridate)

plotCLArray <- function(clArray, bands=1:3){
  tmp <- tempfile()
  if(length(dim(clArray))==2) 
    writeJPEG(clArray, target = tmp)
  else
    writeJPEG(clArray[,,bands], target = tmp)
  
  plotJPEG(tmp)
}


plotJPEG <- function(path, add=FALSE)
{
  jpgNative <-  readJPEG(path, native=T) # read the file
  res <-  dim(jpgNative)[1:2] # get the resolution
  if (!add) # initialize an empty plot area if add==FALSE
    plot(NA,xlim=c(1,res[2]),ylim=c(1,res[1]), type='n',
         xaxs='i',yaxs='i',
         # xaxt='n',yaxt='n',
         xlab='',ylab='',bty='o')
  rasterImage(jpgNative,1,1,res[2],res[1])
}

loadCube <- function(zipfile, header = 'headers/cube.hdr'){
  tmp <- '.tmp.cube'
  # if(file.exists(tmp)) file.remove(tmp)
  gunzip(filename = zipfile, destname = tmp, remove = FALSE, overwrite = TRUE)
  a <- try(read.ENVI(tmp, headerfile = header), silent = T)
  file.remove(tmp)
  # attributes(a)$name <- basename(zipfile)
  a
}

loadDark <- function(darkfile, header = 'headers/dark.hdr'){
  d <- try(read.ENVI(darkfile, headerfile = header), silent = T)
  if(class(d)=="try-error") return(NULL)
  m <- apply(d, 3, mean)
  m
}
saveJPEG <- function(hp, jpgFile, outDir = 'jpeg/'){
  
  r <- rotate(rotate(hp[,,56]))
  g <- rotate(rotate(hp[,,33]))
  b <- rotate(rotate(hp[,,22]))
  
  r <- r/(quantile(r, .99)*.99)
  g <- g/(quantile(g, .99)*.99)
  b <- b/(quantile(b, .99)*.99)
  
  rgb <- abind(r, g, b, along = 3)
  
  rgb[rgb>1] <- 1
  rgb[rgb<0] <- 0
  
  rgb <- aperm(rgb, perm = c(2,1,3))
  writeJPEG(rgb, target = paste0(outDir, jpgFile))
  invisible(rgb)
}

rotate <- function(x) t(apply(x, 2, rev))




parseROI <- function(roifilename, roipath = '', downloadDir = tempdir()){
  fname <- paste0(roipath, roifilename)
  #if(!file.exists(fname)) return(NULL)
  
  roilines <- readLines(fname)
  
  wEmptyLine <- roilines%in%c('', ' ',  '  ')
  wCommented <- as.vector(sapply(roilines, grepl,  pattern = '^#'))
  wNotSkip <- !(wEmptyLine|wCommented)
  
  
  parseroiline <- function(roilines, property){
    wProp <- grepl(roilines, pattern = property)
    gsub(roilines[wProp], pattern = paste0('# ', property, ': '), replacement = '')
  }
  
  ROIList <- list(siteName = parseroiline(roilines[wCommented], 'Site'), 
                  vegType = parseroiline(roilines[wCommented], 'Veg Type'), 
                  ID = as.numeric(parseroiline(roilines[wCommented], 'ROI ID Number')), 
                  Owner = parseroiline(roilines[wCommented], 'Owner'), 
                  createDate = parseroiline(roilines[wCommented], 'Creation Date'), 
                  createTime = parseroiline(roilines[wCommented], 'Creation Time'), 
                  updateDate = parseroiline(roilines[wCommented], 'Update Date'), 
                  updateTime = parseroiline(roilines[wCommented], 'Update Time'), 
                  Description = parseroiline(roilines[wCommented], 'Description'), 
                  masks = NULL)
  
  
  parsedMasks <- read.table(textConnection(roilines[which(wNotSkip)]), sep = ',', header = T)
  
  masksList <- list()
  for(i in 1:nrow(parsedMasks)){
    maskpath <- paste0(roipath, parsedMasks$maskfile[i])
    maskpointspath <- gsub(maskpath, pattern = '.tif', replacement = '_vector.csv')
    if(file.exists(maskpointspath)) {
      dummy=0
      maskpoints <- as.matrix(read.csv(maskpointspath, header = F, skip = 1))
    }else{
      maskpoints <- NULL
    }
    
    # maskpath <- tryDownload(maskpath, downloadDir = downloadDir, showLoad = T, Update = F)
    
    tmpMask <- list(maskpoints = maskpoints, 
                    startdate = as.character(parsedMasks$start_date[i]), 
                    starttime = as.character(parsedMasks$start_time[i]), 
                    enddate = as.character(parsedMasks$end_date[i]), 
                    endtime = as.character(parsedMasks$end_time[i]), 
                    sampleImage = as.character(parsedMasks$sample_image[i]),
                    rasteredMask = as.matrix(raster(maskpath)))
    
    tmpMask$rasteredMask[(!is.na(tmpMask$rasteredMask))&tmpMask$rasteredMask!=0] <- 1
    
    masksList[[length(masksList)+1]] <- tmpMask
    
  }
  names(masksList) <- gsub(parsedMasks$maskfile, pattern = '.tif', replacement = '')
  ROIList$masks <- masksList
  ROIList
}


getDNSpectra <- function(hp, hptime, ROI, prob = 0.5){
  m <- findMask(ROI, hptime)
  if(is.null(m)) return(NA)
  if(length(m)==0) return(NA)
  
  # par(mfrow=c(2,1)) 
  # plotCLArray(hp[,,50]/6000)
  # plotCLArray(m)
  
  dm <- dim(hp)
  sp <- rep(NA, dm[3])
  for(s in 1:dm[3]) {
    roi <- hp[,,s][m==0]
    roi <- roi[(roi<4095)]
    sp[s] <- quantile(roi, probs=prob, na.rm = T)
    
  }
  
  dummy <- 0
  sp
  
}

findMask <- function(ROI, hptime){
  hptime <- as.POSIXct(hptime, format='%Y-%m-%d %H:%M:%S')
  nm <- length(ROI$masks)
  mtimes <- data.frame(start= as.POSIXlt(rep(NA, nm)),
                       end= as.POSIXlt(rep(NA, nm)))
  
  for(j in 1:nm){
    m <- ROI$masks[[j]]
    mtimes$start[j] <- as.POSIXlt(paste(m$startdate, m$starttime), format='%Y-%m-%d %H:%M:%S')
    mtimes$end[j] <- as.POSIXlt(paste(m$enddate, m$endtime), format='%Y-%m-%d %H:%M:%S')
  }
  w <- apply(mtimes, MARGIN = 1, FUN = function(x){hptime>=x[1]&hptime<=x[2]})
  
  if(!any(w)) return(NULL)
  
  mi <- min(which(w))
  m <- ROI$masks[[mi]]$rasteredMask
  m <- rotate(rotate(t(m)))
  m
}


getDT <- function(datadir= '/mnt/monsoon/data/archive/harvardbarnsoc',
                  format='.cube.gz' ){
  cubes <- dir(path = datadir, pattern = paste0('*', format), full.names = T, recursive = T, all.files = T, no.. = T)
  info <- as.data.table(file.info(cubes))
  
  DT <- data.table(path=cubes, info[, .(size, mtime, ctime, atime)])
  splt <- strsplit(gsub(basename(DT$path),  pattern = format, replacement = ''), '_')
  # all(unlist(lapply(splt, length))==5)
  DT[, DateTime:=as.POSIXct(paste(apply(matrix(sprintf(as.numeric(unlist(strsplit(sapply(splt, function(x)(x[4])), split = '-'))), fmt = '%02d'), ncol=3, byrow = T), MARGIN = 1, paste, collapse='-'),
                                  unlist(lapply(strsplit(sapply(splt, function(x)(x[5])) , split = '.', fixed = T), FUN = function(x)({paste(sprintf('%02d',as.numeric(x)), collapse = ':')})))),
                            "%d-%m-%Y %H:%M:%S", tz = "")]
  
  DT[,Hour:=as.numeric(strftime(DateTime, format = '%H'))]
  DT[,HM:=format(DateTime, format = '%H:%M')]
  DT[,YMDHM:=format(DateTime, format = '%Y-%m-%d-%H-%M')]
  DT
}