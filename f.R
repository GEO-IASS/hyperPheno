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

loadCube <- function(zipfile, header = 'header.hdr'){
  tmp <- '.tmp.cube'
  # if(file.exists(tmp)) file.remove(tmp)
  gunzip(filename = zipfile, destname = tmp, remove = FALSE, overwrite = TRUE)
  a <- try(read.ENVI(tmp, headerfile = header), silent = T)
  file.remove(tmp)
  # attributes(a)$name <- basename(zipfile)
  a
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
