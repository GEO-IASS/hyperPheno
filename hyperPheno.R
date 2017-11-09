#######################################################################
# The main scripts functions for the hyperPheno R package. 
# 
# The hyperPheno is developed and maintained by Bijan Seyednasrollah.
# The main initial development was done in November, 2017.
#
# Most recent release: https://github.com/bnasr/hyperPheno
#######################################################################


library(R.utils)
library(data.table)
library(caTools)
source('f.R')

datadir <- '/mnt/monsoon/data/archive/harvardbarnsoc'
cubes <- dir(path = datadir, pattern = '*.cube.gz', full.names = T, recursive = T, all.files = T, no.. = T)
info <- as.data.table(file.info(cubes))

DT <- data.table(path=cubes, info[, .(size, mtime, ctime, atime)])
splt <- strsplit(gsub(basename(DT$path),  pattern = '.cube.gz', replacement = ''), '_')
# all(unlist(lapply(splt, length))==5)
DT[, DateTime:=as.POSIXct(paste(apply(matrix(sprintf(as.numeric(unlist(strsplit(sapply(splt, function(x)(x[4])), split = '-'))), fmt = '%02d'), ncol=3, byrow = T), MARGIN = 1, paste, collapse='-'),
                                unlist(lapply(strsplit(sapply(splt, function(x)(x[5])) , split = '.', fixed = T), FUN = function(x)({paste(sprintf('%02d',as.numeric(x)), collapse = ':')})))),
                          "%d-%m-%Y %H:%M:%S", tz = "")]
DT[,Hour:=as.numeric(strftime(DateTime, format = '%H'))]
DT[,HM:=format(DateTime, format = '%H:%M')]
str(DT)

# selDT <- DT[size>1000&Hour%in%(13)]
selDT <- DT[size>1000&HM>='13:00'&HM<'13:30']
str(selDT)
selDT
n <- nrow(selDT)

jpgFiles <- gsub(pattern = '.cube.gz', replacement = '.jpg', x = basename(selDT$path))
write.table(selDT[,.(path=jpgFiles, DateTime=DateTime)], 'jpeg2/files.txt', row.names = F, col.names = T, sep = ',')

if(file.exists('./.tmp.cube.tmp'))file.remove('./.tmp.cube.tmp')

pb <- txtProgressBar(0, n, style = 3)
for(i in 1:n){
  zipfile <- selDT$path[i]
  jpgFile <- jpgFiles[i]
  # jpgFile <- paste0('harvardbarnsoc  ', 
  #                   strftime(selDT$DateTime[i], format = '%Y-%m-%d  %H-%M-%S'), 
  #                   '  ',
  #                   paste(strsplit(strsplit(basename(zipfile), '-')[[1]][1], '_')[[1]][2:3], collapse = '-'),
  #                   '.jpg')
  
  hp <- loadCube(zipfile, header = 'headers/cube.hdr')
  if(class(hp)!="try-error")
    saveJPEG(hp, jpgFile, 'jpeg2/')
  setTxtProgressBar(pb, value = i)
}



