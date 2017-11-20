# selDT <- DT[size>1000&Hour%in%(13)]
selDT <- DT[size>1000&HM>='13:00'&HM<'13:30']
str(selDT)
selDT
n <- nrow(selDT)

jpgFiles <- gsub(pattern = '.cube.gz', replacement = '.jpg', x = basename(selDT$path))
write.table(selDT[,.(path=jpgFiles, DateTime=DateTime)], 'harvardsoc/jpeg2/files.txt', row.names = F, col.names = T, sep = ',')

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
    saveJPEG(hp, jpgFile, 'harvardsoc/jpeg2/')
  setTxtProgressBar(pb, value = i)
}



