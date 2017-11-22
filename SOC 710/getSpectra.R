source('hyperPheno.R')
source('readROI.R')

# selDT <- cubesDT[size>1000&Hour>=9&Hour<=17]
selDT <- cubesDT
str(selDT)
selDT
n <- nrow(selDT)

if(file.exists('./.tmp.cube.tmp'))file.remove('./.tmp.cube.tmp')
ROIs <- c('RF12', 'RF25', 'RF50', 'RF99', 'DB1', 'DB2', 'DB3', 'DB4', 'DB5', 'EN1', 'EN2', 'EN3', 'EN4', 'EN5')

DN.array <- array(dim = c(128, 14, n))
dimnames(DN.array) <- list(1:128, ROIs, selDT$DateTime)
dimnames(DN.array) <- list(1:128, ROIs, make.names(as.character(selDT$DateTime)))

pb <- txtProgressBar(0, n, style = 3)
for(i in 1:n){
  zipfile <- selDT$path[i]
  hp <- loadCube(zipfile, header = 'headers/cube.hdr')
  hptime <- selDT$DateTime[i]
  if(class(hp)!="try-error"){
    DN.array[,1,i] <- getDNSpectra(hp, hptime, ROI = RF12, prob = 0.5 ) - darkSpectra
    DN.array[,2,i] <- getDNSpectra(hp, hptime, ROI = RF25, prob = 0.5 ) - darkSpectra
    DN.array[,3,i] <- getDNSpectra(hp, hptime, ROI = RF50, prob = 0.5 ) - darkSpectra
    DN.array[,4,i] <- getDNSpectra(hp, hptime, ROI = RF99, prob = 0.5 ) - darkSpectra
    
    DN.array[,5,i] <- getDNSpectra(hp, hptime, ROI = DB1, prob = 0.5 ) - darkSpectra
    DN.array[,6,i] <- getDNSpectra(hp, hptime, ROI = DB2, prob = 0.5 ) - darkSpectra
    DN.array[,7,i] <- getDNSpectra(hp, hptime, ROI = DB3, prob = 0.5 ) - darkSpectra
    DN.array[,8,i] <- getDNSpectra(hp, hptime, ROI = DB4, prob = 0.5 ) - darkSpectra
    DN.array[,9,i] <- getDNSpectra(hp, hptime, ROI = DB5, prob = 0.5 ) - darkSpectra
    
    DN.array[,10,i] <- getDNSpectra(hp, hptime, ROI = EN1, prob = 0.5 ) - darkSpectra
    DN.array[,11,i] <- getDNSpectra(hp, hptime, ROI = EN2, prob = 0.5 ) - darkSpectra
    DN.array[,12,i] <- getDNSpectra(hp, hptime, ROI = EN3, prob = 0.5 ) - darkSpectra
    DN.array[,13,i] <- getDNSpectra(hp, hptime, ROI = EN4, prob = 0.5 ) - darkSpectra
    DN.array[,14,i] <- getDNSpectra(hp, hptime, ROI = EN5, prob = 0.5 ) - darkSpectra
  }
  setTxtProgressBar(pb, value = i)
}

# save(DN.array, file='DN.array.RData')

 

