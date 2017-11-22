selDT <- darksDT[size>1000&HM>='13:00'&HM<'13:30']
n <- nrow(selDT)
if(file.exists('./.tmp.cube.tmp'))file.remove('./.tmp.cube.tmp')
darkSpectra <- matrix(NA, n, 128)
pb <- txtProgressBar(0, n, style = 3)
for(i in 1:n){
  sp <- loadDark(selDT$path[i], header = 'headers/dark.hdr')
  if(!is.null(sp)) darkSpectra[i,] <- sp
  setTxtProgressBar(pb, value = i)
}
darkSpectra <- na.omit(darkSpectra)
q <- apply(darkSpectra, MARGIN = 2, quantile)