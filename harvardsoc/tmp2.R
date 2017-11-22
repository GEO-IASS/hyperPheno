library(ggplot2)

library(data.table)
library(lubridate)
# load('../hyperPheno/DN.RData')
# load('../hyperPheno/DN.array.RData')
DN <- DN.array
w <- c(371.549988, 376.560722, 381.575760, 386.595102, 391.618748, 396.646698, 401.678952, 406.715511, 411.756373, 416.801539, 421.851009, 426.904783, 431.962861, 437.025243, 442.091929, 447.162920, 452.238214, 457.317812, 462.401714, 467.489920, 472.582430, 477.679244, 482.780362, 487.885784, 492.995511, 498.109541, 503.227875, 508.350513, 513.477455, 518.608701, 523.744251, 528.884105, 534.028264, 539.176726, 544.329492, 549.486562, 554.647936, 559.813614, 564.983596, 570.157882, 575.336473, 580.519367, 585.706565, 590.898067, 596.093873, 601.293983, 606.498397, 611.707115, 616.920138, 622.137464, 627.359094, 632.585028, 637.815266, 643.049808, 648.288654, 653.531804, 658.779259, 664.031017, 669.287079, 674.547445, 679.812115, 685.081089, 690.354367, 695.631949, 700.913836, 706.200026, 711.490520, 716.785318, 722.084420, 727.387826, 732.695536, 738.007550, 743.323869, 748.644491, 753.969417, 759.298647, 764.632181, 769.970019, 775.312161, 780.658607, 786.009358, 791.364412, 796.723770, 802.087432, 807.455398, 812.827668, 818.204242, 823.585120, 828.970303, 834.359789, 839.753579, 845.151673, 850.554071, 855.960773, 861.371779, 866.787090, 872.206704, 877.630622, 883.058844, 888.491370, 893.928200, 899.369334, 904.814772, 910.264515, 915.718561, 921.176911, 926.639565, 932.106523, 937.577785, 943.053351, 948.533222, 954.017396, 959.505874, 964.998656, 970.495742, 975.997132, 981.502826, 987.012824, 992.527127, 998.045733, 1003.568643, 1009.095857, 1014.627375, 1020.163197, 1025.703323, 1031.247754, 1036.796488, 1042.349526)
excldWs <- which(w>=940|w<400)
w <- w[-excldWs]
DN <- DN[-excldWs,,]

r <- aperm(apply(DN, c(1,3), function(x){x/x[1]}), c(2,1,3))
sc <- apply(r, c(2,3), function(x){x/sum(x)})

n <- dim(DN)[3]

t <- as.POSIXct(gsub(pattern = 'X', '', dimnames(DN)[[3]]), format='%Y.%m.%d.%H.%M.%S')
DT <- data.table(time=t)
DT[,index:=1:.N]
DT[,doy:=yday(time)]
DT[,d:=day(time)]
DT[,m:=month(time)]
DT[,y:=year(time)]
DT[,w:=week(time)]
DT[,g:=paste(y,sprintf('%02d',w), sep='.')]
DT



findOutliers <- function(x, probs = c(.05,.95)){
  rng <- quantile(x, na.rm = T, probs = probs)
  w <- which(x<rng[1]|x>rng[2])
  w
}

outliersLow <- apply(apply(DN, c(2,3), sum), 1, findOutliers, probs = c(.25, .95))
outliers90 <- apply(apply(DN, c(2,3), sum), 1, findOutliers, probs = c(.05, .95))


par(mfcol=c(1,2), mar = c(3,2,1,1))
# col <- (12, alpha = .5)
cc <- c(
  '#a6cee3',
  '#1f78b4',
  '#b2df8a',
  '#33a02c',
  '#fb9a99',
  '#e31a1c',
  '#fdbf6f',
  '#ff7f00',
  '#cab2d6',
  '#6a3d9a',
  '#ffff99',
  '#b15928'
  )
col <- colorRampPalette(cc)(52)
# col <- paste0(col,'80')

f <- c(outliersLow$RF12, outliers90$DB1)
plotSp(w, r[,"DB1", -f]*.12, col = col, g = DT$w[-f], ylim = c(0,.5), sleep = 1)

f <- c(outliersLow$RF12, outliers90$EN1)
plotSp(w, r[,"EN1", -f]*.12, col = col, g = DT$w[-f], ylim = c(0,.5), sleep = 1)

# a <- cor(na.omit(apply(sc[,"DB1", -c(outliersLow$RF12, outliers90$DB1)], 1, diff)))
# b <- cor(na.omit(apply(sc[,"EN4", -c(outliersLow$RF12, outliers90$EN4)], 1, diff)))
# gplots::heatmap.2(a, dendrogram='none', Rowv=F, Colv=FALSE,trace='none')
# gplots::heatmap.2(b, dendrogram='none', Rowv=F, Colv=FALSE,trace='none')


tb <- data.table( DT,  
                  NIR = apply(r[w>750,"DB1", ], MARGIN = 2, mean, na.rm=T), 
                  VIS = apply(r[w<750,"DB1", ], MARGIN = 2, mean, na.rm=T))
tb[,ct:=y+(w-1)/52]
ggplot(tb, aes(ct, NIR)) + geom_smooth()
ggplot(tb, aes(ct, VIS)) + geom_smooth()


plotSp(w, sc[,"DB1", -c(outliersLow$RF12, outliers90$DB1)], col = '#30703007', sleep = .1, )


plotSp(w, sc[,"DB1", -c(outliersLow$RF12, outliers90$DB1)], col = '#30703007', sleep = .1)
# col <- colorRampPalette(c('blue','green1', 'green4','orange'))(12)[month(t)]
# col <- paste0(col,'07')
col <- rainbow(365, alpha = .01)[yday(t)]

par(mfcol=c(2,1), mar = c(3,2,1,1))
plotSp(w, sc[,"DB1", -c(outliersLow$RF12, outliers90$DB1)], col = col)
plotSp(w, sc[,"EN1", -c(outliersLow$RF12, outliers90$EN1)], col = col)

par(mfcol=c(5,2), mar = c(3,2,1,1))

plotSp(w, sc[,"DB2", -c(outliersLow$RF12, outliers90$DB2)], col = col)
plotSp(w, sc[,"DB3", -c(outliersLow$RF12, outliers90$DB3)], col = col)
plotSp(w, sc[,"DB4", -c(outliersLow$RF12, outliers90$DB4)], col = col)
plotSp(w, sc[,"DB5", -c(outliersLow$RF12, outliers90$DB5)], col = col)

plotSp(w, sc[,"EN1", -c(outliersLow$RF12, outliers90$EN1)], col = col)
plotSp(w, sc[,"EN2", -c(outliersLow$RF12, outliers90$EN2)], col = col)
plotSp(w, sc[,"EN3", -c(outliersLow$RF12, outliers90$EN3)], col = col)
plotSp(w, sc[,"EN4", -c(outliersLow$RF12, outliers90$EN4)], col = col)
plotSp(w, sc[,"EN5", -c(outliersLow$RF12, outliers90$EN5)], col = col)



par(mfcol=c(2,1), mar = c(3,2,1,1))
plotSp(w, r[,"DB1", -c(outliersLow$RF12, outliers90$DB1)]*.12, col = col, sleep = NULL, ylim = c(0,1))
plotSp(w, r[,"EN1", -c(outliersLow$RF12, outliers90$EN1)]*.12, col = col, sleep = NULL, ylim = c(0,1))








a <- cor(na.omit(apply(sc[,"DB1", -c(outliersLow$RF12, outliers90$DB1)], 1, diff)))
rownames(a) <- colnames(a) <- w
heatmap(a)
gplots::heatmap.2(a, dendrogram='none', Rowv=F, Colv=FALSE,trace='none')

b <- sc[,"EN4", -c(outliersLow$RF12, outliers90$EN4)]

plotSp(w, sc[,"DB1", -c(outliersLow$RF12, outliers90$DB1)], col = '#30703010', sleep = NULL, ylim = c(0, .04))
plotSp(w, sc[,"EN4", -c(outliersLow$RF12, outliers90$EN4)], col = '#70303010', sleep = NULL, add = T)


plotSp(w, sc[,"DB1", -c(outliersLow$RF12, outliers90$DB1)], col = '#30703007', sleep = .1)
plotSp(w, sc[,"EN1", -c(outliersLow$RF12, outliers90$EN1)], col = '#70303007', sleep = .1, add = T)

plotSp(w, sc[,"EN2", -c(outliersLow$RF12, outliers90$EN2)], col = '#70303007', sleep = .1, add = F)

plotSp(w, sc[,"EN3", -c(outliersLow$RF12, outliers90$EN3)], col = '#70303007', sleep = NULL, add = F)
plotSp(w, sc[,"EN4", -c(outliersLow$RF12, outliers90$EN4)], col = '#70303007', sleep = NULL, add = F)


par(mfcol=c(5,2), mar = c(3,2,1,1))

plotSp(w, sc[,"DB1", -c(outliersLow$RF12, outliers90$DB1)], col = '#30803020')
plotSp(w, sc[,"DB2", -c(outliersLow$RF12, outliers90$DB2)], col = '#30803020')
plotSp(w, sc[,"DB3", -c(outliersLow$RF12, outliers90$DB3)], col = '#30803020')
plotSp(w, sc[,"DB4", -c(outliersLow$RF12, outliers90$DB4)], col = '#30803020')
plotSp(w, sc[,"DB5", -c(outliersLow$RF12, outliers90$DB5)], col = '#30803020')

plotSp(w, sc[,"EN1", -c(outliersLow$RF12, outliers90$EN1)], col = '#80303020')
plotSp(w, sc[,"EN2", -c(outliersLow$RF12, outliers90$EN2)], col = '#80303020')
plotSp(w, sc[,"EN3", -c(outliersLow$RF12, outliers90$EN3)], col = '#80303020')
plotSp(w, sc[,"EN4", -c(outliersLow$RF12, outliers90$EN4)], col = '#80303020')
plotSp(w, sc[,"EN5", -c(outliersLow$RF12, outliers90$EN5)], col = '#80303020')

plotSp(w, sc[,"DB1", -c(outliersLow$RF12, outliers90$DB1)], col = '#30803020', sleep = .1)


