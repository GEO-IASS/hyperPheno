source('f.R')
library(caTools)
library(R.utils)

w <- c(371.549988, 376.560722, 381.575760, 386.595102, 391.618748, 396.646698, 401.678952, 406.715511, 411.756373, 416.801539, 421.851009, 426.904783, 431.962861, 437.025243, 442.091929, 447.162920, 452.238214, 457.317812, 462.401714, 467.489920, 472.582430, 477.679244, 482.780362, 487.885784, 492.995511, 498.109541, 503.227875, 508.350513, 513.477455, 518.608701, 523.744251, 528.884105, 534.028264, 539.176726, 544.329492, 549.486562, 554.647936, 559.813614, 564.983596, 570.157882, 575.336473, 580.519367, 585.706565, 590.898067, 596.093873, 601.293983, 606.498397, 611.707115, 616.920138, 622.137464, 627.359094, 632.585028, 637.815266, 643.049808, 648.288654, 653.531804, 658.779259, 664.031017, 669.287079, 674.547445, 679.812115, 685.081089, 690.354367, 695.631949, 700.913836, 706.200026, 711.490520, 716.785318, 722.084420, 727.387826, 732.695536, 738.007550, 743.323869, 748.644491, 753.969417, 759.298647, 764.632181, 769.970019, 775.312161, 780.658607, 786.009358, 791.364412, 796.723770, 802.087432, 807.455398, 812.827668, 818.204242, 823.585120, 828.970303, 834.359789, 839.753579, 845.151673, 850.554071, 855.960773, 861.371779, 866.787090, 872.206704, 877.630622, 883.058844, 888.491370, 893.928200, 899.369334, 904.814772, 910.264515, 915.718561, 921.176911, 926.639565, 932.106523, 937.577785, 943.053351, 948.533222, 954.017396, 959.505874, 964.998656, 970.495742, 975.997132, 981.502826, 987.012824, 992.527127, 998.045733, 1003.568643, 1009.095857, 1014.627375, 1020.163197, 1025.703323, 1031.247754, 1036.796488, 1042.349526)
i <- 100


ref <- DN[,"RF12",i]/.12

plot(w, ref/ref, type='l', ylim=c(0,1))

lines(w, DN[,"DB1",i]/ref, type='l', col='green1')
lines(w, DN[,"DB2",i]/ref, type='l', col='green2')
lines(w, DN[,"DB3",i]/ref, type='l', col='green3')
lines(w, DN[,"DB4",i]/ref, type='l', col='green4')
lines(w, DN[,"DB5",i]/ref, type='l', col='greenyellow')

lines(w, DN[,"EN1",i]/ref, type='l', col='rosybrown1')
lines(w, DN[,"EN2",i]/ref, type='l', col='rosybrown2')
lines(w, DN[,"EN3",i]/ref, type='l', col='rosybrown3')
#lines(w, DN[,"EN4",i]/ref, type='l', col='rosybrown4')
# lines(w, DN[,"EN5",i]/ref, type='l', col='salmon4')



#=====

dark <- '/home/bijan/Projects/hyperPheno/files/harvardbarnsoc_I6_L0-511_21-7-2015_13.0.15.dark'
cube <- '/home/bijan/Projects/hyperPheno/files/harvardbarnsoc_I6_L0-511_21-7-2015_13.0.54.cube'
d <- read.ENVI(dark, headerfile = 'headers/dark.hdr')


cubes <- dir(path = 'files/', pattern = '*.cube.gz', full.names = T, recursive = T, all.files = T, no.. = T)
cubes <- dir(path = 'files/tmp/', pattern = '*.cube.gz', full.names = T, recursive = F, all.files = T, no.. = T)
cubes <- cubes[file.info(cubes)$size>10000]

labs <- basename(cubes)
n <- length(cubes)

c <- NULL
c <- list()
pb <- txtProgressBar(0, n, style = 3)
for(i in 1:n) {
  c[[i]] <- loadCube(cubes[i])
  c[[i]] 
  setTxtProgressBar(pb, value = i)
}
names(c) <- labs


plotCLArray(c[[1]][,,c(56, 33, 22)]/800)
plotCLArray(c[[8]][,,c(56, 33, 22)]/500)

q <- t(apply(a, quantile, MARGIN = 3, probs=c(0.025, 0.25, 0.5, 0.75, 0.975)))

plot(w, q[,3], type = 'l')
arrows(w, q[,1], w, q[,5], length=0.00, col = '#808080')
arrows(w, q[,2], w, q[,4], length=0.00, col = '#404040', lwd=2)

lines(w, m)
range(a)


plotCLArray(a[,,c(100, 40, 2)]/300)

f <- file('~/tmp/harvardbarnsoc_I6_L0-511_5-8-2015_14.0.25.dark', 'rb')
d =readBin(f, 'int', 1000)
close(f)
summary(d)





# plot(w,  sp99, type='l')
#   
# sp <- data.table(sp99 = sp99, sp50 = sp50, sp25 = sp25, sp12 = sp12)
# apply(sp/sp12, 2, median)
# 
# 
# spref <- sp99
# plot(w,  sp99/spref, type='l', ylim=c(0,1))
# lines(w, sp50/spref, col=2)
# lines(w, sp25/spref, col=3)
# lines(w, sp12/spref, col=4)
# legend('bottom', legend = paste('RF', c(99, 50, 25, 12)), lwd=2, col=1:4, bty='n')


# 
# 4.684895 3.198958 1.714619 1.000000 
# 4.443976 3.255957 1.813654 1.000000 
# 3.398848 2.756278 1.822846 1.000000 
# 3.549032 2.918493 1.797300 1.000000 
# 3.000029 2.689718 1.777293 1.000000 
# 5.462456 3.146062 1.993178 1.000000 
# 2.254243 2.114976 1.599499 1.000000 
# 
# 1.0000000 0.6793078 0.3668119 0.2126985 
# 1.0000000 0.7245331 0.4059619 0.2223389 
# 1.0000000 0.8100272 0.5134471 0.2942174 
# 1.0000000 0.8117048 0.5026248 0.2817694 
# 1.0000000 0.8969330 0.5886352 0.3296061 
# 1.0000000 0.5759981 0.3650630 0.1830683 
# 1.0000000 0.9449234 0.6992847 0.4387997 
# 
# 
# 

