



library(mvtnorm)

f = matrix(rep(seq(min(partHome$freqHome), max(partHome$freqHome), length.out = 1000), 1000), 1000, 1000)
d = t(matrix(rep(seq(min(partHome$durnHome), max(partHome$durnHome), length.out = 1000), 1000), 1000, 1000))
total.time = f * d



durnHome.DEN.Child = partHome$durnHome[paste(partHome$pathogen,partHome$category) == 'DEN Child']
freqHome.DEN.Child = partHome$freqHome[paste(partHome$pathogen,partHome$category) == 'DEN Child']
par = coef(timeHome.models$F3.C4$DEN.Child)
surf.DEN.Child =
  matrix(dmvnorm(cbind(log(as.vector(f)), log(as.vector(d))),
                 c(par['freq.mean'], par['durn.mean']),
                 matrix(c(
                   par['freq.std'] ^ 2,
                   prod(par[c('freq.std', 'durn.std', 'corr')]),
                   prod(par[c('freq.std', 'durn.std', 'corr')]),
                   par['durn.std'] ^ 2),
                   2, 2)),
         1000, 1000)
surf.DEN.Child[total.time > 1] = -1


durnHome.DEN.Homemaker = partHome$durnHome[paste(partHome$pathogen,partHome$category) == 'DEN Homemaker']
freqHome.DEN.Homemaker = partHome$freqHome[paste(partHome$pathogen,partHome$category) == 'DEN Homemaker']
par = coef(timeHome.models$F3.C4$DEN.Homemaker)
surf.DEN.Homemaker =
  matrix(dmvnorm(cbind(log(as.vector(f)), log(as.vector(d))),
                 c(par['freq.mean'], par['durn.mean']),
                 matrix(c(
                   par['freq.std'] ^ 2,
                   prod(par[c('freq.std', 'durn.std', 'corr')]),
                   prod(par[c('freq.std', 'durn.std', 'corr')]),
                   par['durn.std'] ^ 2),
                 2, 2)),
1000, 1000)
surf.DEN.Homemaker[total.time > 1] = -1


durnHome.DEN.Other = partHome$durnHome[paste(partHome$pathogen,partHome$category) == 'DEN Other']
freqHome.DEN.Other = partHome$freqHome[paste(partHome$pathogen,partHome$category) == 'DEN Other']
par = coef(timeHome.models$F3.C4$DEN.Other)
surf.DEN.Other =
  matrix(dmvnorm(cbind(log(as.vector(f)), log(as.vector(d))),
                 c(par['freq.mean'], par['durn.mean']),
                 matrix(c(
                   par['freq.std'] ^ 2,
                   prod(par[c('freq.std', 'durn.std', 'corr')]),
                   prod(par[c('freq.std', 'durn.std', 'corr')]),
                   par['durn.std'] ^ 2),
                 2, 2)),
1000, 1000)
surf.DEN.Other[total.time > 1] = -1


durnHome.DEN.Student = partHome$durnHome[paste(partHome$pathogen,partHome$category) == 'DEN Student']
freqHome.DEN.Student = partHome$freqHome[paste(partHome$pathogen,partHome$category) == 'DEN Student']
par = coef(timeHome.models$F3.C4$DEN.Student)
surf.DEN.Student =
  matrix(dmvnorm(cbind(log(as.vector(f)), log(as.vector(d))),
                 c(par['freq.mean'], par['durn.mean']),
                 matrix(c(
                   par['freq.std'] ^ 2,
                   prod(par[c('freq.std', 'durn.std', 'corr')]),
                   prod(par[c('freq.std', 'durn.std', 'corr')]),
                   par['durn.std'] ^ 2),
                   2, 2)),
         1000, 1000)
surf.DEN.Student[total.time > 1] = -1

durnHome.FEV.Child = partHome$durnHome[paste(partHome$pathogen,partHome$category) == 'FEV Child']
freqHome.FEV.Child = partHome$freqHome[paste(partHome$pathogen,partHome$category) == 'FEV Child']
par = coef(timeHome.models$F3.C4$FEV.Child)
surf.FEV.Child =
  matrix(dmvnorm(cbind(log(as.vector(f)), log(as.vector(d))),
                 c(par['freq.mean'], par['durn.mean']),
                 matrix(c(
                   par['freq.std'] ^ 2,
                   prod(par[c('freq.std', 'durn.std', 'corr')]),
                   prod(par[c('freq.std', 'durn.std', 'corr')]),
                   par['durn.std'] ^ 2),
                   2, 2)),
         1000, 1000)
surf.FEV.Child[total.time > 1] = -1

durnHome.FEV.Homemaker = partHome$durnHome[paste(partHome$pathogen,partHome$category) == 'FEV Homemaker']
freqHome.FEV.Homemaker = partHome$freqHome[paste(partHome$pathogen,partHome$category) == 'FEV Homemaker']
par = coef(timeHome.models$F3.C4$FEV.Homemaker)
surf.FEV.Homemaker =
  matrix(dmvnorm(cbind(log(as.vector(f)), log(as.vector(d))),
                 c(par['freq.mean'], par['durn.mean']),
                 matrix(c(
                   par['freq.std'] ^ 2,
                   prod(par[c('freq.std', 'durn.std', 'corr')]),
                   prod(par[c('freq.std', 'durn.std', 'corr')]),
                   par['durn.std'] ^ 2),
                   2, 2)),
         1000, 1000)
surf.FEV.Homemaker[total.time > 1] = -1

durnHome.FEV.Other = partHome$durnHome[paste(partHome$pathogen,partHome$category) == 'FEV Other']
freqHome.FEV.Other = partHome$freqHome[paste(partHome$pathogen,partHome$category) == 'FEV Other']
par = coef(timeHome.models$F3.C4$FEV.Other)
surf.FEV.Other =
  matrix(dmvnorm(cbind(log(as.vector(f)), log(as.vector(d))),
                 c(par['freq.mean'], par['durn.mean']),
                 matrix(c(
                   par['freq.std'] ^ 2,
                   prod(par[c('freq.std', 'durn.std', 'corr')]),
                   prod(par[c('freq.std', 'durn.std', 'corr')]),
                   par['durn.std'] ^ 2),
                   2, 2)),
         1000, 1000)
surf.FEV.Other[total.time > 1] = -1

durnHome.FEV.Student = partHome$durnHome[paste(partHome$pathogen,partHome$category) == 'FEV Student']
freqHome.FEV.Student = partHome$freqHome[paste(partHome$pathogen,partHome$category) == 'FEV Student']
par = coef(timeHome.models$F3.C4$FEV.Student)
surf.FEV.Student =
  matrix(dmvnorm(cbind(log(as.vector(f)), log(as.vector(d))),
                 c(par['freq.mean'], par['durn.mean']),
                 matrix(c(
                   par['freq.std'] ^ 2,
                   prod(par[c('freq.std', 'durn.std', 'corr')]),
                   prod(par[c('freq.std', 'durn.std', 'corr')]),
                   par['durn.std'] ^ 2),
                   2, 2)),
         1000, 1000)
surf.FEV.Student[total.time > 1] = -1

durnHome.Healthy.Child = partHome$durnHome[paste(partHome$pathogen,partHome$category) == 'NA Child']
freqHome.Healthy.Child = partHome$freqHome[paste(partHome$pathogen,partHome$category) == 'NA Child']
par = coef(timeHome.models$F3.C4$Healthy.Child)
surf.Healthy.Child =
  matrix(dmvnorm(cbind(log(as.vector(f)), log(as.vector(d))),
                 c(par['freq.mean'], par['durn.mean']),
                 matrix(c(
                   par['freq.std'] ^ 2,
                   prod(par[c('freq.std', 'durn.std', 'corr')]),
                   prod(par[c('freq.std', 'durn.std', 'corr')]),
                   par['durn.std'] ^ 2),
                   2, 2)),
         1000, 1000)
surf.Healthy.Child[total.time > 1] = -1

durnHome.Healthy.Homemaker = partHome$durnHome[paste(partHome$pathogen,partHome$category) == 'NA Homemaker']
freqHome.Healthy.Homemaker = partHome$freqHome[paste(partHome$pathogen,partHome$category) == 'NA Homemaker']
par = coef(timeHome.models$F3.C4$Healthy.Homemaker)
surf.Healthy.Homemaker =
  matrix(dmvnorm(cbind(log(as.vector(f)), log(as.vector(d))),
                 c(par['freq.mean'], par['durn.mean']),
                 matrix(c(
                   par['freq.std'] ^ 2,
                   prod(par[c('freq.std', 'durn.std', 'corr')]),
                   prod(par[c('freq.std', 'durn.std', 'corr')]),
                   par['durn.std'] ^ 2),
                   2, 2)),
         1000, 1000)
surf.Healthy.Homemaker[total.time > 1] = -1

durnHome.Healthy.Other = partHome$durnHome[paste(partHome$pathogen,partHome$category) == 'NA Other']
freqHome.Healthy.Other = partHome$freqHome[paste(partHome$pathogen,partHome$category) == 'NA Other']
par = coef(timeHome.models$F3.C4$Healthy.Other)
surf.Healthy.Other =
  matrix(dmvnorm(cbind(log(as.vector(f)), log(as.vector(d))),
                 c(par['freq.mean'], par['durn.mean']),
                 matrix(c(
                   par['freq.std'] ^ 2,
                   prod(par[c('freq.std', 'durn.std', 'corr')]),
                   prod(par[c('freq.std', 'durn.std', 'corr')]),
                   par['durn.std'] ^ 2),
                   2, 2)),
         1000, 1000)
surf.Healthy.Other[total.time > 1] = -1

durnHome.Healthy.Student = partHome$durnHome[paste(partHome$pathogen,partHome$category) == 'NA Student']
freqHome.Healthy.Student = partHome$freqHome[paste(partHome$pathogen,partHome$category) == 'NA Student']
par = coef(timeHome.models$F3.C4$Healthy.Student)
surf.Healthy.Student =
  matrix(dmvnorm(cbind(log(as.vector(f)), log(as.vector(d))),
                 c(par['freq.mean'], par['durn.mean']),
                 matrix(c(
                   par['freq.std'] ^ 2,
                   prod(par[c('freq.std', 'durn.std', 'corr')]),
                   prod(par[c('freq.std', 'durn.std', 'corr')]),
                   par['durn.std'] ^ 2),
                   2, 2)),
         1000, 1000)
surf.Healthy.Student[total.time > 1] = -1





tiff(filename = '../output/freqDurn.tiff', width = 960, height = 720)

par(mar = c(.5, 1, .5, 0), oma = c(4, 4, 4, 4))
layout(matrix(1 : 12, 3, 4, byrow = TRUE))


image(f[, 1], d[1, ], surf.Healthy.Child,
      zlim = range(surf.Healthy.Child[surf.Healthy.Child > -1]),
      xlab = '', ylab = '', main = '', xaxt = 'n', cex = 1.5)
points(freqHome.Healthy.Child, durnHome.Healthy.Child, cex = .25, pch = 20, col = rgb(0, 0, 0, .6))
text(8, .9,
  paste('mean duration = ', as.character(formatC(mean(durnHome.Healthy.Child), digits = 2)), sep = ''),pos=2,cex=1.6)
text(8, .8,
     paste('mean frequency = ', as.character(formatC(mean(freqHome.Healthy.Child), digits = 2)), sep = ''),pos=2,cex=1.6)
text(8, .7,
     paste('mean time = ', as.character(formatC(mean(durnHome.Healthy.Child)*mean(freqHome.Healthy.Child), digits = 2)), sep = ''),pos=2,cex=1.6)
mtext('School-age children',3,line=1)

image(f[, 1], d[1, ], surf.Healthy.Student,
      zlim = range(surf.Healthy.Student[surf.Healthy.Student > -1]),
      xlab = '', ylab = '', main = '', xaxt = 'n', yaxt = 'n', cex = 1.5)
points(freqHome.Healthy.Student, durnHome.Healthy.Student, cex = .25, pch = 20, col = rgb(0, 0, 0, .6))
text(8, .9,
     paste('mean duration = ', as.character(formatC(mean(durnHome.Healthy.Student), digits = 2)), sep = ''),pos=2,cex=1.6)
text(8, .8,
     paste('mean frequency = ', as.character(formatC(mean(freqHome.Healthy.Student), digits = 2)), sep = ''),pos=2,cex=1.6)
text(8, .7,
     paste('mean time = ', as.character(formatC(mean(durnHome.Healthy.Student)*mean(freqHome.Healthy.Student), digits = 2)), sep = ''),pos=2,cex=1.6)
mtext('College students',3,line=1)

image(f[, 1], d[1, ], surf.Healthy.Homemaker,
      zlim = range(surf.Healthy.Homemaker[surf.Healthy.Homemaker > -1]),
      xlab = '', ylab = '', main = '', xaxt = 'n', yaxt = 'n', cex = 1.5)
points(freqHome.Healthy.Homemaker, durnHome.Healthy.Homemaker, cex = .25, pch = 20, col = rgb(0, 0, 0, .6))
text(8, .9,
     paste('mean duration = ', as.character(formatC(mean(durnHome.Healthy.Homemaker), digits = 2)), sep = ''),pos=2,cex=1.6)
text(8, .8,
     paste('mean frequency = ', as.character(formatC(mean(freqHome.Healthy.Homemaker), digits = 2)), sep = ''),pos=2,cex=1.6)
text(8, .7,
     paste('mean time = ', as.character(formatC(mean(durnHome.Healthy.Homemaker)*mean(freqHome.Healthy.Homemaker), digits = 2)), sep = ''),pos=2,cex=1.6)
mtext('Homemakers / Unemployed adults',3,line=1)

image(f[, 1], d[1, ], surf.Healthy.Other,
      zlim = range(surf.Healthy.Other[surf.Healthy.Other > -1]),
      xlab = '', ylab = '', main = '', xaxt = 'n', yaxt = 'n', cex = 1.5)
points(freqHome.Healthy.Other, durnHome.Healthy.Other, cex = .25, pch = 20, col = rgb(0, 0, 0, .6))
text(8, .9,
     paste('mean duration = ', as.character(formatC(mean(durnHome.Healthy.Other), digits = 2)), sep = ''),pos=2,cex=1.6)
text(8, .8,
     paste('mean frequency = ', as.character(formatC(mean(freqHome.Healthy.Other), digits = 2)), sep = ''),pos=2,cex=1.6)
text(8, .7,
     paste('mean time = ', as.character(formatC(mean(durnHome.Healthy.Other)*mean(freqHome.Healthy.Other), digits = 2)), sep = ''),pos=2,cex=1.6)
mtext('Afebrile',4,line=2)
mtext('Working adults',3,line=1)



image(f[, 1], d[1, ], surf.DEN.Child,
      zlim = range(surf.DEN.Child[surf.DEN.Child > -1]),
      xlab = '', ylab = '', main = '', xaxt = 'n', cex = 1.5)
points(freqHome.DEN.Child, durnHome.DEN.Child, cex = .25, pch = 20, col = rgb(0, 0, 0, .6))
text(8, .9,
     paste('mean duration = ', as.character(formatC(mean(durnHome.DEN.Child), digits = 2)), sep = ''),pos=2,cex=1.6)
text(8, .8,
     paste('mean frequency = ', as.character(formatC(mean(freqHome.DEN.Child), digits = 2)), sep = ''),pos=2,cex=1.6)
text(8, .7,
     paste('mean time = ', as.character(formatC(mean(durnHome.DEN.Child)*mean(freqHome.DEN.Child), digits = 2)), sep = ''),pos=2,cex=1.6)
mtext('Duration of visits (days)',2,line=3)

image(f[, 1], d[1, ], surf.DEN.Student,
      zlim = range(surf.DEN.Student[surf.DEN.Student > -1]),
      xlab = '', ylab = '', main = '', xaxt = 'n', yaxt = 'n', cex = 1.5)
points(freqHome.DEN.Student, durnHome.DEN.Student, cex = .25, pch = 20, col = rgb(0, 0, 0, .6))
text(8, .9,
     paste('mean duration = ', as.character(formatC(mean(durnHome.DEN.Student), digits = 2)), sep = ''),pos=2,cex=1.6)
     text(8, .8,
          paste('mean frequency = ', as.character(formatC(mean(freqHome.DEN.Student), digits = 2)), sep = ''),pos=2,cex=1.6)
     text(8, .7,
          paste('mean time = ', as.character(formatC(mean(durnHome.DEN.Student)*mean(freqHome.DEN.Student), digits = 2)), sep = ''),pos=2,cex=1.6)

image(f[, 1], d[1, ], surf.DEN.Homemaker,
      zlim = range(surf.DEN.Homemaker[surf.DEN.Homemaker > -1]),
      xlab = '', ylab = '', main = '', xaxt = 'n', yaxt = 'n', cex = 1.5)
points(freqHome.DEN.Homemaker, durnHome.DEN.Homemaker, cex = .25, pch = 20, col = rgb(0, 0, 0, .6))
text(8, .9,
     paste('mean duration = ', as.character(formatC(mean(durnHome.DEN.Homemaker), digits = 2)), sep = ''),pos=2,cex=1.6)
     text(8, .8,
          paste('mean frequency = ', as.character(formatC(mean(freqHome.DEN.Homemaker), digits = 2)), sep = ''),pos=2,cex=1.6)
     text(8, .7,
          paste('mean time = ', as.character(formatC(mean(durnHome.DEN.Homemaker)*mean(freqHome.DEN.Homemaker), digits = 2)), sep = ''),pos=2,cex=1.6)

image(f[, 1], d[1, ], surf.DEN.Other,
      zlim = range(surf.DEN.Other[surf.DEN.Other > -1]),
      xlab = '', ylab = '', main = '', xaxt = 'n', yaxt = 'n', cex = 1.5)
points(freqHome.DEN.Other, durnHome.DEN.Other, cex = .25, pch = 20, col = rgb(0, 0, 0, .6))
text(8, .9,
     paste('mean duration = ', as.character(formatC(mean(durnHome.DEN.Other), digits = 2)), sep = ''),pos=2,cex=1.6)
     text(8, .8,
          paste('mean frequency = ', as.character(formatC(mean(freqHome.DEN.Other), digits = 2)), sep = ''),pos=2,cex=1.6)
     text(8, .7,
          paste('mean time = ', as.character(formatC(mean(durnHome.DEN.Other)*mean(freqHome.DEN.Other), digits = 2)), sep = ''),pos=2,cex=1.6)
mtext('Febrile, DENV+',4,line=2)



image(f[, 1], d[1, ], surf.FEV.Child,
      zlim = range(surf.FEV.Child[surf.FEV.Child > -1]),
      xlab = '', ylab = '', main = '', cex = 1.5)
points(freqHome.FEV.Child, durnHome.FEV.Child, cex = .25, pch = 20, col = rgb(0, 0, 0, .6))
text(8, .9,
     paste('mean duration = ', as.character(formatC(mean(durnHome.FEV.Child), digits = 2)), sep = ''),pos=2,cex=1.6)
text(8, .8,
     paste('mean frequency = ', as.character(formatC(mean(freqHome.FEV.Child), digits = 2)), sep = ''),pos=2,cex=1.6)
text(8, .7,
     paste('mean time = ', as.character(formatC(mean(durnHome.FEV.Child)*mean(freqHome.FEV.Child), digits = 2)), sep = ''),pos=2,cex=1.6)

image(f[, 1], d[1, ], surf.FEV.Student,
      zlim = range(surf.FEV.Student[surf.FEV.Student > -1]),
      xlab = '', ylab = '', main = '', yaxt = 'n', cex = 1.5)
points(freqHome.FEV.Student, durnHome.FEV.Student, cex = .25, pch = 20, col = rgb(0, 0, 0, .6))
text(8, .9,
     paste('mean duration = ', as.character(formatC(mean(durnHome.FEV.Student), digits = 2)), sep = ''),pos=2,cex=1.6)
text(8, .8,
     paste('mean frequency = ', as.character(formatC(mean(freqHome.FEV.Student), digits = 2)), sep = ''),pos=2,cex=1.6)
text(8, .7,
     paste('mean time = ', as.character(formatC(mean(durnHome.FEV.Student)*mean(freqHome.FEV.Student), digits = 2)), sep = ''),pos=2,cex=1.6)
mtext('Frequency of visits (times per day)',1,at=8.1,line=3)

image(f[, 1], d[1, ], surf.FEV.Homemaker,
      zlim = range(surf.FEV.Homemaker[surf.FEV.Homemaker > -1]),
      xlab = '', ylab = '', main = '', yaxt = 'n', cex = 1.5)
points(freqHome.FEV.Homemaker, durnHome.FEV.Homemaker, cex = .25, pch = 20, col = rgb(0, 0, 0, .6))
text(8, .9,
     paste('mean duration = ', as.character(formatC(mean(durnHome.FEV.Homemaker), digits = 2)), sep = ''),pos=2,cex=1.6)
text(8, .8,
     paste('mean frequency = ', as.character(formatC(mean(freqHome.FEV.Homemaker), digits = 2)), sep = ''),pos=2,cex=1.6)
text(8, .7,
     paste('mean time = ', as.character(formatC(mean(durnHome.FEV.Homemaker)*mean(freqHome.FEV.Homemaker), digits = 2)), sep = ''),pos=2,cex=1.6)

image(f[, 1], d[1, ], surf.FEV.Other,
      zlim = range(surf.FEV.Other[surf.FEV.Other > -1]),
      xlab = '', ylab = '', main = '', yaxt = 'n', cex = 1.5)
points(freqHome.FEV.Other, durnHome.FEV.Other, cex = .25, pch = 20, col = rgb(0, 0, 0, .6))
text(8, .9,
     paste('mean duration = ', as.character(formatC(mean(durnHome.FEV.Other), digits = 2)), sep = ''),pos=2,cex=1.6)
text(8, .8,
     paste('mean frequency = ', as.character(formatC(mean(freqHome.FEV.Other), digits = 2)), sep = ''),pos=2,cex=1.6)
text(8, .7,
     paste('mean time = ', as.character(formatC(mean(durnHome.FEV.Other)*mean(freqHome.FEV.Other), digits = 2)), sep = ''),pos=2,cex=1.6)
mtext('Febrile, DENV-',4,line=2)


dev.off()