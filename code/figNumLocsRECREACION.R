partLoc.full = partLoc.all
partLoc.all = partLoc.all[partLoc.all$landuse_class=='RECREACION',]
numLoc.models = numLocRECREACION.models

breakpoints = seq(0,max(table(paste(partLoc.all$part_code,partLoc.all$pathogen))),1) + .5

pdf('../output/numLocsRECREACION.pdf', width = 6.5, height = 3.25)

par(mar = c(.5, 1, .5, 0), oma = c(3, 1, 2, 2))
layout(matrix(1 : 12, 3, 4, byrow = TRUE))

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Child',]
hist(
  table(paste(partLoc$part_code,partLoc$pathogen)),
  breaks = breakpoints,
  freq = F, col = 'gray', border = 'gray', main = '', xaxt = 'n', yaxt = 'n', xlim=c(0,30))
segments(
  breakpoints-.5,0,breakpoints-.5,dpois(breakpoints-.5,numLoc.models$F3.C4$Healthy.Child$par[1]),
  lwd=.5)
points(
  breakpoints-.5,
  dpois(breakpoints-.5,numLoc.models$F3.C4$Healthy.Child$par[1]),
  pch=19,cex=.2)
abline(v=mean(table(paste(partLoc$part_code,partLoc$pathogen))),lwd=2)
axis(1,at=seq(0,30,5),labels=rep(' ',7))
# text(8, .9,
#      paste('mean duration = ', as.character(formatC(mean(durnHome.Healthy.Child), digits = 2)), sep = ''),pos=2,cex=1.3)
# text(8, .8,
#      paste('mean frequency = ', as.character(formatC(mean(freqHome.Healthy.Child), digits = 2)), sep = ''),pos=2,cex=1.3)
# text(8, .7,
#      paste('mean time = ', as.character(formatC(mean(durnHome.Healthy.Child)*mean(freqHome.Healthy.Child), digits = 2)), sep = ''),pos=2,cex=1.3)
mtext('School-age children',3,line=1,cex=.5)
text(x=30,y=.05,adj=c(1,0),as.character(formatC(mean(table(paste(partLoc$part_code,partLoc$pathogen))), digits = 2)))

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Student',]
hist(
  table(paste(partLoc$part_code,partLoc$pathogen)),
  breaks = breakpoints,
  freq = F, col = 'gray', border = 'gray', main = '', xaxt = 'n', yaxt = 'n', xlim=c(0,30))
segments(
  breakpoints-.5,0,breakpoints-.5,dpois(breakpoints-.5,numLoc.models$F3.C4$Healthy.Student$par[1]),
  lwd=.5)
points(
  breakpoints-.5,
  dpois(breakpoints-.5,numLoc.models$F3.C4$Healthy.Student$par[1]),
  pch=19,cex=.2)
abline(v=mean(table(paste(partLoc$part_code,partLoc$pathogen))),lwd=2)
axis(1,at=seq(0,30,5),labels=rep(' ',7))
# text(8, .9,
#      paste('mean duration = ', as.character(formatC(mean(durnHome.Healthy.Student), digits = 2)), sep = ''),pos=2,cex=1.3)
# text(8, .8,
#      paste('mean frequency = ', as.character(formatC(mean(freqHome.Healthy.Student), digits = 2)), sep = ''),pos=2,cex=1.3)
# text(8, .7,
#      paste('mean time = ', as.character(formatC(mean(durnHome.Healthy.Student)*mean(freqHome.Healthy.Student), digits = 2)), sep = ''),pos=2,cex=1.3)
mtext('College students',3,line=1,cex=.5)
text(x=30,y=.05,adj=c(1,0),as.character(formatC(mean(table(paste(partLoc$part_code,partLoc$pathogen))), digits = 2)))

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Homemaker',]
hist(
  table(paste(partLoc$part_code,partLoc$pathogen)),
  breaks = breakpoints,
  freq = F, col = 'gray', border = 'gray', main = '', xaxt = 'n', yaxt = 'n', xlim=c(0,30))
segments(
  breakpoints-.5,0,breakpoints-.5,dpois(breakpoints-.5,numLoc.models$F3.C4$Healthy.Homemaker$par[1]),
  lwd=.5)
points(
  breakpoints-.5,
  dpois(breakpoints-.5,numLoc.models$F3.C4$Healthy.Homemaker$par[1]),
  pch=19,cex=.2)
abline(v=mean(table(paste(partLoc$part_code,partLoc$pathogen))),lwd=2)
axis(1,at=seq(0,30,5),labels=rep(' ',7))
# text(8, .9,
#      paste('mean duration = ', as.character(formatC(mean(durnHome.Healthy.Homemaker), digits = 2)), sep = ''),pos=2,cex=1.3)
# text(8, .8,
#      paste('mean frequency = ', as.character(formatC(mean(freqHome.Healthy.Homemaker), digits = 2)), sep = ''),pos=2,cex=1.3)
# text(8, .7,
#      paste('mean time = ', as.character(formatC(mean(durnHome.Healthy.Homemaker)*mean(freqHome.Healthy.Homemaker), digits = 2)), sep = ''),pos=2,cex=1.3)
mtext('Homemakers / Unemployed adults',3,line=1,cex=.5)
text(x=30,y=.05,adj=c(1,0),as.character(formatC(mean(table(paste(partLoc$part_code,partLoc$pathogen))), digits = 2)))

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Other',]
hist(
  table(paste(partLoc$part_code,partLoc$pathogen)),
  breaks = breakpoints,
  freq = F, col = 'gray', border = 'gray', main = '', xaxt = 'n', yaxt = 'n', xlim=c(0,30))
segments(
  breakpoints-.5,0,breakpoints-.5,dpois(breakpoints-.5,numLoc.models$F3.C4$Healthy.Other$par[1]),
  lwd=.5)
points(
  breakpoints-.5,
  dpois(breakpoints-.5,numLoc.models$F3.C4$Healthy.Other$par[1]),
  pch=19,cex=.2)
abline(v=mean(table(paste(partLoc$part_code,partLoc$pathogen))),lwd=2)
axis(1,at=seq(0,30,5),labels=rep(' ',7))
# text(8, .9,
#      paste('mean duration = ', as.character(formatC(mean(durnHome.Healthy.Other), digits = 2)), sep = ''),pos=2,cex=1.3)
# text(8, .8,
#      paste('mean frequency = ', as.character(formatC(mean(freqHome.Healthy.Other), digits = 2)), sep = ''),pos=2,cex=1.3)
# text(8, .7,
#      paste('mean time = ', as.character(formatC(mean(durnHome.Healthy.Other)*mean(freqHome.Healthy.Other), digits = 2)), sep = ''),pos=2,cex=1.3)
mtext('Afebrile',4,line=1,cex=.5)
mtext('Working adults',3,line=1,cex=.5)
text(x=30,y=.05,adj=c(1,0),as.character(formatC(mean(table(paste(partLoc$part_code,partLoc$pathogen))), digits = 2)))

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Child',]
hist(
  table(paste(partLoc$part_code,partLoc$pathogen)),
  breaks = breakpoints,
  freq = F, col = 'gray', border = 'gray', main = '', xaxt = 'n', yaxt = 'n', xlim=c(0,30))
segments(
  breakpoints-.5,0,breakpoints-.5,dpois(breakpoints-.5,numLoc.models$F3.C4$DEN.Child$par[1]),
  lwd=.5)
points(
  breakpoints-.5,
  dpois(breakpoints-.5,numLoc.models$F3.C4$DEN.Child$par[1]),
  pch=19,cex=.2)
abline(v=mean(table(paste(partLoc$part_code,partLoc$pathogen))),lwd=2)
axis(1,at=seq(0,30,5),labels=rep(' ',7))
# text(8, .9,
#      paste('mean duration = ', as.character(formatC(mean(durnHome.DEN.Child), digits = 2)), sep = ''),pos=2,cex=1.3)
# text(8, .8,
#      paste('mean frequency = ', as.character(formatC(mean(freqHome.DEN.Child), digits = 2)), sep = ''),pos=2,cex=1.3)
# text(8, .7,
#      paste('mean time = ', as.character(formatC(mean(durnHome.DEN.Child)*mean(freqHome.DEN.Child), digits = 2)), sep = ''),pos=2,cex=1.3)
mtext('Frequency',2,line=1,cex=.5)
text(x=30,y=.05,adj=c(1,0),as.character(formatC(mean(table(paste(partLoc$part_code,partLoc$pathogen))), digits = 2)))

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Student',]
hist(
  table(paste(partLoc$part_code,partLoc$pathogen)),
  breaks = breakpoints,
  freq = F, col = 'gray', border = 'gray', main = '', xaxt = 'n', yaxt = 'n', xlim=c(0,30))
segments(
  breakpoints-.5,0,breakpoints-.5,dpois(breakpoints-.5,numLoc.models$F3.C4$DEN.Student$par[1]),
  lwd=.5)
points(
  breakpoints-.5,
  dpois(breakpoints-.5,numLoc.models$F3.C4$DEN.Student$par[1]),
  pch=19,cex=.2)
abline(v=mean(table(paste(partLoc$part_code,partLoc$pathogen))),lwd=2)
axis(1,at=seq(0,30,5),labels=rep(' ',7))
# text(8, .9,
#      paste('mean duration = ', as.character(formatC(mean(durnHome.DEN.Student), digits = 2)), sep = ''),pos=2,cex=1.3)
# text(8, .8,
#      paste('mean frequency = ', as.character(formatC(mean(freqHome.DEN.Student), digits = 2)), sep = ''),pos=2,cex=1.3)
# text(8, .7,
#      paste('mean time = ', as.character(formatC(mean(durnHome.DEN.Student)*mean(freqHome.DEN.Student), digits = 2)), sep = ''),pos=2,cex=1.3)
text(x=30,y=.05,adj=c(1,0),as.character(formatC(mean(table(paste(partLoc$part_code,partLoc$pathogen))), digits = 2)))

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Homemaker',]
hist(
  table(paste(partLoc$part_code,partLoc$pathogen)),
  breaks = breakpoints,
  freq = F, col = 'gray', border = 'gray', main = '', xaxt = 'n', yaxt = 'n', xlim=c(0,30))
segments(
  breakpoints-.5,0,breakpoints-.5,dpois(breakpoints-.5,numLoc.models$F3.C4$DEN.Homemaker$par[1]),
  lwd=.5)
points(
  breakpoints-.5,
  dpois(breakpoints-.5,numLoc.models$F3.C4$DEN.Homemaker$par[1]),
  pch=19,cex=.2)
abline(v=mean(table(paste(partLoc$part_code,partLoc$pathogen))),lwd=2)
axis(1,at=seq(0,30,5),labels=rep(' ',7))
# text(8, .9,
#      paste('mean duration = ', as.character(formatC(mean(durnHome.DEN.Homemaker), digits = 2)), sep = ''),pos=2,cex=1.3)
# text(8, .8,
#      paste('mean frequency = ', as.character(formatC(mean(freqHome.DEN.Homemaker), digits = 2)), sep = ''),pos=2,cex=1.3)
# text(8, .7,
#      paste('mean time = ', as.character(formatC(mean(durnHome.DEN.Homemaker)*mean(freqHome.DEN.Homemaker), digits = 2)), sep = ''),pos=2,cex=1.3)
text(x=30,y=.05,adj=c(1,0),as.character(formatC(mean(table(paste(partLoc$part_code,partLoc$pathogen))), digits = 2)))

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Other',]
hist(
  table(paste(partLoc$part_code,partLoc$pathogen)),
  breaks = breakpoints,
  freq = F, col = 'gray', border = 'gray', main = '', xaxt = 'n', yaxt = 'n', xlim=c(0,30))
segments(
  breakpoints-.5,0,breakpoints-.5,dpois(breakpoints-.5,numLoc.models$F3.C4$DEN.Other$par[1]),
  lwd=.5)
points(
  breakpoints-.5,
  dpois(breakpoints-.5,numLoc.models$F3.C4$DEN.Other$par[1]),
  pch=19,cex=.2)
abline(v=mean(table(paste(partLoc$part_code,partLoc$pathogen))),lwd=2)
# text(8, .9,
#      paste('mean duration = ', as.character(formatC(mean(durnHome.DEN.Other), digits = 2)), sep = ''),pos=2,cex=1.3)
# text(8, .8,
#      paste('mean frequency = ', as.character(formatC(mean(freqHome.DEN.Other), digits = 2)), sep = ''),pos=2,cex=1.3)
# text(8, .7,
#      paste('mean time = ', as.character(formatC(mean(durnHome.DEN.Other)*mean(freqHome.DEN.Other), digits = 2)), sep = ''),pos=2,cex=1.3)
mtext('Febrile, DENV+',4,line=1,cex=.5)
axis(1,at=seq(0,30,5),labels=rep(' ',7))
text(x=30,y=.05,adj=c(1,0),as.character(formatC(mean(table(paste(partLoc$part_code,partLoc$pathogen))), digits = 2)))

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Child',]
hist(
  table(paste(partLoc$part_code,partLoc$pathogen)),
  breaks = breakpoints,
  freq = F, col = 'gray', border = 'gray', main = '', xaxt = 'n', yaxt = 'n', xlim=c(0,30))
segments(
  breakpoints-.5,0,breakpoints-.5,dpois(breakpoints-.5,numLoc.models$F3.C4$FEV.Child$par[1]),
  lwd=.5)
points(
  breakpoints-.5,
  dpois(breakpoints-.5,numLoc.models$F3.C4$FEV.Child$par[1]),
  pch=19,cex=.2)
abline(v=mean(table(paste(partLoc$part_code,partLoc$pathogen))),lwd=2)
axis(1,cex.axis=.7)
# text(8, .9,
#      paste('mean duration = ', as.character(formatC(mean(durnHome.FEV.Child), digits = 2)), sep = ''),pos=2,cex=1.3)
# text(8, .8,
#      paste('mean frequency = ', as.character(formatC(mean(freqHome.FEV.Child), digits = 2)), sep = ''),pos=2,cex=1.3)
# text(8, .7,
#      paste('mean time = ', as.character(formatC(mean(durnHome.FEV.Child)*mean(freqHome.FEV.Child), digits = 2)), sep = ''),pos=2,cex=1.3)
text(x=30,y=.05,adj=c(1,0),as.character(formatC(mean(table(paste(partLoc$part_code,partLoc$pathogen))), digits = 2)))

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Student',]
hist(
  table(paste(partLoc$part_code,partLoc$pathogen)),
  breaks = breakpoints,
  freq = F, col = 'gray', border = 'gray', main = '', xaxt = 'n', yaxt = 'n', xlim=c(0,30))
segments(
  breakpoints-.5,0,breakpoints-.5,dpois(breakpoints-.5,numLoc.models$F3.C4$FEV.Student$par[1]),
  lwd=.5)
points(
  breakpoints-.5,
  dpois(breakpoints-.5,numLoc.models$F3.C4$FEV.Student$par[1]),
  pch=19,cex=.2)
abline(v=mean(table(paste(partLoc$part_code,partLoc$pathogen))),lwd=2)
axis(1,cex.axis=.7)
# text(8, .9,
#      paste('mean duration = ', as.character(formatC(mean(durnHome.FEV.Student), digits = 2)), sep = ''),pos=2,cex=1.3)
# text(8, .8,
#      paste('mean frequency = ', as.character(formatC(mean(freqHome.FEV.Student), digits = 2)), sep = ''),pos=2,cex=1.3)
# text(8, .7,
#      paste('mean time = ', as.character(formatC(mean(durnHome.FEV.Student)*mean(freqHome.FEV.Student), digits = 2)), sep = ''),pos=2,cex=1.3)
mtext('Number of locations visited',1,at=32,line=2.5,cex=.5)
text(x=30,y=.05,adj=c(1,0),as.character(formatC(mean(table(paste(partLoc$part_code,partLoc$pathogen))), digits = 2)))

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Homemaker',]
hist(
  table(paste(partLoc$part_code,partLoc$pathogen)),
  breaks = breakpoints,
  freq = F, col = 'gray', border = 'gray', main = '', xaxt = 'n', yaxt = 'n', xlim=c(0,30))
segments(
  breakpoints-.5,0,breakpoints-.5,dpois(breakpoints-.5,numLoc.models$F3.C4$FEV.Homemaker$par[1]),
  lwd=.5)
points(
  breakpoints-.5,
  dpois(breakpoints-.5,numLoc.models$F3.C4$FEV.Homemaker$par[1]),
  pch=19,cex=.2)
abline(v=mean(table(paste(partLoc$part_code,partLoc$pathogen))),lwd=2)
axis(1,cex.axis=.7)
# text(8, .9,
#      paste('mean duration = ', as.character(formatC(mean(durnHome.FEV.Homemaker), digits = 2)), sep = ''),pos=2,cex=1.3)
# text(8, .8,
#      paste('mean frequency = ', as.character(formatC(mean(freqHome.FEV.Homemaker), digits = 2)), sep = ''),pos=2,cex=1.3)
# text(8, .7,
#      paste('mean time = ', as.character(formatC(mean(durnHome.FEV.Homemaker)*mean(freqHome.FEV.Homemaker), digits = 2)), sep = ''),pos=2,cex=1.3)
text(x=30,y=.05,adj=c(1,0),as.character(formatC(mean(table(paste(partLoc$part_code,partLoc$pathogen))), digits = 2)))

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Other',]
hist(
  table(paste(partLoc$part_code,partLoc$pathogen)),
  breaks = breakpoints,
  freq = F, col = 'gray', border = 'gray', main = '', xaxt = 'n', yaxt = 'n', xlim=c(0,30))
segments(
  breakpoints-.5,0,breakpoints-.5,dpois(breakpoints-.5,numLoc.models$F3.C4$FEV.Other$par[1]),
  lwd=.5)
points(
  breakpoints-.5,
  dpois(breakpoints-.5,numLoc.models$F3.C4$FEV.Other$par[1]),
  pch=19,cex=.2)
abline(v=mean(table(paste(partLoc$part_code,partLoc$pathogen))),lwd=2)
axis(1,cex.axis=.7)
# text(8, .9,
#      paste('mean duration = ', as.character(formatC(mean(durnHome.FEV.Other), digits = 2)), sep = ''),pos=2,cex=1.3)
# text(8, .8,
#      paste('mean frequency = ', as.character(formatC(mean(freqHome.FEV.Other), digits = 2)), sep = ''),pos=2,cex=1.3)
# text(8, .7,
#      paste('mean time = ', as.character(formatC(mean(durnHome.FEV.Other)*mean(freqHome.FEV.Other), digits = 2)), sep = ''),pos=2,cex=1.3)
mtext('Febrile, DENV-',4,line=1,cex=.5)
text(x=30,y=.05,adj=c(1,0),as.character(formatC(mean(table(paste(partLoc$part_code,partLoc$pathogen))), digits = 2)))


dev.off()

partLoc.all = partLoc.full