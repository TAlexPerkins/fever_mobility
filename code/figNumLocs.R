breakpoints = seq(0,max(table(paste(partLoc.all$part_code,partLoc.all$pathogen))),1) + .5

pdf('../output/numLocs.pdf', width = 6.5, height = 3.25)

par(mar = c(.5, 1, .5, 0), oma = c(3, 1, 2, 2))
layout(matrix(1 : 12, 3, 4, byrow = TRUE))

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Child',]
h = hist(
  table(paste(partLoc$part_code,partLoc$pathogen)),
  breaks = breakpoints,
  freq = F, col = 'gray', border = 'gray', main = '', xaxt = 'n', yaxt = 'n', xlim=c(0,30))
segments(
  breakpoints-.5,0,breakpoints-.5,dnbinom(breakpoints-.5,numLoc.models$F3.C4$Healthy.Child$par[1],numLoc.models$F3.C4$Healthy.Child$par[2]),
  lwd=.5)
points(
  breakpoints-.5,
  dnbinom(breakpoints-.5,numLoc.models$F3.C4$Healthy.Child$par[1],numLoc.models$F3.C4$Healthy.Child$par[2]),
  pch=19,cex=.2)
abline(v=mean(table(paste(partLoc$part_code,partLoc$pathogen))),lwd=2)
axis(1,at=seq(0,30,5),labels=rep(' ',7))
text(30, .95*max(h$density),
     paste('mean = ', formatC(mean(table(paste(partLoc$part_code,partLoc$pathogen))),digits=2,width=3), sep = ''),pos=2,cex=.7)
text(30, .8*max(h$density),
     paste('size = ', formatC(numLoc.models$F3.C4$Healthy.Child$par[1], digits = 2), sep = ''),pos=2,cex=.7)
text(30, .65*max(h$density),
     paste('prob = ', formatC(numLoc.models$F3.C4$Healthy.Child$par[2], digits = 2), sep = ''),pos=2,cex=.7)

mtext('School-age children',3,line=1,cex=.5)

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Student',]
h = hist(
  table(paste(partLoc$part_code,partLoc$pathogen)),
  breaks = breakpoints,
  freq = F, col = 'gray', border = 'gray', main = '', xaxt = 'n', yaxt = 'n', xlim=c(0,30))
segments(
  breakpoints-.5,0,breakpoints-.5,dnbinom(breakpoints-.5,numLoc.models$F3.C4$Healthy.Student$par[1],numLoc.models$F3.C4$Healthy.Student$par[2]),
  lwd=.5)
points(
  breakpoints-.5,
  dnbinom(breakpoints-.5,numLoc.models$F3.C4$Healthy.Student$par[1],numLoc.models$F3.C4$Healthy.Student$par[2]),
  pch=19,cex=.2)
abline(v=mean(table(paste(partLoc$part_code,partLoc$pathogen))),lwd=2)
axis(1,at=seq(0,30,5),labels=rep(' ',7))
text(30, .95*max(h$density),
     paste('mean = ', formatC(mean(table(paste(partLoc$part_code,partLoc$pathogen))),digits=2,width=3), sep = ''),pos=2,cex=.7)
text(30, .8*max(h$density),
     paste('size = ', formatC(numLoc.models$F3.C4$Healthy.Student$par[1], digits = 2), sep = ''),pos=2,cex=.7)
text(30, .65*max(h$density),
     paste('prob = ', formatC(numLoc.models$F3.C4$Healthy.Student$par[2], digits = 2), sep = ''),pos=2,cex=.7)
mtext('College students',3,line=1,cex=.5)

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Homemaker',]
h = hist(
  table(paste(partLoc$part_code,partLoc$pathogen)),
  breaks = breakpoints,
  freq = F, col = 'gray', border = 'gray', main = '', xaxt = 'n', yaxt = 'n', xlim=c(0,30))
segments(
  breakpoints-.5,0,breakpoints-.5,dnbinom(breakpoints-.5,numLoc.models$F3.C4$Healthy.Homemaker$par[1],numLoc.models$F3.C4$Healthy.Homemaker$par[2]),
  lwd=.5)
points(
  breakpoints-.5,
  dnbinom(breakpoints-.5,numLoc.models$F3.C4$Healthy.Homemaker$par[1],numLoc.models$F3.C4$Healthy.Homemaker$par[2]),
  pch=19,cex=.2)
abline(v=mean(table(paste(partLoc$part_code,partLoc$pathogen))),lwd=2)
axis(1,at=seq(0,30,5),labels=rep(' ',7))
text(30, .95*max(h$density),
     paste('mean = ', formatC(mean(table(paste(partLoc$part_code,partLoc$pathogen))),digits=2,width=3), sep = ''),pos=2,cex=.7)
text(30, .8*max(h$density),
     paste('size = ', formatC(numLoc.models$F3.C4$Healthy.Homemaker$par[1], digits = 2), sep = ''),pos=2,cex=.7)
text(30, .65*max(h$density),
     paste('prob = ', formatC(numLoc.models$F3.C4$Healthy.Homemaker$par[2], digits = 2), sep = ''),pos=2,cex=.7)
mtext('Homemakers / Unemployed adults',3,line=1,cex=.5)

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Other',]
h = hist(
  table(paste(partLoc$part_code,partLoc$pathogen)),
  breaks = breakpoints,
  freq = F, col = 'gray', border = 'gray', main = '', xaxt = 'n', yaxt = 'n', xlim=c(0,30))
segments(
  breakpoints-.5,0,breakpoints-.5,dnbinom(breakpoints-.5,numLoc.models$F3.C4$Healthy.Other$par[1],numLoc.models$F3.C4$Healthy.Other$par[2]),
  lwd=.5)
points(
  breakpoints-.5,
  dnbinom(breakpoints-.5,numLoc.models$F3.C4$Healthy.Other$par[1],numLoc.models$F3.C4$Healthy.Other$par[2]),
  pch=19,cex=.2)
abline(v=mean(table(paste(partLoc$part_code,partLoc$pathogen))),lwd=2)
axis(1,at=seq(0,30,5),labels=rep(' ',7))
text(30, .95*max(h$density),
     paste('mean = ', formatC(mean(table(paste(partLoc$part_code,partLoc$pathogen))),digits=2,width=3), sep = ''),pos=2,cex=.7)
text(30, .8*max(h$density),
     paste('size = ', formatC(numLoc.models$F3.C4$Healthy.Other$par[1], digits = 2), sep = ''),pos=2,cex=.7)
text(30, .65*max(h$density),
     paste('prob = ', formatC(numLoc.models$F3.C4$Healthy.Other$par[2], digits = 2), sep = ''),pos=2,cex=.7)
mtext('Afebrile',4,line=1,cex=.5)
mtext('Working adults',3,line=1,cex=.5)


partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Child',]
h = hist(
  table(paste(partLoc$part_code,partLoc$pathogen)),
  breaks = breakpoints,
  freq = F, col = 'gray', border = 'gray', main = '', xaxt = 'n', yaxt = 'n', xlim=c(0,30))
segments(
  breakpoints-.5,0,breakpoints-.5,dnbinom(breakpoints-.5,numLoc.models$F3.C4$DEN.Child$par[1],numLoc.models$F3.C4$DEN.Child$par[2]),
  lwd=.5)
points(
  breakpoints-.5,
  dnbinom(breakpoints-.5,numLoc.models$F3.C4$DEN.Child$par[1],numLoc.models$F3.C4$DEN.Child$par[2]),
  pch=19,cex=.2)
abline(v=mean(table(paste(partLoc$part_code,partLoc$pathogen))),lwd=2)
axis(1,at=seq(0,30,5),labels=rep(' ',7))
text(30, .95*max(h$density),
     paste('mean = ', formatC(mean(table(paste(partLoc$part_code,partLoc$pathogen))),digits=2,width=3), sep = ''),pos=2,cex=.7)
text(30, .8*max(h$density),
     paste('size = ', formatC(numLoc.models$F3.C4$DEN.Child$par[1], digits = 2), sep = ''),pos=2,cex=.7)
text(30, .65*max(h$density),
     paste('prob = ', formatC(numLoc.models$F3.C4$DEN.Child$par[2], digits = 2), sep = ''),pos=2,cex=.7)
mtext('Frequency',2,line=1,cex=.5)

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Student',]
h = hist(
  table(paste(partLoc$part_code,partLoc$pathogen)),
  breaks = breakpoints,
  freq = F, col = 'gray', border = 'gray', main = '', xaxt = 'n', yaxt = 'n', xlim=c(0,30))
segments(
  breakpoints-.5,0,breakpoints-.5,dnbinom(breakpoints-.5,numLoc.models$F3.C4$DEN.Student$par[1],numLoc.models$F3.C4$DEN.Student$par[2]),
  lwd=.5)
points(
  breakpoints-.5,
  dnbinom(breakpoints-.5,numLoc.models$F3.C4$DEN.Student$par[1],numLoc.models$F3.C4$DEN.Student$par[2]),
  pch=19,cex=.2)
abline(v=mean(table(paste(partLoc$part_code,partLoc$pathogen))),lwd=2)
axis(1,at=seq(0,30,5),labels=rep(' ',7))
text(30, .95*max(h$density),
     paste('mean = ', formatC(mean(table(paste(partLoc$part_code,partLoc$pathogen))),digits=2,width=3), sep = ''),pos=2,cex=.7)
text(30, .8*max(h$density),
     paste('size = ', formatC(numLoc.models$F3.C4$DEN.Student$par[1], digits = 2), sep = ''),pos=2,cex=.7)
text(30, .65*max(h$density),
     paste('prob = ', formatC(numLoc.models$F3.C4$DEN.Student$par[2], digits = 2), sep = ''),pos=2,cex=.7)

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Homemaker',]
h = hist(
  table(paste(partLoc$part_code,partLoc$pathogen)),
  breaks = breakpoints,
  freq = F, col = 'gray', border = 'gray', main = '', xaxt = 'n', yaxt = 'n', xlim=c(0,30))
segments(
  breakpoints-.5,0,breakpoints-.5,dnbinom(breakpoints-.5,numLoc.models$F3.C4$DEN.Homemaker$par[1],numLoc.models$F3.C4$DEN.Homemaker$par[2]),
  lwd=.5)
points(
  breakpoints-.5,
  dnbinom(breakpoints-.5,numLoc.models$F3.C4$DEN.Homemaker$par[1],numLoc.models$F3.C4$DEN.Homemaker$par[2]),
  pch=19,cex=.2)
abline(v=mean(table(paste(partLoc$part_code,partLoc$pathogen))),lwd=2)
axis(1,at=seq(0,30,5),labels=rep(' ',7))
text(30, .95*max(h$density),
     paste('mean = ', formatC(mean(table(paste(partLoc$part_code,partLoc$pathogen))),digits=2,width=3), sep = ''),pos=2,cex=.7)
text(30, .8*max(h$density),
     paste('size = ', formatC(numLoc.models$F3.C4$DEN.Homemaker$par[1], digits = 2), sep = ''),pos=2,cex=.7)
text(30, .65*max(h$density),
     paste('prob = ', formatC(numLoc.models$F3.C4$DEN.Homemaker$par[2], digits = 2), sep = ''),pos=2,cex=.7)

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Other',]
h = hist(
  table(paste(partLoc$part_code,partLoc$pathogen)),
  breaks = breakpoints,
  freq = F, col = 'gray', border = 'gray', main = '', xaxt = 'n', yaxt = 'n', xlim=c(0,30))
segments(
  breakpoints-.5,0,breakpoints-.5,dnbinom(breakpoints-.5,numLoc.models$F3.C4$DEN.Other$par[1],numLoc.models$F3.C4$DEN.Other$par[2]),
  lwd=.5)
points(
  breakpoints-.5,
  dnbinom(breakpoints-.5,numLoc.models$F3.C4$DEN.Other$par[1],numLoc.models$F3.C4$DEN.Other$par[2]),
  pch=19,cex=.2)
abline(v=mean(table(paste(partLoc$part_code,partLoc$pathogen))),lwd=2)
text(30, .95*max(h$density),
     paste('mean = ', formatC(mean(table(paste(partLoc$part_code,partLoc$pathogen))),digits=2,width=3), sep = ''),pos=2,cex=.7)
text(30, .8*max(h$density),
     paste('size = ', formatC(numLoc.models$F3.C4$DEN.Other$par[1], digits = 2), sep = ''),pos=2,cex=.7)
text(30, .65*max(h$density),
     paste('prob = ', formatC(numLoc.models$F3.C4$DEN.Other$par[2], digits = 2), sep = ''),pos=2,cex=.7)
mtext('Febrile, DENV+',4,line=1,cex=.5)
axis(1,at=seq(0,30,5),labels=rep(' ',7))


partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Child',]
h = hist(
  table(paste(partLoc$part_code,partLoc$pathogen)),
  breaks = breakpoints,
  freq = F, col = 'gray', border = 'gray', main = '', xaxt = 'n', yaxt = 'n', xlim=c(0,30))
segments(
  breakpoints-.5,0,breakpoints-.5,dnbinom(breakpoints-.5,numLoc.models$F3.C4$FEV.Child$par[1],numLoc.models$F3.C4$FEV.Child$par[2]),
  lwd=.5)
points(
  breakpoints-.5,
  dnbinom(breakpoints-.5,numLoc.models$F3.C4$FEV.Child$par[1],numLoc.models$F3.C4$FEV.Child$par[2]),
  pch=19,cex=.2)
abline(v=mean(table(paste(partLoc$part_code,partLoc$pathogen))),lwd=2)
axis(1,cex.axis=.7)
text(30, .95*max(h$density),
     paste('mean = ', formatC(mean(table(paste(partLoc$part_code,partLoc$pathogen))),digits=2,width=3), sep = ''),pos=2,cex=.7)
text(30, .8*max(h$density),
     paste('size = ', formatC(numLoc.models$F3.C4$FEV.Child$par[1], digits = 2), sep = ''),pos=2,cex=.7)
text(30, .65*max(h$density),
     paste('prob = ', formatC(numLoc.models$F3.C4$FEV.Child$par[2], digits = 2), sep = ''),pos=2,cex=.7)

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Student',]
h = hist(
  table(paste(partLoc$part_code,partLoc$pathogen)),
  breaks = breakpoints,
  freq = F, col = 'gray', border = 'gray', main = '', xaxt = 'n', yaxt = 'n', xlim=c(0,30))
segments(
  breakpoints-.5,0,breakpoints-.5,dnbinom(breakpoints-.5,numLoc.models$F3.C4$FEV.Student$par[1],numLoc.models$F3.C4$FEV.Student$par[2]),
  lwd=.5)
points(
  breakpoints-.5,
  dnbinom(breakpoints-.5,numLoc.models$F3.C4$FEV.Student$par[1],numLoc.models$F3.C4$FEV.Student$par[2]),
  pch=19,cex=.2)
abline(v=mean(table(paste(partLoc$part_code,partLoc$pathogen))),lwd=2)
axis(1,cex.axis=.7)
text(30, .95*max(h$density),
     paste('mean = ', formatC(mean(table(paste(partLoc$part_code,partLoc$pathogen))),digits=2,width=3), sep = ''),pos=2,cex=.7)
text(30, .8*max(h$density),
     paste('size = ', formatC(numLoc.models$F3.C4$FEV.Student$par[1], digits = 2), sep = ''),pos=2,cex=.7)
text(30, .65*max(h$density),
     paste('prob = ', formatC(numLoc.models$F3.C4$FEV.Student$par[2], digits = 2), sep = ''),pos=2,cex=.7)
mtext('Number of locations visited',1,at=32,line=2.5,cex=.5)

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Homemaker',]
h = hist(
  table(paste(partLoc$part_code,partLoc$pathogen)),
  breaks = breakpoints,
  freq = F, col = 'gray', border = 'gray', main = '', xaxt = 'n', yaxt = 'n', xlim=c(0,30))
segments(
  breakpoints-.5,0,breakpoints-.5,dnbinom(breakpoints-.5,numLoc.models$F3.C4$FEV.Homemaker$par[1],numLoc.models$F3.C4$FEV.Homemaker$par[2]),
  lwd=.5)
points(
  breakpoints-.5,
  dnbinom(breakpoints-.5,numLoc.models$F3.C4$FEV.Homemaker$par[1],numLoc.models$F3.C4$FEV.Homemaker$par[2]),
  pch=19,cex=.2)
abline(v=mean(table(paste(partLoc$part_code,partLoc$pathogen))),lwd=2)
axis(1,cex.axis=.7)
text(30, .95*max(h$density),
     paste('mean = ', formatC(mean(table(paste(partLoc$part_code,partLoc$pathogen))),digits=2,width=3), sep = ''),pos=2,cex=.7)
text(30, .8*max(h$density),
     paste('size = ', formatC(numLoc.models$F3.C4$FEV.Homemaker$par[1], digits = 2), sep = ''),pos=2,cex=.7)
text(30, .65*max(h$density),
     paste('prob = ', formatC(numLoc.models$F3.C4$FEV.Homemaker$par[2], digits = 2), sep = ''),pos=2,cex=.7)

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Other',]
h = hist(
  table(paste(partLoc$part_code,partLoc$pathogen)),
  breaks = breakpoints,
  freq = F, col = 'gray', border = 'gray', main = '', xaxt = 'n', yaxt = 'n', xlim=c(0,30))
segments(
  breakpoints-.5,0,breakpoints-.5,dnbinom(breakpoints-.5,numLoc.models$F3.C4$FEV.Other$par[1],numLoc.models$F3.C4$FEV.Other$par[2]),
  lwd=.5)
points(
  breakpoints-.5,
  dnbinom(breakpoints-.5,numLoc.models$F3.C4$FEV.Other$par[1],numLoc.models$F3.C4$FEV.Other$par[2]),
  pch=19,cex=.2)
abline(v=mean(table(paste(partLoc$part_code,partLoc$pathogen))),lwd=2)
axis(1,cex.axis=.7)
text(30, .95*max(h$density),
     paste('mean = ', formatC(mean(table(paste(partLoc$part_code,partLoc$pathogen))),digits=2,width=3), sep = ''),pos=2,cex=.7)
text(30, .8*max(h$density),
     paste('size = ', formatC(numLoc.models$F3.C4$FEV.Other$par[1], digits = 2), sep = ''),pos=2,cex=.7)
text(30, .65*max(h$density),
     paste('prob = ', formatC(numLoc.models$F3.C4$FEV.Other$par[2], digits = 2), sep = ''),pos=2,cex=.7)
mtext('Febrile, DENV-',4,line=1,cex=.5)


dev.off()