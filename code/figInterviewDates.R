pdf('../output/dates_interviews.pdf',width=6.5,height=6.5)

layout(matrix(1:6,3,2))

par(mar=rep(1,4),oma=c(3,3,2,2))

hist(partHome$interview_date[partHome$pathogen=='NA'],breaks=as.Date(paste(rep(2008:2013,c(6,12,12,12,12,8)),c(7:12,1:12,1:12,1:12,1:12,1:8),1,sep='-')),freq=T,
     xlab='',ylab='',main='',col='gray',xaxt='n')
abline(v=as.Date(paste(2008:2013,1,1,sep='-')),lty=2)
axis(1,at=as.Date(paste(2008:2013,7,1,sep='-')),labels=2008:2013)
axis(2,las=1)
mtext('Time at home',3,line=1.5)

hist(partHome$interview_date[partHome$pathogen=='DEN'],breaks=as.Date(paste(rep(2008:2013,c(6,12,12,12,12,8)),c(7:12,1:12,1:12,1:12,1:12,1:8),1,sep='-')),freq=T,
     xlab='',ylab='',main='',col='gray',xaxt='n')
abline(v=as.Date(paste(2008:2013,1,1,sep='-')),lty=2)
axis(1,at=as.Date(paste(2008:2013,7,1,sep='-')),labels=2008:2013)
axis(2,las=1)
mtext('Number of interviews',2,line=2.5)

hist(partHome$interview_date[partHome$pathogen=='FEV'],breaks=as.Date(paste(rep(2008:2013,c(6,12,12,12,12,8)),c(7:12,1:12,1:12,1:12,1:12,1:8),1,sep='-')),freq=T,
     xlab='',ylab='',main='',col='gray',xaxt='n')
abline(v=as.Date(paste(2008:2013,1,1,sep='-')),lty=2)
axis(1,at=as.Date(paste(2008:2013,7,1,sep='-')),labels=2008:2013)
axis(2,las=1)


hist(participants.notHome$interview_date[participants.notHome$pathogen=='NA'],breaks=as.Date(paste(rep(2008:2013,c(6,12,12,12,12,8)),c(7:12,1:12,1:12,1:12,1:12,1:8),1,sep='-')),freq=T,
     xlab='',ylab='',main='',col='gray',xaxt='n')
abline(v=as.Date(paste(2008:2013,1,1,sep='-')),lty=2)
axis(1,at=as.Date(paste(2008:2013,7,1,sep='-')),labels=2008:2013)
axis(2,las=1)
mtext('Time elsewhere',3,line=1.5)
mtext('Afebrile',4,line=1.5)

hist(participants.notHome$interview_date[participants.notHome$pathogen=='DEN'],breaks=as.Date(paste(rep(2008:2013,c(6,12,12,12,12,8)),c(7:12,1:12,1:12,1:12,1:12,1:8),1,sep='-')),freq=T,
     xlab='',ylab='',main='',col='gray',xaxt='n')
abline(v=as.Date(paste(2008:2013,1,1,sep='-')),lty=2)
axis(1,at=as.Date(paste(2008:2013,7,1,sep='-')),labels=2008:2013)
axis(2,las=1)
mtext('Febrile, DENV+',4,line=1.5)

hist(participants.notHome$interview_date[participants.notHome$pathogen=='FEV'],breaks=as.Date(paste(rep(2008:2013,c(6,12,12,12,12,8)),c(7:12,1:12,1:12,1:12,1:12,1:8),1,sep='-')),freq=T,
     xlab='',ylab='',main='',col='gray',xaxt='n')
abline(v=as.Date(paste(2008:2013,1,1,sep='-')),lty=2)
axis(1,at=as.Date(paste(2008:2013,7,1,sep='-')),labels=2008:2013)
axis(2,las=1)
mtext('Febrile, DENV-',4,line=1.5)
mtext('Date',1,line=3,at=as.Date('2007-12-20'))

dev.off()

