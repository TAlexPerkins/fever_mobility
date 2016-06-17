# This code estimates parameters for different specifications of the model
# using data from retrospective interviews.


# load packages and functions
if(!require(bbmle)){install.packages('bbmle'); library(bbmle)}
if(!require(klaR)){install.packages('klaR'); library(klaR)}
if(!require(mgcv)){install.packages('mgcv'); library(mgcv)}
if(!require(mvtnorm)){install.packages('mvtnorm'); library(mvtnorm)}




################
# time at home #
################

# load data
load('../data/partHome.RData')


# fit and compare models of time spent at home and make figure
source('estimateTimeHome.R')
source('figTimeHome.R')




####################
# time not at home #
####################

# load data
load('../data/partLoc.RData')


# fit and compare models of number of locations and make figure
source('estimateNumLocs.R')
source('figNumLocs.R')


# fit and compare models of time spent at various location types and make figures
source('estimateNumHOUSE.R')
source('figNumLocsHOUSE.R')

source('estimateNumTIENDAS.R')
source('figNumLocsTIENDAS.R')

source('estimateNumRECREACION.R')
source('figNumLocsRECREACION.R')

source('estimateNumEDUCACION.R')
source('figNumLocsEDUCACION.R')

source('estimateNumSALUD.R')
source('figNumLocsSALUD.R')

source('estimateNumIGLESIAS.R')
source('figNumLocsIGLESIAS.R')

source('estimateNumINSTITUCIONES.R')
source('figNumLocsINSTITUCIONES.R')

source('estimateNumOTROS.R')
source('figNumLocsOTROS.R')


# fit and compare models of effect of distance from home and make figure
# source('estimateWhere.R')
# source('figWhere.R')


# fit and compare models of time spent at various location types and make figures
source('estimateTimeHOUSE.R')
source('figTimeHOUSE.R')

source('estimateTimeTIENDAS.R')
source('figTimeTIENDAS.R')

source('estimateTimeRECREACION.R')
source('figTimeRECREACION.R')

source('estimateTimeEDUCACION.R')
source('figTimeEDUCACION.R')

source('estimateTimeSALUD.R')
source('figTimeSALUD.R')

source('estimateTimeIGLESIAS.R')
source('figTimeIGLESIAS.R')

source('estimateTimeINSTITUCIONES.R')
source('figTimeINSTITUCIONES.R')

source('estimateTimeOTROS.R')
source('figTimeOTROS.R')



############
# metadata #
############

# table of number of time at home participants by fever status and demographic category
table(partHome$pathogen,partHome$category)


# table of number of time not home participants by fever status and demographic category
participants.notHome = match(
  unique(paste(partLoc.all$part_code,partLoc.all$pathogen)),
  paste(partLoc.all$part_code,partLoc.all$pathogen))

participants.notHome = cbind(
  part_code = partLoc.all$part_code[participants.notHome],
  pathogen = partLoc.all$pathogen[participants.notHome],
  cateogry = partLoc.all$category[participants.notHome],
  interview_id = partLoc.all$interview_id[participants.notHome],
  age = partLoc.all$age[participants.notHome],
  sex = partLoc.all$sex[participants.notHome])
# participants.notHome[participants.notHome[,'sex']=='1','sex'] = 'F'
# participants.notHome[participants.notHome[,'sex']=='2','sex'] = 'M'

table(participants.notHome[,2],participants.notHome[,3])


# total numbers of participants together and for each interview type

# 776
ints.home = parts.home = nrow(partHome)

# 988
ints.notHome = length(unique(paste(partLoc.all$part_code,partLoc.all$pathogen)))

# 926
parts.notHome = length(unique(partLoc.all$part_code))

# 764
parts.intersect = length(intersect(
  unique(partHome$part_code),
  unique(partLoc.all$part_code)))

# 926
parts.union = length(union(
  unique(partHome$part_code),
  unique(partLoc.all$part_code)))

# analyze interview dates for interviews about time at home
# partHome = cbind(partHome, interview_date = as.Date(Interviews$interview_date[which(Interviews$interview_id %in% partHome$interview_id)]))
# participants.notHome = data.frame(participants.notHome)
# participants.notHome = cbind(participants.notHome, interview_date = as.Date(Interviews$interview_date[which(Interviews$interview_id %in% participants.notHome$interview_id)]))

# plot the dates of interviews stratified by interviews of time at home vs elsewhere and by fever status
# source('figInterviewDates.R')
