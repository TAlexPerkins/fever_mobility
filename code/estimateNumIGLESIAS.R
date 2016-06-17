liknumLocIGLESIAS = function(){
  numLocIGLESIAS.data = table(paste(partLoc$part_code,partLoc$pathogen))
  if(sum(names(numLocIGLESIAS.data)=='NA NA')){
    numLocIGLESIAS.data = 
      numLocIGLESIAS.data[-which(names(numLocIGLESIAS.data)=='NA NA')]
  }
  
  numLocIGLESIAS.optim = fitdistr(numLocIGLESIAS.data,'Poisson')
  
  return(list(
    par = as.numeric(coef(numLocIGLESIAS.optim)),
    LL = as.numeric(logLik(numLocIGLESIAS.optim))))
  
#   numLocIGLESIAS.optim = matrix(0,3,100)
#   
#   for(ii in 1:100){
#     numLocIGLESIAS.optim[1:2,ii] = c(ii, mean(numLocIGLESIAS.data) / (ii+mean(numLocIGLESIAS.data)))
#     numLocIGLESIAS.optim[3,ii] = sum(log(dnbinom(numLocIGLESIAS.data,numLocIGLESIAS.optim[1,ii],numLocIGLESIAS.optim[2,ii])))
#   }
#   
#   return(list(
#     par = numLocIGLESIAS.optim[1:2,which.max(numLocIGLESIAS.optim[3,])],
#     LL = max(numLocIGLESIAS.optim[3,])))
}



partLoc.full = partLoc.all
partLoc.all = partLoc.all[partLoc.all$landuse_class=='IGLESIAS',]



numLocIGLESIAS.models = list()

# H:F3,C4
numLocIGLESIAS.models$F3.C4 = list()

# DEN
partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Child',]
numLocIGLESIAS.models$F3.C4$DEN.Child = liknumLocIGLESIAS()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Homemaker',]
numLocIGLESIAS.models$F3.C4$DEN.Homemaker = liknumLocIGLESIAS()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Other',]
numLocIGLESIAS.models$F3.C4$DEN.Other = liknumLocIGLESIAS()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Student',]
numLocIGLESIAS.models$F3.C4$DEN.Student = liknumLocIGLESIAS()

# FEV
partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Child',]
numLocIGLESIAS.models$F3.C4$FEV.Child = liknumLocIGLESIAS()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Homemaker',]
numLocIGLESIAS.models$F3.C4$FEV.Homemaker = liknumLocIGLESIAS()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Other',]
numLocIGLESIAS.models$F3.C4$FEV.Other = liknumLocIGLESIAS()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Student',]
numLocIGLESIAS.models$F3.C4$FEV.Student = liknumLocIGLESIAS()

# Healthy
partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Child',]
numLocIGLESIAS.models$F3.C4$Healthy.Child = liknumLocIGLESIAS()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Homemaker',]
numLocIGLESIAS.models$F3.C4$Healthy.Homemaker = liknumLocIGLESIAS()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Other',]
numLocIGLESIAS.models$F3.C4$Healthy.Other = liknumLocIGLESIAS()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Student',]
numLocIGLESIAS.models$F3.C4$Healthy.Student = liknumLocIGLESIAS()

# calculate and store degrees of freedom and log likelihood
numLocIGLESIAS.models$F3.C4$LL = sum(
  sapply(numLocIGLESIAS.models$F3.C4, function(mm) mm$LL))

numLocIGLESIAS.models$F3.C4$k = length(numLocIGLESIAS.models$F3.C4) - 2



# H:F2,C4
numLocIGLESIAS.models$F2.C4 = list()

# DEN + FEV
partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) %in% c('DEN Child', 'FEV Child'),]
numLocIGLESIAS.models$F2.C4$DENFEV.Child = liknumLocIGLESIAS()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) %in% c('DEN Homemaker', 'FEV Homemaker'),]
numLocIGLESIAS.models$F2.C4$DENFEV.Homemaker = liknumLocIGLESIAS()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) %in% c('DEN Other', 'FEV Other'),]
numLocIGLESIAS.models$F2.C4$DENFEV.Other = liknumLocIGLESIAS()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) %in% c('DEN Student', 'FEV Student'),]
numLocIGLESIAS.models$F2.C4$DENFEV.Student = liknumLocIGLESIAS()

# Healthy
numLocIGLESIAS.models$F2.C4$Healthy.Child = numLocIGLESIAS.models$F3.C4$Healthy.Child
numLocIGLESIAS.models$F2.C4$Healthy.Homemaker = numLocIGLESIAS.models$F3.C4$Healthy.Homemaker
numLocIGLESIAS.models$F2.C4$Healthy.Other = numLocIGLESIAS.models$F3.C4$Healthy.Other
numLocIGLESIAS.models$F2.C4$Healthy.Student = numLocIGLESIAS.models$F3.C4$Healthy.Student

# calculate and store degress of freedom and log likelihood
numLocIGLESIAS.models$F2.C4$LL = sum(
  sapply(numLocIGLESIAS.models$F2.C4, function(mm) mm$LL))

numLocIGLESIAS.models$F2.C4$k = length(numLocIGLESIAS.models$F2.C4) - 2



# H:F1,C4
numLocIGLESIAS.models$F1.C4 = list()

# DEN + FEV + Healthy
partLoc = partLoc.all[partLoc.all$category == 'Child',]
numLocIGLESIAS.models$F1.C4$Child = liknumLocIGLESIAS()

partLoc = partLoc.all[partLoc.all$category == 'Homemaker',]
numLocIGLESIAS.models$F1.C4$Homemaker = liknumLocIGLESIAS()

partLoc = partLoc.all[partLoc.all$category == 'Other',]
numLocIGLESIAS.models$F1.C4$Other = liknumLocIGLESIAS()

partLoc = partLoc.all[partLoc.all$category == 'Student',]
numLocIGLESIAS.models$F1.C4$Student = liknumLocIGLESIAS()

# calculate and store degress of freedom and log likelihood
numLocIGLESIAS.models$F1.C4$LL = sum(
  sapply(numLocIGLESIAS.models$F1.C4, function(mm) mm$LL))

numLocIGLESIAS.models$F1.C4$k = length(numLocIGLESIAS.models$F1.C4) - 2



# H:F3,C1
numLocIGLESIAS.models$F3.C1 = list()

# Child + Homemaker + Other + Student
partLoc = partLoc.all[partLoc.all$pathogen == 'DEN',]
numLocIGLESIAS.models$F3.C1$DEN = liknumLocIGLESIAS()

partLoc = partLoc.all[partLoc.all$pathogen == 'FEV',]
numLocIGLESIAS.models$F3.C1$FEV = liknumLocIGLESIAS()

partLoc = partLoc.all[partLoc.all$pathogen == 'NA',]
numLocIGLESIAS.models$F3.C1$Healthy = liknumLocIGLESIAS()

# calculate and store degress of freedom and log likelihood
numLocIGLESIAS.models$F3.C1$LL = sum(
  sapply(numLocIGLESIAS.models$F3.C1, function(mm) mm$LL))

numLocIGLESIAS.models$F3.C1$k = length(numLocIGLESIAS.models$F3.C1) - 2



# H:F2,C1
numLocIGLESIAS.models$F2.C1 = list()

# DEN + FEV + Healthy
partLoc = partLoc.all[partLoc.all$pathogen %in% c('DEN', 'FEV'),]
numLocIGLESIAS.models$F2.C1$DEN = liknumLocIGLESIAS()

numLocIGLESIAS.models$F2.C1$Healthy = numLocIGLESIAS.models$F3.C1$Healthy

# calculate and store degress of freedom and log likelihood
numLocIGLESIAS.models$F2.C1$LL = sum(
  sapply(numLocIGLESIAS.models$F2.C1, function(mm) mm$LL))

numLocIGLESIAS.models$F2.C1$k = length(numLocIGLESIAS.models$F2.C1) - 2



# H:F1,C1
numLocIGLESIAS.models$F1.C1 = list()

# all
partLoc = partLoc.all
numLocIGLESIAS.models$F1.C1$all = liknumLocIGLESIAS()

# calculate and store degress of freedom and log likelihood
numLocIGLESIAS.models$F1.C1$LL = sum(
  sapply(numLocIGLESIAS.models$F1.C1, function(mm) mm$LL))

numLocIGLESIAS.models$F1.C1$k = length(numLocIGLESIAS.models$F1.C1) - 2




# matrices summarizing hypothesis tests

# test statistics
mat.teststat = matrix(NA,length(numLocIGLESIAS.models),length(numLocIGLESIAS.models))
row.names(mat.teststat) = names(numLocIGLESIAS.models)
colnames(mat.teststat) = names(numLocIGLESIAS.models)

mat.teststat['F3.C4','F2.C4'] =
  2 * (numLocIGLESIAS.models$F3.C4$LL - numLocIGLESIAS.models$F2.C4$LL)
mat.teststat['F3.C4','F1.C4'] =
  2 * (numLocIGLESIAS.models$F3.C4$LL - numLocIGLESIAS.models$F1.C4$LL)
mat.teststat['F3.C4','F3.C1'] =
  2 * (numLocIGLESIAS.models$F3.C4$LL - numLocIGLESIAS.models$F3.C1$LL)
mat.teststat['F3.C4','F2.C1'] =
  2 * (numLocIGLESIAS.models$F3.C4$LL - numLocIGLESIAS.models$F2.C1$LL)
mat.teststat['F3.C4','F1.C1'] =
  2 * (numLocIGLESIAS.models$F3.C4$LL - numLocIGLESIAS.models$F1.C1$LL)
mat.teststat['F2.C4','F1.C4'] =
  2 * (numLocIGLESIAS.models$F2.C4$LL - numLocIGLESIAS.models$F1.C4$LL)
mat.teststat['F2.C4','F2.C1'] =
  2 * (numLocIGLESIAS.models$F2.C4$LL - numLocIGLESIAS.models$F2.C1$LL)
mat.teststat['F2.C4','F1.C1'] =
  2 * (numLocIGLESIAS.models$F2.C4$LL - numLocIGLESIAS.models$F1.C1$LL)
mat.teststat['F1.C4','F1.C1'] =
  2 * (numLocIGLESIAS.models$F1.C4$LL - numLocIGLESIAS.models$F1.C1$LL)
mat.teststat['F3.C1','F2.C1'] =
  2 * (numLocIGLESIAS.models$F3.C1$LL - numLocIGLESIAS.models$F2.C1$LL)
mat.teststat['F3.C1','F1.C1'] =
  2 * (numLocIGLESIAS.models$F3.C1$LL - numLocIGLESIAS.models$F1.C1$LL)
mat.teststat['F2.C1','F1.C1'] =
  2 * (numLocIGLESIAS.models$F2.C1$LL - numLocIGLESIAS.models$F1.C1$LL)


# degrees of freedom
mat.df = matrix(NA,length(numLocIGLESIAS.models),length(numLocIGLESIAS.models))
row.names(mat.df) = names(numLocIGLESIAS.models)
colnames(mat.df) = names(numLocIGLESIAS.models)

mat.df['F3.C4','F2.C4'] =
  numLocIGLESIAS.models$F3.C4$k - numLocIGLESIAS.models$F2.C4$k
mat.df['F3.C4','F1.C4'] =
  numLocIGLESIAS.models$F3.C4$k - numLocIGLESIAS.models$F1.C4$k
mat.df['F3.C4','F3.C1'] =
  numLocIGLESIAS.models$F3.C4$k - numLocIGLESIAS.models$F3.C1$k
mat.df['F3.C4','F2.C1'] =
  numLocIGLESIAS.models$F3.C4$k - numLocIGLESIAS.models$F2.C1$k
mat.df['F3.C4','F1.C1'] =
  numLocIGLESIAS.models$F3.C4$k - numLocIGLESIAS.models$F1.C1$k
mat.df['F2.C4','F1.C4'] =
  numLocIGLESIAS.models$F2.C4$k - numLocIGLESIAS.models$F1.C4$k
mat.df['F2.C4','F2.C1'] =
  numLocIGLESIAS.models$F2.C4$k - numLocIGLESIAS.models$F2.C1$k
mat.df['F2.C4','F1.C1'] =
  numLocIGLESIAS.models$F2.C4$k - numLocIGLESIAS.models$F1.C1$k
mat.df['F1.C4','F1.C1'] =
  numLocIGLESIAS.models$F1.C4$k - numLocIGLESIAS.models$F1.C1$k
mat.df['F3.C1','F2.C1'] =
  numLocIGLESIAS.models$F3.C1$k - numLocIGLESIAS.models$F2.C1$k
mat.df['F3.C1','F1.C1'] =
  numLocIGLESIAS.models$F3.C1$k - numLocIGLESIAS.models$F1.C1$k
mat.df['F2.C1','F1.C1'] =
  numLocIGLESIAS.models$F2.C1$k - numLocIGLESIAS.models$F1.C1$k


# p value
mat.p = 1-pchisq(mat.teststat,mat.df)

partLoc.all = partLoc.full


