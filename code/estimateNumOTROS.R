liknumLocOTROS = function(){
  numLocOTROS.data = table(paste(partLoc$part_code,partLoc$pathogen))
  if(sum(names(numLocOTROS.data)=='NA NA')){
    numLocOTROS.data = 
      numLocOTROS.data[-which(names(numLocOTROS.data)=='NA NA')]
  }
  
  numLocOTROS.optim = fitdistr(numLocOTROS.data,'Poisson')
  
  return(list(
    par = as.numeric(coef(numLocOTROS.optim)),
    LL = as.numeric(logLik(numLocOTROS.optim))))
  
#   numLocOTROS.optim = matrix(0,3,100)
#   
#   for(ii in 1:100){
#     numLocOTROS.optim[1:2,ii] = c(ii, mean(numLocOTROS.data) / (ii+mean(numLocOTROS.data)))
#     numLocOTROS.optim[3,ii] = sum(log(dnbinom(numLocOTROS.data,numLocOTROS.optim[1,ii],numLocOTROS.optim[2,ii])))
#   }
#   
#   return(list(
#     par = numLocOTROS.optim[1:2,which.max(numLocOTROS.optim[3,])],
#     LL = max(numLocOTROS.optim[3,])))
}



partLoc.full = partLoc.all
partLoc.all = partLoc.all[partLoc.all$landuse_class=='OTROS',]



numLocOTROS.models = list()

# H:F3,C4
numLocOTROS.models$F3.C4 = list()

# DEN
partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Child',]
numLocOTROS.models$F3.C4$DEN.Child = liknumLocOTROS()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Homemaker',]
numLocOTROS.models$F3.C4$DEN.Homemaker = liknumLocOTROS()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Other',]
numLocOTROS.models$F3.C4$DEN.Other = liknumLocOTROS()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Student',]
numLocOTROS.models$F3.C4$DEN.Student = liknumLocOTROS()

# FEV
partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Child',]
numLocOTROS.models$F3.C4$FEV.Child = liknumLocOTROS()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Homemaker',]
numLocOTROS.models$F3.C4$FEV.Homemaker = liknumLocOTROS()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Other',]
numLocOTROS.models$F3.C4$FEV.Other = liknumLocOTROS()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Student',]
numLocOTROS.models$F3.C4$FEV.Student = liknumLocOTROS()

# Healthy
partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Child',]
numLocOTROS.models$F3.C4$Healthy.Child = liknumLocOTROS()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Homemaker',]
numLocOTROS.models$F3.C4$Healthy.Homemaker = liknumLocOTROS()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Other',]
numLocOTROS.models$F3.C4$Healthy.Other = liknumLocOTROS()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Student',]
numLocOTROS.models$F3.C4$Healthy.Student = liknumLocOTROS()

# calculate and store degrees of freedom and log likelihood
numLocOTROS.models$F3.C4$LL = sum(
  sapply(numLocOTROS.models$F3.C4, function(mm) mm$LL))

numLocOTROS.models$F3.C4$k = length(numLocOTROS.models$F3.C4) - 2



# H:F2,C4
numLocOTROS.models$F2.C4 = list()

# DEN + FEV
partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) %in% c('DEN Child', 'FEV Child'),]
numLocOTROS.models$F2.C4$DENFEV.Child = liknumLocOTROS()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) %in% c('DEN Homemaker', 'FEV Homemaker'),]
numLocOTROS.models$F2.C4$DENFEV.Homemaker = liknumLocOTROS()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) %in% c('DEN Other', 'FEV Other'),]
numLocOTROS.models$F2.C4$DENFEV.Other = liknumLocOTROS()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) %in% c('DEN Student', 'FEV Student'),]
numLocOTROS.models$F2.C4$DENFEV.Student = liknumLocOTROS()

# Healthy
numLocOTROS.models$F2.C4$Healthy.Child = numLocOTROS.models$F3.C4$Healthy.Child
numLocOTROS.models$F2.C4$Healthy.Homemaker = numLocOTROS.models$F3.C4$Healthy.Homemaker
numLocOTROS.models$F2.C4$Healthy.Other = numLocOTROS.models$F3.C4$Healthy.Other
numLocOTROS.models$F2.C4$Healthy.Student = numLocOTROS.models$F3.C4$Healthy.Student

# calculate and store degress of freedom and log likelihood
numLocOTROS.models$F2.C4$LL = sum(
  sapply(numLocOTROS.models$F2.C4, function(mm) mm$LL))

numLocOTROS.models$F2.C4$k = length(numLocOTROS.models$F2.C4) - 2



# H:F1,C4
numLocOTROS.models$F1.C4 = list()

# DEN + FEV + Healthy
partLoc = partLoc.all[partLoc.all$category == 'Child',]
numLocOTROS.models$F1.C4$Child = liknumLocOTROS()

partLoc = partLoc.all[partLoc.all$category == 'Homemaker',]
numLocOTROS.models$F1.C4$Homemaker = liknumLocOTROS()

partLoc = partLoc.all[partLoc.all$category == 'Other',]
numLocOTROS.models$F1.C4$Other = liknumLocOTROS()

partLoc = partLoc.all[partLoc.all$category == 'Student',]
numLocOTROS.models$F1.C4$Student = liknumLocOTROS()

# calculate and store degress of freedom and log likelihood
numLocOTROS.models$F1.C4$LL = sum(
  sapply(numLocOTROS.models$F1.C4, function(mm) mm$LL))

numLocOTROS.models$F1.C4$k = length(numLocOTROS.models$F1.C4) - 2



# H:F3,C1
numLocOTROS.models$F3.C1 = list()

# Child + Homemaker + Other + Student
partLoc = partLoc.all[partLoc.all$pathogen == 'DEN',]
numLocOTROS.models$F3.C1$DEN = liknumLocOTROS()

partLoc = partLoc.all[partLoc.all$pathogen == 'FEV',]
numLocOTROS.models$F3.C1$FEV = liknumLocOTROS()

partLoc = partLoc.all[partLoc.all$pathogen == 'NA',]
numLocOTROS.models$F3.C1$Healthy = liknumLocOTROS()

# calculate and store degress of freedom and log likelihood
numLocOTROS.models$F3.C1$LL = sum(
  sapply(numLocOTROS.models$F3.C1, function(mm) mm$LL))

numLocOTROS.models$F3.C1$k = length(numLocOTROS.models$F3.C1) - 2



# H:F2,C1
numLocOTROS.models$F2.C1 = list()

# DEN + FEV + Healthy
partLoc = partLoc.all[partLoc.all$pathogen %in% c('DEN', 'FEV'),]
numLocOTROS.models$F2.C1$DEN = liknumLocOTROS()

numLocOTROS.models$F2.C1$Healthy = numLocOTROS.models$F3.C1$Healthy

# calculate and store degress of freedom and log likelihood
numLocOTROS.models$F2.C1$LL = sum(
  sapply(numLocOTROS.models$F2.C1, function(mm) mm$LL))

numLocOTROS.models$F2.C1$k = length(numLocOTROS.models$F2.C1) - 2



# H:F1,C1
numLocOTROS.models$F1.C1 = list()

# all
partLoc = partLoc.all
numLocOTROS.models$F1.C1$all = liknumLocOTROS()

# calculate and store degress of freedom and log likelihood
numLocOTROS.models$F1.C1$LL = sum(
  sapply(numLocOTROS.models$F1.C1, function(mm) mm$LL))

numLocOTROS.models$F1.C1$k = length(numLocOTROS.models$F1.C1) - 2




# matrices summarizing hypothesis tests

# test statistics
mat.teststat = matrix(NA,length(numLocOTROS.models),length(numLocOTROS.models))
row.names(mat.teststat) = names(numLocOTROS.models)
colnames(mat.teststat) = names(numLocOTROS.models)

mat.teststat['F3.C4','F2.C4'] =
  2 * (numLocOTROS.models$F3.C4$LL - numLocOTROS.models$F2.C4$LL)
mat.teststat['F3.C4','F1.C4'] =
  2 * (numLocOTROS.models$F3.C4$LL - numLocOTROS.models$F1.C4$LL)
mat.teststat['F3.C4','F3.C1'] =
  2 * (numLocOTROS.models$F3.C4$LL - numLocOTROS.models$F3.C1$LL)
mat.teststat['F3.C4','F2.C1'] =
  2 * (numLocOTROS.models$F3.C4$LL - numLocOTROS.models$F2.C1$LL)
mat.teststat['F3.C4','F1.C1'] =
  2 * (numLocOTROS.models$F3.C4$LL - numLocOTROS.models$F1.C1$LL)
mat.teststat['F2.C4','F1.C4'] =
  2 * (numLocOTROS.models$F2.C4$LL - numLocOTROS.models$F1.C4$LL)
mat.teststat['F2.C4','F2.C1'] =
  2 * (numLocOTROS.models$F2.C4$LL - numLocOTROS.models$F2.C1$LL)
mat.teststat['F2.C4','F1.C1'] =
  2 * (numLocOTROS.models$F2.C4$LL - numLocOTROS.models$F1.C1$LL)
mat.teststat['F1.C4','F1.C1'] =
  2 * (numLocOTROS.models$F1.C4$LL - numLocOTROS.models$F1.C1$LL)
mat.teststat['F3.C1','F2.C1'] =
  2 * (numLocOTROS.models$F3.C1$LL - numLocOTROS.models$F2.C1$LL)
mat.teststat['F3.C1','F1.C1'] =
  2 * (numLocOTROS.models$F3.C1$LL - numLocOTROS.models$F1.C1$LL)
mat.teststat['F2.C1','F1.C1'] =
  2 * (numLocOTROS.models$F2.C1$LL - numLocOTROS.models$F1.C1$LL)


# degrees of freedom
mat.df = matrix(NA,length(numLocOTROS.models),length(numLocOTROS.models))
row.names(mat.df) = names(numLocOTROS.models)
colnames(mat.df) = names(numLocOTROS.models)

mat.df['F3.C4','F2.C4'] =
  numLocOTROS.models$F3.C4$k - numLocOTROS.models$F2.C4$k
mat.df['F3.C4','F1.C4'] =
  numLocOTROS.models$F3.C4$k - numLocOTROS.models$F1.C4$k
mat.df['F3.C4','F3.C1'] =
  numLocOTROS.models$F3.C4$k - numLocOTROS.models$F3.C1$k
mat.df['F3.C4','F2.C1'] =
  numLocOTROS.models$F3.C4$k - numLocOTROS.models$F2.C1$k
mat.df['F3.C4','F1.C1'] =
  numLocOTROS.models$F3.C4$k - numLocOTROS.models$F1.C1$k
mat.df['F2.C4','F1.C4'] =
  numLocOTROS.models$F2.C4$k - numLocOTROS.models$F1.C4$k
mat.df['F2.C4','F2.C1'] =
  numLocOTROS.models$F2.C4$k - numLocOTROS.models$F2.C1$k
mat.df['F2.C4','F1.C1'] =
  numLocOTROS.models$F2.C4$k - numLocOTROS.models$F1.C1$k
mat.df['F1.C4','F1.C1'] =
  numLocOTROS.models$F1.C4$k - numLocOTROS.models$F1.C1$k
mat.df['F3.C1','F2.C1'] =
  numLocOTROS.models$F3.C1$k - numLocOTROS.models$F2.C1$k
mat.df['F3.C1','F1.C1'] =
  numLocOTROS.models$F3.C1$k - numLocOTROS.models$F1.C1$k
mat.df['F2.C1','F1.C1'] =
  numLocOTROS.models$F2.C1$k - numLocOTROS.models$F1.C1$k


# p value
mat.p = 1-pchisq(mat.teststat,mat.df)

partLoc.all = partLoc.full


