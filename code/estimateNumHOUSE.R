liknumLocHOUSE = function(){
  numLocHOUSE.data = table(paste(partLoc$part_code,partLoc$pathogen))
  if(sum(names(numLocHOUSE.data)=='NA NA')){
    numLocHOUSE.data = 
      numLocHOUSE.data[-which(names(numLocHOUSE.data)=='NA NA')]
  }
  
  numLocHOUSE.optim = fitdistr(numLocHOUSE.data,'Poisson')
  
  return(list(
    par = as.numeric(coef(numLocHOUSE.optim)),
    LL = as.numeric(logLik(numLocHOUSE.optim))))
  
#   numLocHOUSE.optim = matrix(0,3,100)
#   
#   for(ii in 1:100){
#     numLocHOUSE.optim[1:2,ii] = c(ii, mean(numLocHOUSE.data) / (ii+mean(numLocHOUSE.data)))
#     numLocHOUSE.optim[3,ii] = sum(log(dnbinom(numLocHOUSE.data,numLocHOUSE.optim[1,ii],numLocHOUSE.optim[2,ii])))
#   }
#   
#   return(list(
#     par = numLocHOUSE.optim[1:2,which.max(numLocHOUSE.optim[3,])],
#     LL = max(numLocHOUSE.optim[3,])))
}



partLoc.full = partLoc.all
partLoc.all = partLoc.all[partLoc.all$landuse_class=='HOUSE',]



numLocHOUSE.models = list()

# H:F3,C4
numLocHOUSE.models$F3.C4 = list()

# DEN
partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Child',]
numLocHOUSE.models$F3.C4$DEN.Child = liknumLocHOUSE()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Homemaker',]
numLocHOUSE.models$F3.C4$DEN.Homemaker = liknumLocHOUSE()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Other',]
numLocHOUSE.models$F3.C4$DEN.Other = liknumLocHOUSE()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Student',]
numLocHOUSE.models$F3.C4$DEN.Student = liknumLocHOUSE()

# FEV
partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Child',]
numLocHOUSE.models$F3.C4$FEV.Child = liknumLocHOUSE()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Homemaker',]
numLocHOUSE.models$F3.C4$FEV.Homemaker = liknumLocHOUSE()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Other',]
numLocHOUSE.models$F3.C4$FEV.Other = liknumLocHOUSE()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Student',]
numLocHOUSE.models$F3.C4$FEV.Student = liknumLocHOUSE()

# Healthy
partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Child',]
numLocHOUSE.models$F3.C4$Healthy.Child = liknumLocHOUSE()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Homemaker',]
numLocHOUSE.models$F3.C4$Healthy.Homemaker = liknumLocHOUSE()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Other',]
numLocHOUSE.models$F3.C4$Healthy.Other = liknumLocHOUSE()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Student',]
numLocHOUSE.models$F3.C4$Healthy.Student = liknumLocHOUSE()

# calculate and store degrees of freedom and log likelihood
numLocHOUSE.models$F3.C4$LL = sum(
  sapply(numLocHOUSE.models$F3.C4, function(mm) mm$LL))

numLocHOUSE.models$F3.C4$k = length(numLocHOUSE.models$F3.C4) - 2



# H:F2,C4
numLocHOUSE.models$F2.C4 = list()

# DEN + FEV
partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) %in% c('DEN Child', 'FEV Child'),]
numLocHOUSE.models$F2.C4$DENFEV.Child = liknumLocHOUSE()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) %in% c('DEN Homemaker', 'FEV Homemaker'),]
numLocHOUSE.models$F2.C4$DENFEV.Homemaker = liknumLocHOUSE()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) %in% c('DEN Other', 'FEV Other'),]
numLocHOUSE.models$F2.C4$DENFEV.Other = liknumLocHOUSE()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) %in% c('DEN Student', 'FEV Student'),]
numLocHOUSE.models$F2.C4$DENFEV.Student = liknumLocHOUSE()

# Healthy
numLocHOUSE.models$F2.C4$Healthy.Child = numLocHOUSE.models$F3.C4$Healthy.Child
numLocHOUSE.models$F2.C4$Healthy.Homemaker = numLocHOUSE.models$F3.C4$Healthy.Homemaker
numLocHOUSE.models$F2.C4$Healthy.Other = numLocHOUSE.models$F3.C4$Healthy.Other
numLocHOUSE.models$F2.C4$Healthy.Student = numLocHOUSE.models$F3.C4$Healthy.Student

# calculate and store degress of freedom and log likelihood
numLocHOUSE.models$F2.C4$LL = sum(
  sapply(numLocHOUSE.models$F2.C4, function(mm) mm$LL))

numLocHOUSE.models$F2.C4$k = length(numLocHOUSE.models$F2.C4) - 2



# H:F1,C4
numLocHOUSE.models$F1.C4 = list()

# DEN + FEV + Healthy
partLoc = partLoc.all[partLoc.all$category == 'Child',]
numLocHOUSE.models$F1.C4$Child = liknumLocHOUSE()

partLoc = partLoc.all[partLoc.all$category == 'Homemaker',]
numLocHOUSE.models$F1.C4$Homemaker = liknumLocHOUSE()

partLoc = partLoc.all[partLoc.all$category == 'Other',]
numLocHOUSE.models$F1.C4$Other = liknumLocHOUSE()

partLoc = partLoc.all[partLoc.all$category == 'Student',]
numLocHOUSE.models$F1.C4$Student = liknumLocHOUSE()

# calculate and store degress of freedom and log likelihood
numLocHOUSE.models$F1.C4$LL = sum(
  sapply(numLocHOUSE.models$F1.C4, function(mm) mm$LL))

numLocHOUSE.models$F1.C4$k = length(numLocHOUSE.models$F1.C4) - 2



# H:F3,C1
numLocHOUSE.models$F3.C1 = list()

# Child + Homemaker + Other + Student
partLoc = partLoc.all[partLoc.all$pathogen == 'DEN',]
numLocHOUSE.models$F3.C1$DEN = liknumLocHOUSE()

partLoc = partLoc.all[partLoc.all$pathogen == 'FEV',]
numLocHOUSE.models$F3.C1$FEV = liknumLocHOUSE()

partLoc = partLoc.all[partLoc.all$pathogen == 'NA',]
numLocHOUSE.models$F3.C1$Healthy = liknumLocHOUSE()

# calculate and store degress of freedom and log likelihood
numLocHOUSE.models$F3.C1$LL = sum(
  sapply(numLocHOUSE.models$F3.C1, function(mm) mm$LL))

numLocHOUSE.models$F3.C1$k = length(numLocHOUSE.models$F3.C1) - 2



# H:F2,C1
numLocHOUSE.models$F2.C1 = list()

# DEN + FEV + Healthy
partLoc = partLoc.all[partLoc.all$pathogen %in% c('DEN', 'FEV'),]
numLocHOUSE.models$F2.C1$DEN = liknumLocHOUSE()

numLocHOUSE.models$F2.C1$Healthy = numLocHOUSE.models$F3.C1$Healthy

# calculate and store degress of freedom and log likelihood
numLocHOUSE.models$F2.C1$LL = sum(
  sapply(numLocHOUSE.models$F2.C1, function(mm) mm$LL))

numLocHOUSE.models$F2.C1$k = length(numLocHOUSE.models$F2.C1) - 2



# H:F1,C1
numLocHOUSE.models$F1.C1 = list()

# all
partLoc = partLoc.all
numLocHOUSE.models$F1.C1$all = liknumLocHOUSE()

# calculate and store degress of freedom and log likelihood
numLocHOUSE.models$F1.C1$LL = sum(
  sapply(numLocHOUSE.models$F1.C1, function(mm) mm$LL))

numLocHOUSE.models$F1.C1$k = length(numLocHOUSE.models$F1.C1) - 2




# matrices summarizing hypothesis tests

# test statistics
mat.teststat = matrix(NA,length(numLocHOUSE.models),length(numLocHOUSE.models))
row.names(mat.teststat) = names(numLocHOUSE.models)
colnames(mat.teststat) = names(numLocHOUSE.models)

mat.teststat['F3.C4','F2.C4'] =
  2 * (numLocHOUSE.models$F3.C4$LL - numLocHOUSE.models$F2.C4$LL)
mat.teststat['F3.C4','F1.C4'] =
  2 * (numLocHOUSE.models$F3.C4$LL - numLocHOUSE.models$F1.C4$LL)
mat.teststat['F3.C4','F3.C1'] =
  2 * (numLocHOUSE.models$F3.C4$LL - numLocHOUSE.models$F3.C1$LL)
mat.teststat['F3.C4','F2.C1'] =
  2 * (numLocHOUSE.models$F3.C4$LL - numLocHOUSE.models$F2.C1$LL)
mat.teststat['F3.C4','F1.C1'] =
  2 * (numLocHOUSE.models$F3.C4$LL - numLocHOUSE.models$F1.C1$LL)
mat.teststat['F2.C4','F1.C4'] =
  2 * (numLocHOUSE.models$F2.C4$LL - numLocHOUSE.models$F1.C4$LL)
mat.teststat['F2.C4','F2.C1'] =
  2 * (numLocHOUSE.models$F2.C4$LL - numLocHOUSE.models$F2.C1$LL)
mat.teststat['F2.C4','F1.C1'] =
  2 * (numLocHOUSE.models$F2.C4$LL - numLocHOUSE.models$F1.C1$LL)
mat.teststat['F1.C4','F1.C1'] =
  2 * (numLocHOUSE.models$F1.C4$LL - numLocHOUSE.models$F1.C1$LL)
mat.teststat['F3.C1','F2.C1'] =
  2 * (numLocHOUSE.models$F3.C1$LL - numLocHOUSE.models$F2.C1$LL)
mat.teststat['F3.C1','F1.C1'] =
  2 * (numLocHOUSE.models$F3.C1$LL - numLocHOUSE.models$F1.C1$LL)
mat.teststat['F2.C1','F1.C1'] =
  2 * (numLocHOUSE.models$F2.C1$LL - numLocHOUSE.models$F1.C1$LL)


# degrees of freedom
mat.df = matrix(NA,length(numLocHOUSE.models),length(numLocHOUSE.models))
row.names(mat.df) = names(numLocHOUSE.models)
colnames(mat.df) = names(numLocHOUSE.models)

mat.df['F3.C4','F2.C4'] =
  numLocHOUSE.models$F3.C4$k - numLocHOUSE.models$F2.C4$k
mat.df['F3.C4','F1.C4'] =
  numLocHOUSE.models$F3.C4$k - numLocHOUSE.models$F1.C4$k
mat.df['F3.C4','F3.C1'] =
  numLocHOUSE.models$F3.C4$k - numLocHOUSE.models$F3.C1$k
mat.df['F3.C4','F2.C1'] =
  numLocHOUSE.models$F3.C4$k - numLocHOUSE.models$F2.C1$k
mat.df['F3.C4','F1.C1'] =
  numLocHOUSE.models$F3.C4$k - numLocHOUSE.models$F1.C1$k
mat.df['F2.C4','F1.C4'] =
  numLocHOUSE.models$F2.C4$k - numLocHOUSE.models$F1.C4$k
mat.df['F2.C4','F2.C1'] =
  numLocHOUSE.models$F2.C4$k - numLocHOUSE.models$F2.C1$k
mat.df['F2.C4','F1.C1'] =
  numLocHOUSE.models$F2.C4$k - numLocHOUSE.models$F1.C1$k
mat.df['F1.C4','F1.C1'] =
  numLocHOUSE.models$F1.C4$k - numLocHOUSE.models$F1.C1$k
mat.df['F3.C1','F2.C1'] =
  numLocHOUSE.models$F3.C1$k - numLocHOUSE.models$F2.C1$k
mat.df['F3.C1','F1.C1'] =
  numLocHOUSE.models$F3.C1$k - numLocHOUSE.models$F1.C1$k
mat.df['F2.C1','F1.C1'] =
  numLocHOUSE.models$F2.C1$k - numLocHOUSE.models$F1.C1$k


# p value
mat.p = 1-pchisq(mat.teststat,mat.df)

partLoc.all = partLoc.full


