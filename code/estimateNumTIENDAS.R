liknumLocTIENDAS = function(){
  numLocTIENDAS.data = table(paste(partLoc$part_code,partLoc$pathogen))
  if(sum(names(numLocTIENDAS.data)=='NA NA')){
    numLocTIENDAS.data = 
      numLocTIENDAS.data[-which(names(numLocTIENDAS.data)=='NA NA')]
  }
  
  numLocTIENDAS.optim = fitdistr(numLocTIENDAS.data,'Poisson')
  
  return(list(
    par = as.numeric(coef(numLocTIENDAS.optim)),
    LL = as.numeric(logLik(numLocTIENDAS.optim))))
  
  #   numLocTIENDAS.optim = matrix(0,3,100)
  #   
  #   for(ii in 1:100){
  #     numLocTIENDAS.optim[1:2,ii] = c(ii, mean(numLocTIENDAS.data) / (ii+mean(numLocTIENDAS.data)))
  #     numLocTIENDAS.optim[3,ii] = sum(log(dnbinom(numLocTIENDAS.data,numLocTIENDAS.optim[1,ii],numLocTIENDAS.optim[2,ii])))
  #   }
  #   
  #   return(list(
  #     par = numLocTIENDAS.optim[1:2,which.max(numLocTIENDAS.optim[3,])],
  #     LL = max(numLocTIENDAS.optim[3,])))
}



partLoc.full = partLoc.all
partLoc.all = partLoc.all[partLoc.all$landuse_class=='TIENDAS',]



numLocTIENDAS.models = list()

# H:F3,C4
numLocTIENDAS.models$F3.C4 = list()

# DEN
partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Child',]
numLocTIENDAS.models$F3.C4$DEN.Child = liknumLocTIENDAS()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Homemaker',]
numLocTIENDAS.models$F3.C4$DEN.Homemaker = liknumLocTIENDAS()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Other',]
numLocTIENDAS.models$F3.C4$DEN.Other = liknumLocTIENDAS()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Student',]
numLocTIENDAS.models$F3.C4$DEN.Student = liknumLocTIENDAS()

# FEV
partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Child',]
numLocTIENDAS.models$F3.C4$FEV.Child = liknumLocTIENDAS()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Homemaker',]
numLocTIENDAS.models$F3.C4$FEV.Homemaker = liknumLocTIENDAS()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Other',]
numLocTIENDAS.models$F3.C4$FEV.Other = liknumLocTIENDAS()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Student',]
numLocTIENDAS.models$F3.C4$FEV.Student = liknumLocTIENDAS()

# Healthy
partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Child',]
numLocTIENDAS.models$F3.C4$Healthy.Child = liknumLocTIENDAS()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Homemaker',]
numLocTIENDAS.models$F3.C4$Healthy.Homemaker = liknumLocTIENDAS()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Other',]
numLocTIENDAS.models$F3.C4$Healthy.Other = liknumLocTIENDAS()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Student',]
numLocTIENDAS.models$F3.C4$Healthy.Student = liknumLocTIENDAS()

# calculate and store degrees of freedom and log likelihood
numLocTIENDAS.models$F3.C4$LL = sum(
  sapply(numLocTIENDAS.models$F3.C4, function(mm) mm$LL))

numLocTIENDAS.models$F3.C4$k = length(numLocTIENDAS.models$F3.C4) - 2



# H:F2,C4
numLocTIENDAS.models$F2.C4 = list()

# DEN + FEV
partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) %in% c('DEN Child', 'FEV Child'),]
numLocTIENDAS.models$F2.C4$DENFEV.Child = liknumLocTIENDAS()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) %in% c('DEN Homemaker', 'FEV Homemaker'),]
numLocTIENDAS.models$F2.C4$DENFEV.Homemaker = liknumLocTIENDAS()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) %in% c('DEN Other', 'FEV Other'),]
numLocTIENDAS.models$F2.C4$DENFEV.Other = liknumLocTIENDAS()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) %in% c('DEN Student', 'FEV Student'),]
numLocTIENDAS.models$F2.C4$DENFEV.Student = liknumLocTIENDAS()

# Healthy
numLocTIENDAS.models$F2.C4$Healthy.Child = numLocTIENDAS.models$F3.C4$Healthy.Child
numLocTIENDAS.models$F2.C4$Healthy.Homemaker = numLocTIENDAS.models$F3.C4$Healthy.Homemaker
numLocTIENDAS.models$F2.C4$Healthy.Other = numLocTIENDAS.models$F3.C4$Healthy.Other
numLocTIENDAS.models$F2.C4$Healthy.Student = numLocTIENDAS.models$F3.C4$Healthy.Student

# calculate and store degress of freedom and log likelihood
numLocTIENDAS.models$F2.C4$LL = sum(
  sapply(numLocTIENDAS.models$F2.C4, function(mm) mm$LL))

numLocTIENDAS.models$F2.C4$k = length(numLocTIENDAS.models$F2.C4) - 2



# H:F1,C4
numLocTIENDAS.models$F1.C4 = list()

# DEN + FEV + Healthy
partLoc = partLoc.all[partLoc.all$category == 'Child',]
numLocTIENDAS.models$F1.C4$Child = liknumLocTIENDAS()

partLoc = partLoc.all[partLoc.all$category == 'Homemaker',]
numLocTIENDAS.models$F1.C4$Homemaker = liknumLocTIENDAS()

partLoc = partLoc.all[partLoc.all$category == 'Other',]
numLocTIENDAS.models$F1.C4$Other = liknumLocTIENDAS()

partLoc = partLoc.all[partLoc.all$category == 'Student',]
numLocTIENDAS.models$F1.C4$Student = liknumLocTIENDAS()

# calculate and store degress of freedom and log likelihood
numLocTIENDAS.models$F1.C4$LL = sum(
  sapply(numLocTIENDAS.models$F1.C4, function(mm) mm$LL))

numLocTIENDAS.models$F1.C4$k = length(numLocTIENDAS.models$F1.C4) - 2



# H:F3,C1
numLocTIENDAS.models$F3.C1 = list()

# Child + Homemaker + Other + Student
partLoc = partLoc.all[partLoc.all$pathogen == 'DEN',]
numLocTIENDAS.models$F3.C1$DEN = liknumLocTIENDAS()

partLoc = partLoc.all[partLoc.all$pathogen == 'FEV',]
numLocTIENDAS.models$F3.C1$FEV = liknumLocTIENDAS()

partLoc = partLoc.all[partLoc.all$pathogen == 'NA',]
numLocTIENDAS.models$F3.C1$Healthy = liknumLocTIENDAS()

# calculate and store degress of freedom and log likelihood
numLocTIENDAS.models$F3.C1$LL = sum(
  sapply(numLocTIENDAS.models$F3.C1, function(mm) mm$LL))

numLocTIENDAS.models$F3.C1$k = length(numLocTIENDAS.models$F3.C1) - 2



# H:F2,C1
numLocTIENDAS.models$F2.C1 = list()

# DEN + FEV + Healthy
partLoc = partLoc.all[partLoc.all$pathogen %in% c('DEN', 'FEV'),]
numLocTIENDAS.models$F2.C1$DEN = liknumLocTIENDAS()

numLocTIENDAS.models$F2.C1$Healthy = numLocTIENDAS.models$F3.C1$Healthy

# calculate and store degress of freedom and log likelihood
numLocTIENDAS.models$F2.C1$LL = sum(
  sapply(numLocTIENDAS.models$F2.C1, function(mm) mm$LL))

numLocTIENDAS.models$F2.C1$k = length(numLocTIENDAS.models$F2.C1) - 2



# H:F1,C1
numLocTIENDAS.models$F1.C1 = list()

# all
partLoc = partLoc.all
numLocTIENDAS.models$F1.C1$all = liknumLocTIENDAS()

# calculate and store degress of freedom and log likelihood
numLocTIENDAS.models$F1.C1$LL = sum(
  sapply(numLocTIENDAS.models$F1.C1, function(mm) mm$LL))

numLocTIENDAS.models$F1.C1$k = length(numLocTIENDAS.models$F1.C1) - 2




# matrices summarizing hypothesis tests

# test statistics
mat.teststat = matrix(NA,length(numLocTIENDAS.models),length(numLocTIENDAS.models))
row.names(mat.teststat) = names(numLocTIENDAS.models)
colnames(mat.teststat) = names(numLocTIENDAS.models)

mat.teststat['F3.C4','F2.C4'] =
  2 * (numLocTIENDAS.models$F3.C4$LL - numLocTIENDAS.models$F2.C4$LL)
mat.teststat['F3.C4','F1.C4'] =
  2 * (numLocTIENDAS.models$F3.C4$LL - numLocTIENDAS.models$F1.C4$LL)
mat.teststat['F3.C4','F3.C1'] =
  2 * (numLocTIENDAS.models$F3.C4$LL - numLocTIENDAS.models$F3.C1$LL)
mat.teststat['F3.C4','F2.C1'] =
  2 * (numLocTIENDAS.models$F3.C4$LL - numLocTIENDAS.models$F2.C1$LL)
mat.teststat['F3.C4','F1.C1'] =
  2 * (numLocTIENDAS.models$F3.C4$LL - numLocTIENDAS.models$F1.C1$LL)
mat.teststat['F2.C4','F1.C4'] =
  2 * (numLocTIENDAS.models$F2.C4$LL - numLocTIENDAS.models$F1.C4$LL)
mat.teststat['F2.C4','F2.C1'] =
  2 * (numLocTIENDAS.models$F2.C4$LL - numLocTIENDAS.models$F2.C1$LL)
mat.teststat['F2.C4','F1.C1'] =
  2 * (numLocTIENDAS.models$F2.C4$LL - numLocTIENDAS.models$F1.C1$LL)
mat.teststat['F1.C4','F1.C1'] =
  2 * (numLocTIENDAS.models$F1.C4$LL - numLocTIENDAS.models$F1.C1$LL)
mat.teststat['F3.C1','F2.C1'] =
  2 * (numLocTIENDAS.models$F3.C1$LL - numLocTIENDAS.models$F2.C1$LL)
mat.teststat['F3.C1','F1.C1'] =
  2 * (numLocTIENDAS.models$F3.C1$LL - numLocTIENDAS.models$F1.C1$LL)
mat.teststat['F2.C1','F1.C1'] =
  2 * (numLocTIENDAS.models$F2.C1$LL - numLocTIENDAS.models$F1.C1$LL)


# degrees of freedom
mat.df = matrix(NA,length(numLocTIENDAS.models),length(numLocTIENDAS.models))
row.names(mat.df) = names(numLocTIENDAS.models)
colnames(mat.df) = names(numLocTIENDAS.models)

mat.df['F3.C4','F2.C4'] =
  numLocTIENDAS.models$F3.C4$k - numLocTIENDAS.models$F2.C4$k
mat.df['F3.C4','F1.C4'] =
  numLocTIENDAS.models$F3.C4$k - numLocTIENDAS.models$F1.C4$k
mat.df['F3.C4','F3.C1'] =
  numLocTIENDAS.models$F3.C4$k - numLocTIENDAS.models$F3.C1$k
mat.df['F3.C4','F2.C1'] =
  numLocTIENDAS.models$F3.C4$k - numLocTIENDAS.models$F2.C1$k
mat.df['F3.C4','F1.C1'] =
  numLocTIENDAS.models$F3.C4$k - numLocTIENDAS.models$F1.C1$k
mat.df['F2.C4','F1.C4'] =
  numLocTIENDAS.models$F2.C4$k - numLocTIENDAS.models$F1.C4$k
mat.df['F2.C4','F2.C1'] =
  numLocTIENDAS.models$F2.C4$k - numLocTIENDAS.models$F2.C1$k
mat.df['F2.C4','F1.C1'] =
  numLocTIENDAS.models$F2.C4$k - numLocTIENDAS.models$F1.C1$k
mat.df['F1.C4','F1.C1'] =
  numLocTIENDAS.models$F1.C4$k - numLocTIENDAS.models$F1.C1$k
mat.df['F3.C1','F2.C1'] =
  numLocTIENDAS.models$F3.C1$k - numLocTIENDAS.models$F2.C1$k
mat.df['F3.C1','F1.C1'] =
  numLocTIENDAS.models$F3.C1$k - numLocTIENDAS.models$F1.C1$k
mat.df['F2.C1','F1.C1'] =
  numLocTIENDAS.models$F2.C1$k - numLocTIENDAS.models$F1.C1$k


# p value
mat.p = 1-pchisq(mat.teststat,mat.df)

partLoc.all = partLoc.full



