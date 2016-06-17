liknumLocEDUCACION = function(){
  numLocEDUCACION.data = table(paste(partLoc$part_code,partLoc$pathogen))
  if(sum(names(numLocEDUCACION.data)=='NA NA')){
    numLocEDUCACION.data = 
      numLocEDUCACION.data[-which(names(numLocEDUCACION.data)=='NA NA')]
  }
  
  numLocEDUCACION.optim = fitdistr(numLocEDUCACION.data,'Poisson')
  
  return(list(
    par = as.numeric(coef(numLocEDUCACION.optim)),
    LL = as.numeric(logLik(numLocEDUCACION.optim))))
  
#   numLocEDUCACION.optim = matrix(0,3,100)
#   
#   for(ii in 1:100){
#     numLocEDUCACION.optim[1:2,ii] = c(ii, mean(numLocEDUCACION.data) / (ii+mean(numLocEDUCACION.data)))
#     numLocEDUCACION.optim[3,ii] = sum(log(dnbinom(numLocEDUCACION.data,numLocEDUCACION.optim[1,ii],numLocEDUCACION.optim[2,ii])))
#   }
#   
#   return(list(
#     par = numLocEDUCACION.optim[1:2,which.max(numLocEDUCACION.optim[3,])],
#     LL = max(numLocEDUCACION.optim[3,])))
}



partLoc.full = partLoc.all
partLoc.all = partLoc.all[partLoc.all$landuse_class=='EDUCACION',]



numLocEDUCACION.models = list()

# H:F3,C4
numLocEDUCACION.models$F3.C4 = list()

# DEN
partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Child',]
numLocEDUCACION.models$F3.C4$DEN.Child = liknumLocEDUCACION()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Homemaker',]
numLocEDUCACION.models$F3.C4$DEN.Homemaker = liknumLocEDUCACION()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Other',]
numLocEDUCACION.models$F3.C4$DEN.Other = liknumLocEDUCACION()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Student',]
numLocEDUCACION.models$F3.C4$DEN.Student = liknumLocEDUCACION()

# FEV
partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Child',]
numLocEDUCACION.models$F3.C4$FEV.Child = liknumLocEDUCACION()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Homemaker',]
numLocEDUCACION.models$F3.C4$FEV.Homemaker = liknumLocEDUCACION()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Other',]
numLocEDUCACION.models$F3.C4$FEV.Other = liknumLocEDUCACION()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Student',]
numLocEDUCACION.models$F3.C4$FEV.Student = liknumLocEDUCACION()

# Healthy
partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Child',]
numLocEDUCACION.models$F3.C4$Healthy.Child = liknumLocEDUCACION()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Homemaker',]
numLocEDUCACION.models$F3.C4$Healthy.Homemaker = liknumLocEDUCACION()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Other',]
numLocEDUCACION.models$F3.C4$Healthy.Other = liknumLocEDUCACION()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Student',]
numLocEDUCACION.models$F3.C4$Healthy.Student = liknumLocEDUCACION()

# calculate and store degrees of freedom and log likelihood
numLocEDUCACION.models$F3.C4$LL = sum(
  sapply(numLocEDUCACION.models$F3.C4, function(mm) mm$LL))

numLocEDUCACION.models$F3.C4$k = length(numLocEDUCACION.models$F3.C4) - 2



# H:F2,C4
numLocEDUCACION.models$F2.C4 = list()

# DEN + FEV
partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) %in% c('DEN Child', 'FEV Child'),]
numLocEDUCACION.models$F2.C4$DENFEV.Child = liknumLocEDUCACION()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) %in% c('DEN Homemaker', 'FEV Homemaker'),]
numLocEDUCACION.models$F2.C4$DENFEV.Homemaker = liknumLocEDUCACION()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) %in% c('DEN Other', 'FEV Other'),]
numLocEDUCACION.models$F2.C4$DENFEV.Other = liknumLocEDUCACION()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) %in% c('DEN Student', 'FEV Student'),]
numLocEDUCACION.models$F2.C4$DENFEV.Student = liknumLocEDUCACION()

# Healthy
numLocEDUCACION.models$F2.C4$Healthy.Child = numLocEDUCACION.models$F3.C4$Healthy.Child
numLocEDUCACION.models$F2.C4$Healthy.Homemaker = numLocEDUCACION.models$F3.C4$Healthy.Homemaker
numLocEDUCACION.models$F2.C4$Healthy.Other = numLocEDUCACION.models$F3.C4$Healthy.Other
numLocEDUCACION.models$F2.C4$Healthy.Student = numLocEDUCACION.models$F3.C4$Healthy.Student

# calculate and store degress of freedom and log likelihood
numLocEDUCACION.models$F2.C4$LL = sum(
  sapply(numLocEDUCACION.models$F2.C4, function(mm) mm$LL))

numLocEDUCACION.models$F2.C4$k = length(numLocEDUCACION.models$F2.C4) - 2



# H:F1,C4
numLocEDUCACION.models$F1.C4 = list()

# DEN + FEV + Healthy
partLoc = partLoc.all[partLoc.all$category == 'Child',]
numLocEDUCACION.models$F1.C4$Child = liknumLocEDUCACION()

partLoc = partLoc.all[partLoc.all$category == 'Homemaker',]
numLocEDUCACION.models$F1.C4$Homemaker = liknumLocEDUCACION()

partLoc = partLoc.all[partLoc.all$category == 'Other',]
numLocEDUCACION.models$F1.C4$Other = liknumLocEDUCACION()

partLoc = partLoc.all[partLoc.all$category == 'Student',]
numLocEDUCACION.models$F1.C4$Student = liknumLocEDUCACION()

# calculate and store degress of freedom and log likelihood
numLocEDUCACION.models$F1.C4$LL = sum(
  sapply(numLocEDUCACION.models$F1.C4, function(mm) mm$LL))

numLocEDUCACION.models$F1.C4$k = length(numLocEDUCACION.models$F1.C4) - 2



# H:F3,C1
numLocEDUCACION.models$F3.C1 = list()

# Child + Homemaker + Other + Student
partLoc = partLoc.all[partLoc.all$pathogen == 'DEN',]
numLocEDUCACION.models$F3.C1$DEN = liknumLocEDUCACION()

partLoc = partLoc.all[partLoc.all$pathogen == 'FEV',]
numLocEDUCACION.models$F3.C1$FEV = liknumLocEDUCACION()

partLoc = partLoc.all[partLoc.all$pathogen == 'NA',]
numLocEDUCACION.models$F3.C1$Healthy = liknumLocEDUCACION()

# calculate and store degress of freedom and log likelihood
numLocEDUCACION.models$F3.C1$LL = sum(
  sapply(numLocEDUCACION.models$F3.C1, function(mm) mm$LL))

numLocEDUCACION.models$F3.C1$k = length(numLocEDUCACION.models$F3.C1) - 2



# H:F2,C1
numLocEDUCACION.models$F2.C1 = list()

# DEN + FEV + Healthy
partLoc = partLoc.all[partLoc.all$pathogen %in% c('DEN', 'FEV'),]
numLocEDUCACION.models$F2.C1$DEN = liknumLocEDUCACION()

numLocEDUCACION.models$F2.C1$Healthy = numLocEDUCACION.models$F3.C1$Healthy

# calculate and store degress of freedom and log likelihood
numLocEDUCACION.models$F2.C1$LL = sum(
  sapply(numLocEDUCACION.models$F2.C1, function(mm) mm$LL))

numLocEDUCACION.models$F2.C1$k = length(numLocEDUCACION.models$F2.C1) - 2



# H:F1,C1
numLocEDUCACION.models$F1.C1 = list()

# all
partLoc = partLoc.all
numLocEDUCACION.models$F1.C1$all = liknumLocEDUCACION()

# calculate and store degress of freedom and log likelihood
numLocEDUCACION.models$F1.C1$LL = sum(
  sapply(numLocEDUCACION.models$F1.C1, function(mm) mm$LL))

numLocEDUCACION.models$F1.C1$k = length(numLocEDUCACION.models$F1.C1) - 2




# matrices summarizing hypothesis tests

# test statistics
mat.teststat = matrix(NA,length(numLocEDUCACION.models),length(numLocEDUCACION.models))
row.names(mat.teststat) = names(numLocEDUCACION.models)
colnames(mat.teststat) = names(numLocEDUCACION.models)

mat.teststat['F3.C4','F2.C4'] =
  2 * (numLocEDUCACION.models$F3.C4$LL - numLocEDUCACION.models$F2.C4$LL)
mat.teststat['F3.C4','F1.C4'] =
  2 * (numLocEDUCACION.models$F3.C4$LL - numLocEDUCACION.models$F1.C4$LL)
mat.teststat['F3.C4','F3.C1'] =
  2 * (numLocEDUCACION.models$F3.C4$LL - numLocEDUCACION.models$F3.C1$LL)
mat.teststat['F3.C4','F2.C1'] =
  2 * (numLocEDUCACION.models$F3.C4$LL - numLocEDUCACION.models$F2.C1$LL)
mat.teststat['F3.C4','F1.C1'] =
  2 * (numLocEDUCACION.models$F3.C4$LL - numLocEDUCACION.models$F1.C1$LL)
mat.teststat['F2.C4','F1.C4'] =
  2 * (numLocEDUCACION.models$F2.C4$LL - numLocEDUCACION.models$F1.C4$LL)
mat.teststat['F2.C4','F2.C1'] =
  2 * (numLocEDUCACION.models$F2.C4$LL - numLocEDUCACION.models$F2.C1$LL)
mat.teststat['F2.C4','F1.C1'] =
  2 * (numLocEDUCACION.models$F2.C4$LL - numLocEDUCACION.models$F1.C1$LL)
mat.teststat['F1.C4','F1.C1'] =
  2 * (numLocEDUCACION.models$F1.C4$LL - numLocEDUCACION.models$F1.C1$LL)
mat.teststat['F3.C1','F2.C1'] =
  2 * (numLocEDUCACION.models$F3.C1$LL - numLocEDUCACION.models$F2.C1$LL)
mat.teststat['F3.C1','F1.C1'] =
  2 * (numLocEDUCACION.models$F3.C1$LL - numLocEDUCACION.models$F1.C1$LL)
mat.teststat['F2.C1','F1.C1'] =
  2 * (numLocEDUCACION.models$F2.C1$LL - numLocEDUCACION.models$F1.C1$LL)


# degrees of freedom
mat.df = matrix(NA,length(numLocEDUCACION.models),length(numLocEDUCACION.models))
row.names(mat.df) = names(numLocEDUCACION.models)
colnames(mat.df) = names(numLocEDUCACION.models)

mat.df['F3.C4','F2.C4'] =
  numLocEDUCACION.models$F3.C4$k - numLocEDUCACION.models$F2.C4$k
mat.df['F3.C4','F1.C4'] =
  numLocEDUCACION.models$F3.C4$k - numLocEDUCACION.models$F1.C4$k
mat.df['F3.C4','F3.C1'] =
  numLocEDUCACION.models$F3.C4$k - numLocEDUCACION.models$F3.C1$k
mat.df['F3.C4','F2.C1'] =
  numLocEDUCACION.models$F3.C4$k - numLocEDUCACION.models$F2.C1$k
mat.df['F3.C4','F1.C1'] =
  numLocEDUCACION.models$F3.C4$k - numLocEDUCACION.models$F1.C1$k
mat.df['F2.C4','F1.C4'] =
  numLocEDUCACION.models$F2.C4$k - numLocEDUCACION.models$F1.C4$k
mat.df['F2.C4','F2.C1'] =
  numLocEDUCACION.models$F2.C4$k - numLocEDUCACION.models$F2.C1$k
mat.df['F2.C4','F1.C1'] =
  numLocEDUCACION.models$F2.C4$k - numLocEDUCACION.models$F1.C1$k
mat.df['F1.C4','F1.C1'] =
  numLocEDUCACION.models$F1.C4$k - numLocEDUCACION.models$F1.C1$k
mat.df['F3.C1','F2.C1'] =
  numLocEDUCACION.models$F3.C1$k - numLocEDUCACION.models$F2.C1$k
mat.df['F3.C1','F1.C1'] =
  numLocEDUCACION.models$F3.C1$k - numLocEDUCACION.models$F1.C1$k
mat.df['F2.C1','F1.C1'] =
  numLocEDUCACION.models$F2.C1$k - numLocEDUCACION.models$F1.C1$k


# p value
mat.p = 1-pchisq(mat.teststat,mat.df)

partLoc.all = partLoc.full


