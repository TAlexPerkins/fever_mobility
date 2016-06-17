liknumLocRECREACION = function(){
  numLocRECREACION.data = table(paste(partLoc$part_code,partLoc$pathogen))
  if(sum(names(numLocRECREACION.data)=='NA NA')){
    numLocRECREACION.data = 
      numLocRECREACION.data[-which(names(numLocRECREACION.data)=='NA NA')]
  }
  
  numLocRECREACION.optim = fitdistr(numLocRECREACION.data,'Poisson')
  
  return(list(
    par = as.numeric(coef(numLocRECREACION.optim)),
    LL = as.numeric(logLik(numLocRECREACION.optim))))
  
#   numLocRECREACION.optim = matrix(0,3,100)
#   
#   for(ii in 1:100){
#     numLocRECREACION.optim[1:2,ii] = c(ii, mean(numLocRECREACION.data) / (ii+mean(numLocRECREACION.data)))
#     numLocRECREACION.optim[3,ii] = sum(log(dnbinom(numLocRECREACION.data,numLocRECREACION.optim[1,ii],numLocRECREACION.optim[2,ii])))
#   }
#   
#   return(list(
#     par = numLocRECREACION.optim[1:2,which.max(numLocRECREACION.optim[3,])],
#     LL = max(numLocRECREACION.optim[3,])))
}



partLoc.full = partLoc.all
partLoc.all = partLoc.all[partLoc.all$landuse_class=='RECREACION',]



numLocRECREACION.models = list()

# H:F3,C4
numLocRECREACION.models$F3.C4 = list()

# DEN
partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Child',]
numLocRECREACION.models$F3.C4$DEN.Child = liknumLocRECREACION()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Homemaker',]
numLocRECREACION.models$F3.C4$DEN.Homemaker = liknumLocRECREACION()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Other',]
numLocRECREACION.models$F3.C4$DEN.Other = liknumLocRECREACION()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Student',]
numLocRECREACION.models$F3.C4$DEN.Student = liknumLocRECREACION()

# FEV
partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Child',]
numLocRECREACION.models$F3.C4$FEV.Child = liknumLocRECREACION()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Homemaker',]
numLocRECREACION.models$F3.C4$FEV.Homemaker = liknumLocRECREACION()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Other',]
numLocRECREACION.models$F3.C4$FEV.Other = liknumLocRECREACION()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Student',]
numLocRECREACION.models$F3.C4$FEV.Student = liknumLocRECREACION()

# Healthy
partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Child',]
numLocRECREACION.models$F3.C4$Healthy.Child = liknumLocRECREACION()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Homemaker',]
numLocRECREACION.models$F3.C4$Healthy.Homemaker = liknumLocRECREACION()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Other',]
numLocRECREACION.models$F3.C4$Healthy.Other = liknumLocRECREACION()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Student',]
numLocRECREACION.models$F3.C4$Healthy.Student = liknumLocRECREACION()

# calculate and store degrees of freedom and log likelihood
numLocRECREACION.models$F3.C4$LL = sum(
  sapply(numLocRECREACION.models$F3.C4, function(mm) mm$LL))

numLocRECREACION.models$F3.C4$k = length(numLocRECREACION.models$F3.C4) - 2



# H:F2,C4
numLocRECREACION.models$F2.C4 = list()

# DEN + FEV
partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) %in% c('DEN Child', 'FEV Child'),]
numLocRECREACION.models$F2.C4$DENFEV.Child = liknumLocRECREACION()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) %in% c('DEN Homemaker', 'FEV Homemaker'),]
numLocRECREACION.models$F2.C4$DENFEV.Homemaker = liknumLocRECREACION()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) %in% c('DEN Other', 'FEV Other'),]
numLocRECREACION.models$F2.C4$DENFEV.Other = liknumLocRECREACION()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) %in% c('DEN Student', 'FEV Student'),]
numLocRECREACION.models$F2.C4$DENFEV.Student = liknumLocRECREACION()

# Healthy
numLocRECREACION.models$F2.C4$Healthy.Child = numLocRECREACION.models$F3.C4$Healthy.Child
numLocRECREACION.models$F2.C4$Healthy.Homemaker = numLocRECREACION.models$F3.C4$Healthy.Homemaker
numLocRECREACION.models$F2.C4$Healthy.Other = numLocRECREACION.models$F3.C4$Healthy.Other
numLocRECREACION.models$F2.C4$Healthy.Student = numLocRECREACION.models$F3.C4$Healthy.Student

# calculate and store degress of freedom and log likelihood
numLocRECREACION.models$F2.C4$LL = sum(
  sapply(numLocRECREACION.models$F2.C4, function(mm) mm$LL))

numLocRECREACION.models$F2.C4$k = length(numLocRECREACION.models$F2.C4) - 2



# H:F1,C4
numLocRECREACION.models$F1.C4 = list()

# DEN + FEV + Healthy
partLoc = partLoc.all[partLoc.all$category == 'Child',]
numLocRECREACION.models$F1.C4$Child = liknumLocRECREACION()

partLoc = partLoc.all[partLoc.all$category == 'Homemaker',]
numLocRECREACION.models$F1.C4$Homemaker = liknumLocRECREACION()

partLoc = partLoc.all[partLoc.all$category == 'Other',]
numLocRECREACION.models$F1.C4$Other = liknumLocRECREACION()

partLoc = partLoc.all[partLoc.all$category == 'Student',]
numLocRECREACION.models$F1.C4$Student = liknumLocRECREACION()

# calculate and store degress of freedom and log likelihood
numLocRECREACION.models$F1.C4$LL = sum(
  sapply(numLocRECREACION.models$F1.C4, function(mm) mm$LL))

numLocRECREACION.models$F1.C4$k = length(numLocRECREACION.models$F1.C4) - 2



# H:F3,C1
numLocRECREACION.models$F3.C1 = list()

# Child + Homemaker + Other + Student
partLoc = partLoc.all[partLoc.all$pathogen == 'DEN',]
numLocRECREACION.models$F3.C1$DEN = liknumLocRECREACION()

partLoc = partLoc.all[partLoc.all$pathogen == 'FEV',]
numLocRECREACION.models$F3.C1$FEV = liknumLocRECREACION()

partLoc = partLoc.all[partLoc.all$pathogen == 'NA',]
numLocRECREACION.models$F3.C1$Healthy = liknumLocRECREACION()

# calculate and store degress of freedom and log likelihood
numLocRECREACION.models$F3.C1$LL = sum(
  sapply(numLocRECREACION.models$F3.C1, function(mm) mm$LL))

numLocRECREACION.models$F3.C1$k = length(numLocRECREACION.models$F3.C1) - 2



# H:F2,C1
numLocRECREACION.models$F2.C1 = list()

# DEN + FEV + Healthy
partLoc = partLoc.all[partLoc.all$pathogen %in% c('DEN', 'FEV'),]
numLocRECREACION.models$F2.C1$DEN = liknumLocRECREACION()

numLocRECREACION.models$F2.C1$Healthy = numLocRECREACION.models$F3.C1$Healthy

# calculate and store degress of freedom and log likelihood
numLocRECREACION.models$F2.C1$LL = sum(
  sapply(numLocRECREACION.models$F2.C1, function(mm) mm$LL))

numLocRECREACION.models$F2.C1$k = length(numLocRECREACION.models$F2.C1) - 2



# H:F1,C1
numLocRECREACION.models$F1.C1 = list()

# all
partLoc = partLoc.all
numLocRECREACION.models$F1.C1$all = liknumLocRECREACION()

# calculate and store degress of freedom and log likelihood
numLocRECREACION.models$F1.C1$LL = sum(
  sapply(numLocRECREACION.models$F1.C1, function(mm) mm$LL))

numLocRECREACION.models$F1.C1$k = length(numLocRECREACION.models$F1.C1) - 2




# matrices summarizing hypothesis tests

# test statistics
mat.teststat = matrix(NA,length(numLocRECREACION.models),length(numLocRECREACION.models))
row.names(mat.teststat) = names(numLocRECREACION.models)
colnames(mat.teststat) = names(numLocRECREACION.models)

mat.teststat['F3.C4','F2.C4'] =
  2 * (numLocRECREACION.models$F3.C4$LL - numLocRECREACION.models$F2.C4$LL)
mat.teststat['F3.C4','F1.C4'] =
  2 * (numLocRECREACION.models$F3.C4$LL - numLocRECREACION.models$F1.C4$LL)
mat.teststat['F3.C4','F3.C1'] =
  2 * (numLocRECREACION.models$F3.C4$LL - numLocRECREACION.models$F3.C1$LL)
mat.teststat['F3.C4','F2.C1'] =
  2 * (numLocRECREACION.models$F3.C4$LL - numLocRECREACION.models$F2.C1$LL)
mat.teststat['F3.C4','F1.C1'] =
  2 * (numLocRECREACION.models$F3.C4$LL - numLocRECREACION.models$F1.C1$LL)
mat.teststat['F2.C4','F1.C4'] =
  2 * (numLocRECREACION.models$F2.C4$LL - numLocRECREACION.models$F1.C4$LL)
mat.teststat['F2.C4','F2.C1'] =
  2 * (numLocRECREACION.models$F2.C4$LL - numLocRECREACION.models$F2.C1$LL)
mat.teststat['F2.C4','F1.C1'] =
  2 * (numLocRECREACION.models$F2.C4$LL - numLocRECREACION.models$F1.C1$LL)
mat.teststat['F1.C4','F1.C1'] =
  2 * (numLocRECREACION.models$F1.C4$LL - numLocRECREACION.models$F1.C1$LL)
mat.teststat['F3.C1','F2.C1'] =
  2 * (numLocRECREACION.models$F3.C1$LL - numLocRECREACION.models$F2.C1$LL)
mat.teststat['F3.C1','F1.C1'] =
  2 * (numLocRECREACION.models$F3.C1$LL - numLocRECREACION.models$F1.C1$LL)
mat.teststat['F2.C1','F1.C1'] =
  2 * (numLocRECREACION.models$F2.C1$LL - numLocRECREACION.models$F1.C1$LL)


# degrees of freedom
mat.df = matrix(NA,length(numLocRECREACION.models),length(numLocRECREACION.models))
row.names(mat.df) = names(numLocRECREACION.models)
colnames(mat.df) = names(numLocRECREACION.models)

mat.df['F3.C4','F2.C4'] =
  numLocRECREACION.models$F3.C4$k - numLocRECREACION.models$F2.C4$k
mat.df['F3.C4','F1.C4'] =
  numLocRECREACION.models$F3.C4$k - numLocRECREACION.models$F1.C4$k
mat.df['F3.C4','F3.C1'] =
  numLocRECREACION.models$F3.C4$k - numLocRECREACION.models$F3.C1$k
mat.df['F3.C4','F2.C1'] =
  numLocRECREACION.models$F3.C4$k - numLocRECREACION.models$F2.C1$k
mat.df['F3.C4','F1.C1'] =
  numLocRECREACION.models$F3.C4$k - numLocRECREACION.models$F1.C1$k
mat.df['F2.C4','F1.C4'] =
  numLocRECREACION.models$F2.C4$k - numLocRECREACION.models$F1.C4$k
mat.df['F2.C4','F2.C1'] =
  numLocRECREACION.models$F2.C4$k - numLocRECREACION.models$F2.C1$k
mat.df['F2.C4','F1.C1'] =
  numLocRECREACION.models$F2.C4$k - numLocRECREACION.models$F1.C1$k
mat.df['F1.C4','F1.C1'] =
  numLocRECREACION.models$F1.C4$k - numLocRECREACION.models$F1.C1$k
mat.df['F3.C1','F2.C1'] =
  numLocRECREACION.models$F3.C1$k - numLocRECREACION.models$F2.C1$k
mat.df['F3.C1','F1.C1'] =
  numLocRECREACION.models$F3.C1$k - numLocRECREACION.models$F1.C1$k
mat.df['F2.C1','F1.C1'] =
  numLocRECREACION.models$F2.C1$k - numLocRECREACION.models$F1.C1$k


# p value
mat.p = 1-pchisq(mat.teststat,mat.df)

partLoc.all = partLoc.full


