liknumLocINSTITUCIONES = function(){
  numLocINSTITUCIONES.data = table(paste(partLoc$part_code,partLoc$pathogen))
  if(sum(names(numLocINSTITUCIONES.data)=='NA NA')){
    numLocINSTITUCIONES.data = 
      numLocINSTITUCIONES.data[-which(names(numLocINSTITUCIONES.data)=='NA NA')]
  }
 
  numLocINSTITUCIONES.optim = fitdistr(numLocINSTITUCIONES.data,'Poisson')
  
  return(list(
    par = as.numeric(coef(numLocINSTITUCIONES.optim)),
    LL = as.numeric(logLik(numLocINSTITUCIONES.optim))))
  
#   numLocINSTITUCIONES.optim = matrix(0,3,100)
#   
#   for(ii in 1:100){
#     numLocINSTITUCIONES.optim[1:2,ii] = c(ii, mean(numLocINSTITUCIONES.data) / (ii+mean(numLocINSTITUCIONES.data)))
#     numLocINSTITUCIONES.optim[3,ii] = sum(log(dnbinom(numLocINSTITUCIONES.data,numLocINSTITUCIONES.optim[1,ii],numLocINSTITUCIONES.optim[2,ii])))
#   }
#   
#   return(list(
#     par = numLocINSTITUCIONES.optim[1:2,which.max(numLocINSTITUCIONES.optim[3,])],
#     LL = max(numLocINSTITUCIONES.optim[3,])))
}



partLoc.full = partLoc.all
partLoc.all = partLoc.all[partLoc.all$landuse_class=='INSTITUCIONES',]



numLocINSTITUCIONES.models = list()

# H:F3,C4
numLocINSTITUCIONES.models$F3.C4 = list()

# DEN
partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Child',]
numLocINSTITUCIONES.models$F3.C4$DEN.Child = liknumLocINSTITUCIONES()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Homemaker',]
numLocINSTITUCIONES.models$F3.C4$DEN.Homemaker = liknumLocINSTITUCIONES()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Other',]
numLocINSTITUCIONES.models$F3.C4$DEN.Other = liknumLocINSTITUCIONES()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Student',]
numLocINSTITUCIONES.models$F3.C4$DEN.Student = liknumLocINSTITUCIONES()

# FEV
partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Child',]
numLocINSTITUCIONES.models$F3.C4$FEV.Child = liknumLocINSTITUCIONES()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Homemaker',]
numLocINSTITUCIONES.models$F3.C4$FEV.Homemaker = liknumLocINSTITUCIONES()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Other',]
numLocINSTITUCIONES.models$F3.C4$FEV.Other = liknumLocINSTITUCIONES()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Student',]
numLocINSTITUCIONES.models$F3.C4$FEV.Student = liknumLocINSTITUCIONES()

# Healthy
partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Child',]
numLocINSTITUCIONES.models$F3.C4$Healthy.Child = liknumLocINSTITUCIONES()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Homemaker',]
numLocINSTITUCIONES.models$F3.C4$Healthy.Homemaker = liknumLocINSTITUCIONES()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Other',]
numLocINSTITUCIONES.models$F3.C4$Healthy.Other = liknumLocINSTITUCIONES()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Student',]
numLocINSTITUCIONES.models$F3.C4$Healthy.Student = liknumLocINSTITUCIONES()

# calculate and store degrees of freedom and log likelihood
numLocINSTITUCIONES.models$F3.C4$LL = sum(
  sapply(numLocINSTITUCIONES.models$F3.C4, function(mm) mm$LL))

numLocINSTITUCIONES.models$F3.C4$k = length(numLocINSTITUCIONES.models$F3.C4) - 2



# H:F2,C4
numLocINSTITUCIONES.models$F2.C4 = list()

# DEN + FEV
partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) %in% c('DEN Child', 'FEV Child'),]
numLocINSTITUCIONES.models$F2.C4$DENFEV.Child = liknumLocINSTITUCIONES()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) %in% c('DEN Homemaker', 'FEV Homemaker'),]
numLocINSTITUCIONES.models$F2.C4$DENFEV.Homemaker = liknumLocINSTITUCIONES()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) %in% c('DEN Other', 'FEV Other'),]
numLocINSTITUCIONES.models$F2.C4$DENFEV.Other = liknumLocINSTITUCIONES()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) %in% c('DEN Student', 'FEV Student'),]
numLocINSTITUCIONES.models$F2.C4$DENFEV.Student = liknumLocINSTITUCIONES()

# Healthy
numLocINSTITUCIONES.models$F2.C4$Healthy.Child = numLocINSTITUCIONES.models$F3.C4$Healthy.Child
numLocINSTITUCIONES.models$F2.C4$Healthy.Homemaker = numLocINSTITUCIONES.models$F3.C4$Healthy.Homemaker
numLocINSTITUCIONES.models$F2.C4$Healthy.Other = numLocINSTITUCIONES.models$F3.C4$Healthy.Other
numLocINSTITUCIONES.models$F2.C4$Healthy.Student = numLocINSTITUCIONES.models$F3.C4$Healthy.Student

# calculate and store degress of freedom and log likelihood
numLocINSTITUCIONES.models$F2.C4$LL = sum(
  sapply(numLocINSTITUCIONES.models$F2.C4, function(mm) mm$LL))

numLocINSTITUCIONES.models$F2.C4$k = length(numLocINSTITUCIONES.models$F2.C4) - 2



# H:F1,C4
numLocINSTITUCIONES.models$F1.C4 = list()

# DEN + FEV + Healthy
partLoc = partLoc.all[partLoc.all$category == 'Child',]
numLocINSTITUCIONES.models$F1.C4$Child = liknumLocINSTITUCIONES()

partLoc = partLoc.all[partLoc.all$category == 'Homemaker',]
numLocINSTITUCIONES.models$F1.C4$Homemaker = liknumLocINSTITUCIONES()

partLoc = partLoc.all[partLoc.all$category == 'Other',]
numLocINSTITUCIONES.models$F1.C4$Other = liknumLocINSTITUCIONES()

partLoc = partLoc.all[partLoc.all$category == 'Student',]
numLocINSTITUCIONES.models$F1.C4$Student = liknumLocINSTITUCIONES()

# calculate and store degress of freedom and log likelihood
numLocINSTITUCIONES.models$F1.C4$LL = sum(
  sapply(numLocINSTITUCIONES.models$F1.C4, function(mm) mm$LL))

numLocINSTITUCIONES.models$F1.C4$k = length(numLocINSTITUCIONES.models$F1.C4) - 2



# H:F3,C1
numLocINSTITUCIONES.models$F3.C1 = list()

# Child + Homemaker + Other + Student
partLoc = partLoc.all[partLoc.all$pathogen == 'DEN',]
numLocINSTITUCIONES.models$F3.C1$DEN = liknumLocINSTITUCIONES()

partLoc = partLoc.all[partLoc.all$pathogen == 'FEV',]
numLocINSTITUCIONES.models$F3.C1$FEV = liknumLocINSTITUCIONES()

partLoc = partLoc.all[partLoc.all$pathogen == 'NA',]
numLocINSTITUCIONES.models$F3.C1$Healthy = liknumLocINSTITUCIONES()

# calculate and store degress of freedom and log likelihood
numLocINSTITUCIONES.models$F3.C1$LL = sum(
  sapply(numLocINSTITUCIONES.models$F3.C1, function(mm) mm$LL))

numLocINSTITUCIONES.models$F3.C1$k = length(numLocINSTITUCIONES.models$F3.C1) - 2



# H:F2,C1
numLocINSTITUCIONES.models$F2.C1 = list()

# DEN + FEV + Healthy
partLoc = partLoc.all[partLoc.all$pathogen %in% c('DEN', 'FEV'),]
numLocINSTITUCIONES.models$F2.C1$DEN = liknumLocINSTITUCIONES()

numLocINSTITUCIONES.models$F2.C1$Healthy = numLocINSTITUCIONES.models$F3.C1$Healthy

# calculate and store degress of freedom and log likelihood
numLocINSTITUCIONES.models$F2.C1$LL = sum(
  sapply(numLocINSTITUCIONES.models$F2.C1, function(mm) mm$LL))

numLocINSTITUCIONES.models$F2.C1$k = length(numLocINSTITUCIONES.models$F2.C1) - 2



# H:F1,C1
numLocINSTITUCIONES.models$F1.C1 = list()

# all
partLoc = partLoc.all
numLocINSTITUCIONES.models$F1.C1$all = liknumLocINSTITUCIONES()

# calculate and store degress of freedom and log likelihood
numLocINSTITUCIONES.models$F1.C1$LL = sum(
  sapply(numLocINSTITUCIONES.models$F1.C1, function(mm) mm$LL))

numLocINSTITUCIONES.models$F1.C1$k = length(numLocINSTITUCIONES.models$F1.C1) - 2




# matrices summarizing hypothesis tests

# test statistics
mat.teststat = matrix(NA,length(numLocINSTITUCIONES.models),length(numLocINSTITUCIONES.models))
row.names(mat.teststat) = names(numLocINSTITUCIONES.models)
colnames(mat.teststat) = names(numLocINSTITUCIONES.models)

mat.teststat['F3.C4','F2.C4'] =
  2 * (numLocINSTITUCIONES.models$F3.C4$LL - numLocINSTITUCIONES.models$F2.C4$LL)
mat.teststat['F3.C4','F1.C4'] =
  2 * (numLocINSTITUCIONES.models$F3.C4$LL - numLocINSTITUCIONES.models$F1.C4$LL)
mat.teststat['F3.C4','F3.C1'] =
  2 * (numLocINSTITUCIONES.models$F3.C4$LL - numLocINSTITUCIONES.models$F3.C1$LL)
mat.teststat['F3.C4','F2.C1'] =
  2 * (numLocINSTITUCIONES.models$F3.C4$LL - numLocINSTITUCIONES.models$F2.C1$LL)
mat.teststat['F3.C4','F1.C1'] =
  2 * (numLocINSTITUCIONES.models$F3.C4$LL - numLocINSTITUCIONES.models$F1.C1$LL)
mat.teststat['F2.C4','F1.C4'] =
  2 * (numLocINSTITUCIONES.models$F2.C4$LL - numLocINSTITUCIONES.models$F1.C4$LL)
mat.teststat['F2.C4','F2.C1'] =
  2 * (numLocINSTITUCIONES.models$F2.C4$LL - numLocINSTITUCIONES.models$F2.C1$LL)
mat.teststat['F2.C4','F1.C1'] =
  2 * (numLocINSTITUCIONES.models$F2.C4$LL - numLocINSTITUCIONES.models$F1.C1$LL)
mat.teststat['F1.C4','F1.C1'] =
  2 * (numLocINSTITUCIONES.models$F1.C4$LL - numLocINSTITUCIONES.models$F1.C1$LL)
mat.teststat['F3.C1','F2.C1'] =
  2 * (numLocINSTITUCIONES.models$F3.C1$LL - numLocINSTITUCIONES.models$F2.C1$LL)
mat.teststat['F3.C1','F1.C1'] =
  2 * (numLocINSTITUCIONES.models$F3.C1$LL - numLocINSTITUCIONES.models$F1.C1$LL)
mat.teststat['F2.C1','F1.C1'] =
  2 * (numLocINSTITUCIONES.models$F2.C1$LL - numLocINSTITUCIONES.models$F1.C1$LL)


# degrees of freedom
mat.df = matrix(NA,length(numLocINSTITUCIONES.models),length(numLocINSTITUCIONES.models))
row.names(mat.df) = names(numLocINSTITUCIONES.models)
colnames(mat.df) = names(numLocINSTITUCIONES.models)

mat.df['F3.C4','F2.C4'] =
  numLocINSTITUCIONES.models$F3.C4$k - numLocINSTITUCIONES.models$F2.C4$k
mat.df['F3.C4','F1.C4'] =
  numLocINSTITUCIONES.models$F3.C4$k - numLocINSTITUCIONES.models$F1.C4$k
mat.df['F3.C4','F3.C1'] =
  numLocINSTITUCIONES.models$F3.C4$k - numLocINSTITUCIONES.models$F3.C1$k
mat.df['F3.C4','F2.C1'] =
  numLocINSTITUCIONES.models$F3.C4$k - numLocINSTITUCIONES.models$F2.C1$k
mat.df['F3.C4','F1.C1'] =
  numLocINSTITUCIONES.models$F3.C4$k - numLocINSTITUCIONES.models$F1.C1$k
mat.df['F2.C4','F1.C4'] =
  numLocINSTITUCIONES.models$F2.C4$k - numLocINSTITUCIONES.models$F1.C4$k
mat.df['F2.C4','F2.C1'] =
  numLocINSTITUCIONES.models$F2.C4$k - numLocINSTITUCIONES.models$F2.C1$k
mat.df['F2.C4','F1.C1'] =
  numLocINSTITUCIONES.models$F2.C4$k - numLocINSTITUCIONES.models$F1.C1$k
mat.df['F1.C4','F1.C1'] =
  numLocINSTITUCIONES.models$F1.C4$k - numLocINSTITUCIONES.models$F1.C1$k
mat.df['F3.C1','F2.C1'] =
  numLocINSTITUCIONES.models$F3.C1$k - numLocINSTITUCIONES.models$F2.C1$k
mat.df['F3.C1','F1.C1'] =
  numLocINSTITUCIONES.models$F3.C1$k - numLocINSTITUCIONES.models$F1.C1$k
mat.df['F2.C1','F1.C1'] =
  numLocINSTITUCIONES.models$F2.C1$k - numLocINSTITUCIONES.models$F1.C1$k


# p value
mat.p = 1-pchisq(mat.teststat,mat.df)

partLoc.all = partLoc.full


