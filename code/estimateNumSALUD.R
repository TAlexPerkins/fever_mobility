liknumLocSALUD = function(){
  numLocSALUD.data = table(paste(partLoc$part_code,partLoc$pathogen))
  if(sum(names(numLocSALUD.data)=='NA NA')){
    numLocSALUD.data = 
      numLocSALUD.data[-which(names(numLocSALUD.data)=='NA NA')]
  }
  
  numLocSALUD.optim = fitdistr(numLocSALUD.data,'Poisson')
  
  return(list(
    par = as.numeric(coef(numLocSALUD.optim)),
    LL = as.numeric(logLik(numLocSALUD.optim))))
  
  #   numLocSALUD.optim = matrix(0,3,100)
  #   
  #   for(ii in 1:100){
  #     numLocSALUD.optim[1:2,ii] = c(ii, mean(numLocSALUD.data) / (ii+mean(numLocSALUD.data)))
  #     numLocSALUD.optim[3,ii] = sum(log(dnbinom(numLocSALUD.data,numLocSALUD.optim[1,ii],numLocSALUD.optim[2,ii])))
  #   }
  #   
  #   return(list(
  #     par = numLocSALUD.optim[1:2,which.max(numLocSALUD.optim[3,])],
  #     LL = max(numLocSALUD.optim[3,])))
}



partLoc.full = partLoc.all
partLoc.all = partLoc.all[partLoc.all$landuse_class=='SALUD',]



numLocSALUD.models = list()

# H:F3,C4
numLocSALUD.models$F3.C4 = list()

# DEN
partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Child',]
numLocSALUD.models$F3.C4$DEN.Child = liknumLocSALUD()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Homemaker',]
numLocSALUD.models$F3.C4$DEN.Homemaker = liknumLocSALUD()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Other',]
numLocSALUD.models$F3.C4$DEN.Other = liknumLocSALUD()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Student',]
numLocSALUD.models$F3.C4$DEN.Student = liknumLocSALUD()

# FEV
partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Child',]
numLocSALUD.models$F3.C4$FEV.Child = liknumLocSALUD()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Homemaker',]
numLocSALUD.models$F3.C4$FEV.Homemaker = liknumLocSALUD()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Other',]
numLocSALUD.models$F3.C4$FEV.Other = liknumLocSALUD()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Student',]
numLocSALUD.models$F3.C4$FEV.Student = liknumLocSALUD()

# Healthy
partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Child',]
numLocSALUD.models$F3.C4$Healthy.Child = liknumLocSALUD()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Homemaker',]
numLocSALUD.models$F3.C4$Healthy.Homemaker = liknumLocSALUD()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Other',]
numLocSALUD.models$F3.C4$Healthy.Other = liknumLocSALUD()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Student',]
numLocSALUD.models$F3.C4$Healthy.Student = liknumLocSALUD()

# calculate and store degrees of freedom and log likelihood
numLocSALUD.models$F3.C4$LL = sum(
  sapply(numLocSALUD.models$F3.C4, function(mm) mm$LL))

numLocSALUD.models$F3.C4$k = length(numLocSALUD.models$F3.C4) - 2



# H:F2,C4
numLocSALUD.models$F2.C4 = list()

# DEN + FEV
partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) %in% c('DEN Child', 'FEV Child'),]
numLocSALUD.models$F2.C4$DENFEV.Child = liknumLocSALUD()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) %in% c('DEN Homemaker', 'FEV Homemaker'),]
numLocSALUD.models$F2.C4$DENFEV.Homemaker = liknumLocSALUD()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) %in% c('DEN Other', 'FEV Other'),]
numLocSALUD.models$F2.C4$DENFEV.Other = liknumLocSALUD()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) %in% c('DEN Student', 'FEV Student'),]
numLocSALUD.models$F2.C4$DENFEV.Student = liknumLocSALUD()

# Healthy
numLocSALUD.models$F2.C4$Healthy.Child = numLocSALUD.models$F3.C4$Healthy.Child
numLocSALUD.models$F2.C4$Healthy.Homemaker = numLocSALUD.models$F3.C4$Healthy.Homemaker
numLocSALUD.models$F2.C4$Healthy.Other = numLocSALUD.models$F3.C4$Healthy.Other
numLocSALUD.models$F2.C4$Healthy.Student = numLocSALUD.models$F3.C4$Healthy.Student

# calculate and store degress of freedom and log likelihood
numLocSALUD.models$F2.C4$LL = sum(
  sapply(numLocSALUD.models$F2.C4, function(mm) mm$LL))

numLocSALUD.models$F2.C4$k = length(numLocSALUD.models$F2.C4) - 2



# H:F1,C4
numLocSALUD.models$F1.C4 = list()

# DEN + FEV + Healthy
partLoc = partLoc.all[partLoc.all$category == 'Child',]
numLocSALUD.models$F1.C4$Child = liknumLocSALUD()

partLoc = partLoc.all[partLoc.all$category == 'Homemaker',]
numLocSALUD.models$F1.C4$Homemaker = liknumLocSALUD()

partLoc = partLoc.all[partLoc.all$category == 'Other',]
numLocSALUD.models$F1.C4$Other = liknumLocSALUD()

partLoc = partLoc.all[partLoc.all$category == 'Student',]
numLocSALUD.models$F1.C4$Student = liknumLocSALUD()

# calculate and store degress of freedom and log likelihood
numLocSALUD.models$F1.C4$LL = sum(
  sapply(numLocSALUD.models$F1.C4, function(mm) mm$LL))

numLocSALUD.models$F1.C4$k = length(numLocSALUD.models$F1.C4) - 2



# H:F3,C1
numLocSALUD.models$F3.C1 = list()

# Child + Homemaker + Other + Student
partLoc = partLoc.all[partLoc.all$pathogen == 'DEN',]
numLocSALUD.models$F3.C1$DEN = liknumLocSALUD()

partLoc = partLoc.all[partLoc.all$pathogen == 'FEV',]
numLocSALUD.models$F3.C1$FEV = liknumLocSALUD()

partLoc = partLoc.all[partLoc.all$pathogen == 'NA',]
numLocSALUD.models$F3.C1$Healthy = liknumLocSALUD()

# calculate and store degress of freedom and log likelihood
numLocSALUD.models$F3.C1$LL = sum(
  sapply(numLocSALUD.models$F3.C1, function(mm) mm$LL))

numLocSALUD.models$F3.C1$k = length(numLocSALUD.models$F3.C1) - 2



# H:F2,C1
numLocSALUD.models$F2.C1 = list()

# DEN + FEV + Healthy
partLoc = partLoc.all[partLoc.all$pathogen %in% c('DEN', 'FEV'),]
numLocSALUD.models$F2.C1$DEN = liknumLocSALUD()

numLocSALUD.models$F2.C1$Healthy = numLocSALUD.models$F3.C1$Healthy

# calculate and store degress of freedom and log likelihood
numLocSALUD.models$F2.C1$LL = sum(
  sapply(numLocSALUD.models$F2.C1, function(mm) mm$LL))

numLocSALUD.models$F2.C1$k = length(numLocSALUD.models$F2.C1) - 2



# H:F1,C1
numLocSALUD.models$F1.C1 = list()

# all
partLoc = partLoc.all
numLocSALUD.models$F1.C1$all = liknumLocSALUD()

# calculate and store degress of freedom and log likelihood
numLocSALUD.models$F1.C1$LL = sum(
  sapply(numLocSALUD.models$F1.C1, function(mm) mm$LL))

numLocSALUD.models$F1.C1$k = length(numLocSALUD.models$F1.C1) - 2




# matrices summarizing hypothesis tests

# test statistics
mat.teststat = matrix(NA,length(numLocSALUD.models),length(numLocSALUD.models))
row.names(mat.teststat) = names(numLocSALUD.models)
colnames(mat.teststat) = names(numLocSALUD.models)

mat.teststat['F3.C4','F2.C4'] =
  2 * (numLocSALUD.models$F3.C4$LL - numLocSALUD.models$F2.C4$LL)
mat.teststat['F3.C4','F1.C4'] =
  2 * (numLocSALUD.models$F3.C4$LL - numLocSALUD.models$F1.C4$LL)
mat.teststat['F3.C4','F3.C1'] =
  2 * (numLocSALUD.models$F3.C4$LL - numLocSALUD.models$F3.C1$LL)
mat.teststat['F3.C4','F2.C1'] =
  2 * (numLocSALUD.models$F3.C4$LL - numLocSALUD.models$F2.C1$LL)
mat.teststat['F3.C4','F1.C1'] =
  2 * (numLocSALUD.models$F3.C4$LL - numLocSALUD.models$F1.C1$LL)
mat.teststat['F2.C4','F1.C4'] =
  2 * (numLocSALUD.models$F2.C4$LL - numLocSALUD.models$F1.C4$LL)
mat.teststat['F2.C4','F2.C1'] =
  2 * (numLocSALUD.models$F2.C4$LL - numLocSALUD.models$F2.C1$LL)
mat.teststat['F2.C4','F1.C1'] =
  2 * (numLocSALUD.models$F2.C4$LL - numLocSALUD.models$F1.C1$LL)
mat.teststat['F1.C4','F1.C1'] =
  2 * (numLocSALUD.models$F1.C4$LL - numLocSALUD.models$F1.C1$LL)
mat.teststat['F3.C1','F2.C1'] =
  2 * (numLocSALUD.models$F3.C1$LL - numLocSALUD.models$F2.C1$LL)
mat.teststat['F3.C1','F1.C1'] =
  2 * (numLocSALUD.models$F3.C1$LL - numLocSALUD.models$F1.C1$LL)
mat.teststat['F2.C1','F1.C1'] =
  2 * (numLocSALUD.models$F2.C1$LL - numLocSALUD.models$F1.C1$LL)


# degrees of freedom
mat.df = matrix(NA,length(numLocSALUD.models),length(numLocSALUD.models))
row.names(mat.df) = names(numLocSALUD.models)
colnames(mat.df) = names(numLocSALUD.models)

mat.df['F3.C4','F2.C4'] =
  numLocSALUD.models$F3.C4$k - numLocSALUD.models$F2.C4$k
mat.df['F3.C4','F1.C4'] =
  numLocSALUD.models$F3.C4$k - numLocSALUD.models$F1.C4$k
mat.df['F3.C4','F3.C1'] =
  numLocSALUD.models$F3.C4$k - numLocSALUD.models$F3.C1$k
mat.df['F3.C4','F2.C1'] =
  numLocSALUD.models$F3.C4$k - numLocSALUD.models$F2.C1$k
mat.df['F3.C4','F1.C1'] =
  numLocSALUD.models$F3.C4$k - numLocSALUD.models$F1.C1$k
mat.df['F2.C4','F1.C4'] =
  numLocSALUD.models$F2.C4$k - numLocSALUD.models$F1.C4$k
mat.df['F2.C4','F2.C1'] =
  numLocSALUD.models$F2.C4$k - numLocSALUD.models$F2.C1$k
mat.df['F2.C4','F1.C1'] =
  numLocSALUD.models$F2.C4$k - numLocSALUD.models$F1.C1$k
mat.df['F1.C4','F1.C1'] =
  numLocSALUD.models$F1.C4$k - numLocSALUD.models$F1.C1$k
mat.df['F3.C1','F2.C1'] =
  numLocSALUD.models$F3.C1$k - numLocSALUD.models$F2.C1$k
mat.df['F3.C1','F1.C1'] =
  numLocSALUD.models$F3.C1$k - numLocSALUD.models$F1.C1$k
mat.df['F2.C1','F1.C1'] =
  numLocSALUD.models$F2.C1$k - numLocSALUD.models$F1.C1$k


# p value
mat.p = 1-pchisq(mat.teststat,mat.df)

partLoc.all = partLoc.full



