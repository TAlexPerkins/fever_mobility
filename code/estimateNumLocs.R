likNumLoc = function(){
  numLoc.data = table(paste(partLoc$part_code,partLoc$pathogen))

  numLoc.optim = matrix(0,3,100)
  
  for(ii in 1:100){
    numLoc.optim[1:2,ii] = c(ii, mean(numLoc.data) / (ii+mean(numLoc.data)))
    numLoc.optim[3,ii] = sum(log(dnbinom(numLoc.data,numLoc.optim[1,ii],numLoc.optim[2,ii])))
  }
  
  return(list(
    par = numLoc.optim[1:2,which.max(numLoc.optim[3,])],
    LL = max(numLoc.optim[3,])))
}





numLoc.models = list()

# H:F3,C4
numLoc.models$F3.C4 = list()

# DEN
partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Child',]
numLoc.models$F3.C4$DEN.Child = likNumLoc()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Homemaker',]
numLoc.models$F3.C4$DEN.Homemaker = likNumLoc()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Other',]
numLoc.models$F3.C4$DEN.Other = likNumLoc()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Student',]
numLoc.models$F3.C4$DEN.Student = likNumLoc()

# FEV
partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Child',]
numLoc.models$F3.C4$FEV.Child = likNumLoc()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Homemaker',]
numLoc.models$F3.C4$FEV.Homemaker = likNumLoc()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Other',]
numLoc.models$F3.C4$FEV.Other = likNumLoc()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Student',]
numLoc.models$F3.C4$FEV.Student = likNumLoc()

# Healthy
partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Child',]
numLoc.models$F3.C4$Healthy.Child = likNumLoc()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Homemaker',]
numLoc.models$F3.C4$Healthy.Homemaker = likNumLoc()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Other',]
numLoc.models$F3.C4$Healthy.Other = likNumLoc()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Student',]
numLoc.models$F3.C4$Healthy.Student = likNumLoc()

# calculate and store degress of freedom and log likelihood
numLoc.models$F3.C4$LL = sum(
  sapply(numLoc.models$F3.C4, function(mm) mm$LL))

numLoc.models$F3.C4$k = 2 * (length(numLoc.models$F3.C4) - 1)



# H:F2,C4
numLoc.models$F2.C4 = list()

# DEN + FEV
partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) %in% c('DEN Child', 'FEV Child'),]
numLoc.models$F2.C4$DENFEV.Child = likNumLoc()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) %in% c('DEN Homemaker', 'FEV Homemaker'),]
numLoc.models$F2.C4$DENFEV.Homemaker = likNumLoc()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) %in% c('DEN Other', 'FEV Other'),]
numLoc.models$F2.C4$DENFEV.Other = likNumLoc()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) %in% c('DEN Student', 'FEV Student'),]
numLoc.models$F2.C4$DENFEV.Student = likNumLoc()

# Healthy
numLoc.models$F2.C4$Healthy.Child = numLoc.models$F3.C4$Healthy.Child
numLoc.models$F2.C4$Healthy.Homemaker = numLoc.models$F3.C4$Healthy.Homemaker
numLoc.models$F2.C4$Healthy.Other = numLoc.models$F3.C4$Healthy.Other
numLoc.models$F2.C4$Healthy.Student = numLoc.models$F3.C4$Healthy.Student

# calculate and store degress of freedom and log likelihood
numLoc.models$F2.C4$LL = sum(
  sapply(numLoc.models$F2.C4, function(mm) mm$LL))

numLoc.models$F2.C4$k = 2 * (length(numLoc.models$F2.C4) - 1)



# H:F1,C4
numLoc.models$F1.C4 = list()

# DEN + FEV + Healthy
partLoc = partLoc.all[partLoc.all$category == 'Child',]
numLoc.models$F1.C4$Child = likNumLoc()

partLoc = partLoc.all[partLoc.all$category == 'Homemaker',]
numLoc.models$F1.C4$Homemaker = likNumLoc()

partLoc = partLoc.all[partLoc.all$category == 'Other',]
numLoc.models$F1.C4$Other = likNumLoc()

partLoc = partLoc.all[partLoc.all$category == 'Student',]
numLoc.models$F1.C4$Student = likNumLoc()

# calculate and store degress of freedom and log likelihood
numLoc.models$F1.C4$LL = sum(
  sapply(numLoc.models$F1.C4, function(mm) mm$LL))

numLoc.models$F1.C4$k = 2 * (length(numLoc.models$F1.C4) - 1)



# H:F3,C1
numLoc.models$F3.C1 = list()

# Child + Homemaker + Other + Student
partLoc = partLoc.all[partLoc.all$pathogen == 'DEN',]
numLoc.models$F3.C1$DEN = likNumLoc()

partLoc = partLoc.all[partLoc.all$pathogen == 'FEV',]
numLoc.models$F3.C1$FEV = likNumLoc()

partLoc = partLoc.all[partLoc.all$pathogen == 'NA',]
numLoc.models$F3.C1$Healthy = likNumLoc()

# calculate and store degress of freedom and log likelihood
numLoc.models$F3.C1$LL = sum(
  sapply(numLoc.models$F3.C1, function(mm) mm$LL))

numLoc.models$F3.C1$k = 2 * (length(numLoc.models$F3.C1) - 1)



# H:F2,C1
numLoc.models$F2.C1 = list()

# DEN + FEV + Healthy
partLoc = partLoc.all[partLoc.all$pathogen %in% c('DEN', 'FEV'),]
numLoc.models$F2.C1$DEN = likNumLoc()

numLoc.models$F2.C1$Healthy = numLoc.models$F3.C1$Healthy

# calculate and store degress of freedom and log likelihood
numLoc.models$F2.C1$LL = sum(
  sapply(numLoc.models$F2.C1, function(mm) mm$LL))

numLoc.models$F2.C1$k = 2 * (length(numLoc.models$F2.C1) - 1)



# H:F1,C1
numLoc.models$F1.C1 = list()

# all
partLoc = partLoc.all
numLoc.models$F1.C1$all = likNumLoc()

# calculate and store degress of freedom and log likelihood
numLoc.models$F1.C1$LL = sum(
  sapply(numLoc.models$F1.C1, function(mm) mm$LL))

numLoc.models$F1.C1$k = 2 * (length(numLoc.models$F1.C1) - 1)




# matrices summarizing hypothesis tests

# test statistics
mat.teststat = matrix(NA,length(numLoc.models),length(numLoc.models))
row.names(mat.teststat) = names(numLoc.models)
colnames(mat.teststat) = names(numLoc.models)

mat.teststat['F3.C4','F2.C4'] =
  2 * (numLoc.models$F3.C4$LL - numLoc.models$F2.C4$LL)
mat.teststat['F3.C4','F1.C4'] =
  2 * (numLoc.models$F3.C4$LL - numLoc.models$F1.C4$LL)
mat.teststat['F3.C4','F3.C1'] =
  2 * (numLoc.models$F3.C4$LL - numLoc.models$F3.C1$LL)
mat.teststat['F3.C4','F2.C1'] =
  2 * (numLoc.models$F3.C4$LL - numLoc.models$F2.C1$LL)
mat.teststat['F3.C4','F1.C1'] =
  2 * (numLoc.models$F3.C4$LL - numLoc.models$F1.C1$LL)
mat.teststat['F2.C4','F1.C4'] =
  2 * (numLoc.models$F2.C4$LL - numLoc.models$F1.C4$LL)
mat.teststat['F2.C4','F2.C1'] =
  2 * (numLoc.models$F2.C4$LL - numLoc.models$F2.C1$LL)
mat.teststat['F2.C4','F1.C1'] =
  2 * (numLoc.models$F2.C4$LL - numLoc.models$F1.C1$LL)
mat.teststat['F1.C4','F1.C1'] =
  2 * (numLoc.models$F1.C4$LL - numLoc.models$F1.C1$LL)
mat.teststat['F3.C1','F2.C1'] =
  2 * (numLoc.models$F3.C1$LL - numLoc.models$F2.C1$LL)
mat.teststat['F3.C1','F1.C1'] =
  2 * (numLoc.models$F3.C1$LL - numLoc.models$F1.C1$LL)
mat.teststat['F2.C1','F1.C1'] =
  2 * (numLoc.models$F2.C1$LL - numLoc.models$F1.C1$LL)


# degrees of freedom
mat.df = matrix(NA,length(numLoc.models),length(numLoc.models))
row.names(mat.df) = names(numLoc.models)
colnames(mat.df) = names(numLoc.models)

mat.df['F3.C4','F2.C4'] =
  numLoc.models$F3.C4$k - numLoc.models$F2.C4$k
mat.df['F3.C4','F1.C4'] =
  numLoc.models$F3.C4$k - numLoc.models$F1.C4$k
mat.df['F3.C4','F3.C1'] =
  numLoc.models$F3.C4$k - numLoc.models$F3.C1$k
mat.df['F3.C4','F2.C1'] =
  numLoc.models$F3.C4$k - numLoc.models$F2.C1$k
mat.df['F3.C4','F1.C1'] =
  numLoc.models$F3.C4$k - numLoc.models$F1.C1$k
mat.df['F2.C4','F1.C4'] =
  numLoc.models$F2.C4$k - numLoc.models$F1.C4$k
mat.df['F2.C4','F2.C1'] =
  numLoc.models$F2.C4$k - numLoc.models$F2.C1$k
mat.df['F2.C4','F1.C1'] =
  numLoc.models$F2.C4$k - numLoc.models$F1.C1$k
mat.df['F1.C4','F1.C1'] =
  numLoc.models$F1.C4$k - numLoc.models$F1.C1$k
mat.df['F3.C1','F2.C1'] =
  numLoc.models$F3.C1$k - numLoc.models$F2.C1$k
mat.df['F3.C1','F1.C1'] =
  numLoc.models$F3.C1$k - numLoc.models$F1.C1$k
mat.df['F2.C1','F1.C1'] =
  numLoc.models$F2.C1$k - numLoc.models$F1.C1$k


# p value
mat.p = 1-pchisq(mat.teststat,mat.df)
