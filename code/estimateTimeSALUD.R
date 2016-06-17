# negative log likelihood function for joint distribution of frequency and duration of visits
likTime = function(freq.mean, durn.mean, freq.std, durn.std, corr){
  -sum(dmvnorm(cbind(log(freq), log(durn)),
               c(freq.mean, durn.mean),
               matrix(c(freq.std ^ 2, corr * freq.std * durn.std,
                        corr * freq.std * durn.std, durn.std ^ 2), 2, 2), log = TRUE))
}

partLoc = partLoc.all[
  which(
    partLoc.all$landuse_class == 'SALUD' &
    partLoc.all$absolute_frequency != 0 &
    partLoc.all$absolute_duration != 0 &
    !is.na(partLoc.all$absolute_frequency) &
    !is.na(partLoc.all$absolute_duration)),]

timeSALUD.models = list()

# H:F3,C4
timeSALUD.models$F3.C4 = list()

# DEN
durn = partLoc$absolute_duration[paste(partLoc$pathogen,partLoc$category) == 'DEN Child']
freq = partLoc$absolute_frequency[paste(partLoc$pathogen,partLoc$category) == 'DEN Child']
timeSALUD.models$F3.C4$DEN.Child = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

durn = partLoc$absolute_duration[paste(partLoc$pathogen,partLoc$category) == 'DEN Homemaker']
freq = partLoc$absolute_frequency[paste(partLoc$pathogen,partLoc$category) == 'DEN Homemaker']
timeSALUD.models$F3.C4$DEN.Homemaker = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

durn = partLoc$absolute_duration[paste(partLoc$pathogen,partLoc$category) == 'DEN Other']
freq = partLoc$absolute_frequency[paste(partLoc$pathogen,partLoc$category) == 'DEN Other']
timeSALUD.models$F3.C4$DEN.Other = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

durn = partLoc$absolute_duration[paste(partLoc$pathogen,partLoc$category) == 'DEN Student']
freq = partLoc$absolute_frequency[paste(partLoc$pathogen,partLoc$category) == 'DEN Student']
timeSALUD.models$F3.C4$DEN.Student = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

# FEV
durn = partLoc$absolute_duration[paste(partLoc$pathogen,partLoc$category) == 'FEV Child']
freq = partLoc$absolute_frequency[paste(partLoc$pathogen,partLoc$category) == 'FEV Child']
timeSALUD.models$F3.C4$FEV.Child = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

durn = partLoc$absolute_duration[paste(partLoc$pathogen,partLoc$category) == 'FEV Homemaker']
freq = partLoc$absolute_frequency[paste(partLoc$pathogen,partLoc$category) == 'FEV Homemaker']
timeSALUD.models$F3.C4$FEV.Homemaker = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

durn = partLoc$absolute_duration[paste(partLoc$pathogen,partLoc$category) == 'FEV Other']
freq = partLoc$absolute_frequency[paste(partLoc$pathogen,partLoc$category) == 'FEV Other']
timeSALUD.models$F3.C4$FEV.Other = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

durn = partLoc$absolute_duration[paste(partLoc$pathogen,partLoc$category) == 'FEV Student']
freq = partLoc$absolute_frequency[paste(partLoc$pathogen,partLoc$category) == 'FEV Student']
timeSALUD.models$F3.C4$FEV.Student = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

# Healthy
durn = partLoc$absolute_duration[paste(partLoc$pathogen,partLoc$category) == 'NA Child']
freq = partLoc$absolute_frequency[paste(partLoc$pathogen,partLoc$category) == 'NA Child']
timeSALUD.models$F3.C4$Healthy.Child = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

durn = partLoc$absolute_duration[paste(partLoc$pathogen,partLoc$category) == 'NA Homemaker']
freq = partLoc$absolute_frequency[paste(partLoc$pathogen,partLoc$category) == 'NA Homemaker']
timeSALUD.models$F3.C4$Healthy.Homemaker = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

durn = partLoc$absolute_duration[paste(partLoc$pathogen,partLoc$category) == 'NA Other']
freq = partLoc$absolute_frequency[paste(partLoc$pathogen,partLoc$category) == 'NA Other']
timeSALUD.models$F3.C4$Healthy.Other = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

durn = partLoc$absolute_duration[paste(partLoc$pathogen,partLoc$category) == 'NA Student']
freq = partLoc$absolute_frequency[paste(partLoc$pathogen,partLoc$category) == 'NA Student']
timeSALUD.models$F3.C4$Healthy.Student = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

timeSALUD.models$F3.C4$LL = sum(sapply(timeSALUD.models$F3.C4, function(ll) logLik(ll)))

timeSALUD.models$F3.C4$k = 5 * (length(timeSALUD.models$F3.C4) - 1)



# H:F2,C4
timeSALUD.models$F2.C4 = list()

# DEN + FEV
durn = partLoc$absolute_duration[paste(partLoc$pathogen,partLoc$category) %in% c('DEN Child', 'FEV Child')]
freq = partLoc$absolute_frequency[paste(partLoc$pathogen,partLoc$category) %in% c('DEN Child', 'FEV Child')]
timeSALUD.models$F2.C4$DENFEV.Child = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

durn = partLoc$absolute_duration[paste(partLoc$pathogen,partLoc$category) %in% c('DEN Homemaker', 'FEV Homemaker')]
freq = partLoc$absolute_frequency[paste(partLoc$pathogen,partLoc$category) %in% c('DEN Homemaker', 'FEV Homemaker')]
timeSALUD.models$F2.C4$DENFEV.Homemaker = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

durn = partLoc$absolute_duration[paste(partLoc$pathogen,partLoc$category) %in% c('DEN Other', 'FEV Other')]
freq = partLoc$absolute_frequency[paste(partLoc$pathogen,partLoc$category) %in% c('DEN Other', 'FEV Other')]
timeSALUD.models$F2.C4$DENFEV.Other = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

durn = partLoc$absolute_duration[paste(partLoc$pathogen,partLoc$category) %in% c('DEN Student', 'FEV Student')]
freq = partLoc$absolute_frequency[paste(partLoc$pathogen,partLoc$category) %in% c('DEN Student', 'FEV Student')]
timeSALUD.models$F2.C4$DENFEV.Student = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

# Healthy
durn = partLoc$absolute_duration[paste(partLoc$pathogen,partLoc$category) == 'NA Child']
freq = partLoc$absolute_frequency[paste(partLoc$pathogen,partLoc$category) == 'NA Child']
timeSALUD.models$F2.C4$Healthy.Child = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

durn = partLoc$absolute_duration[paste(partLoc$pathogen,partLoc$category) == 'NA Homemaker']
freq = partLoc$absolute_frequency[paste(partLoc$pathogen,partLoc$category) == 'NA Homemaker']
timeSALUD.models$F2.C4$Healthy.Homemaker = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

durn = partLoc$absolute_duration[paste(partLoc$pathogen,partLoc$category) == 'NA Other']
freq = partLoc$absolute_frequency[paste(partLoc$pathogen,partLoc$category) == 'NA Other']
timeSALUD.models$F2.C4$Healthy.Other = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

durn = partLoc$absolute_duration[paste(partLoc$pathogen,partLoc$category) == 'NA Student']
freq = partLoc$absolute_frequency[paste(partLoc$pathogen,partLoc$category) == 'NA Student']
timeSALUD.models$F2.C4$Healthy.Student = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

timeSALUD.models$F2.C4$LL = sum(sapply(timeSALUD.models$F2.C4, function(ll) logLik(ll)))

timeSALUD.models$F2.C4$k = 5 * (length(timeSALUD.models$F2.C4) - 1)



# H:F1,C4
durn = partLoc$absolute_duration[partLoc$category == 'Child']
freq = partLoc$absolute_frequency[partLoc$category == 'Child']
timeSALUD.models$F1.C4$Child = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

durn = partLoc$absolute_duration[partLoc$category == 'Homemaker']
freq = partLoc$absolute_frequency[partLoc$category == 'Homemaker']
timeSALUD.models$F1.C4$Homemaker = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

durn = partLoc$absolute_duration[partLoc$category == 'Other']
freq = partLoc$absolute_frequency[partLoc$category == 'Other']
timeSALUD.models$F1.C4$Other = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

durn = partLoc$absolute_duration[partLoc$category == 'Student']
freq = partLoc$absolute_frequency[partLoc$category == 'Student']
timeSALUD.models$F1.C4$Student = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

timeSALUD.models$F1.C4$LL = sum(sapply(timeSALUD.models$F1.C4, function(ll) logLik(ll)))

timeSALUD.models$F1.C4$k = 5 * (length(timeSALUD.models$F1.C4) - 1)



# H:F3,C1
durn = partLoc$absolute_duration[partLoc$pathogen == 'DEN']
freq = partLoc$absolute_frequency[partLoc$pathogen == 'DEN']
timeSALUD.models$F3.C1$DEN = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

durn = partLoc$absolute_duration[partLoc$pathogen == 'FEV']
freq = partLoc$absolute_frequency[partLoc$pathogen == 'FEV']
timeSALUD.models$F3.C1$FEV = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

durn = partLoc$absolute_duration[partLoc$pathogen == 'NA']
freq = partLoc$absolute_frequency[partLoc$pathogen == 'NA']
timeSALUD.models$F3.C1$Healthy = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

timeSALUD.models$F3.C1$LL = sum(sapply(timeSALUD.models$F3.C1, function(ll) logLik(ll)))

timeSALUD.models$F3.C1$k = 5 * (length(timeSALUD.models$F3.C1) - 1)



# H:F2,C1
durn = partLoc$absolute_duration[partLoc$pathogen %in% c('DEN', 'FEV')]
freq = partLoc$absolute_frequency[partLoc$pathogen %in% c('DEN', 'FEV')]
timeSALUD.models$F2.C1$DENFEV = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

durn = partLoc$absolute_duration[partLoc$pathogen == 'NA']
freq = partLoc$absolute_frequency[partLoc$pathogen == 'NA']
timeSALUD.models$F2.C1$Healthy = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))


timeSALUD.models$F2.C1$LL = sum(sapply(timeSALUD.models$F2.C1, function(ll) logLik(ll)))

timeSALUD.models$F2.C1$k = 5 * (length(timeSALUD.models$F2.C1) - 1)



# H:F1,C1
durn = partLoc$absolute_duration
freq = partLoc$absolute_frequency
timeSALUD.models$F1.C1$ALL = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

timeSALUD.models$F1.C1$LL = sum(sapply(timeSALUD.models$F1.C1, function(ll) logLik(ll)))

timeSALUD.models$F1.C1$k = 5 * (length(timeSALUD.models$F1.C1) - 1)



# matrices summarizing hypothesis tests

# test statistics
mat.teststat = matrix(NA,length(timeSALUD.models),length(timeSALUD.models))
row.names(mat.teststat) = names(timeSALUD.models)
colnames(mat.teststat) = names(timeSALUD.models)

mat.teststat['F3.C4','F2.C4'] =
  2 * (timeSALUD.models$F3.C4$LL - timeSALUD.models$F2.C4$LL)
mat.teststat['F3.C4','F1.C4'] =
  2 * (timeSALUD.models$F3.C4$LL - timeSALUD.models$F1.C4$LL)
mat.teststat['F3.C4','F3.C1'] =
  2 * (timeSALUD.models$F3.C4$LL - timeSALUD.models$F3.C1$LL)
mat.teststat['F3.C4','F2.C1'] =
  2 * (timeSALUD.models$F3.C4$LL - timeSALUD.models$F2.C1$LL)
mat.teststat['F3.C4','F1.C1'] =
  2 * (timeSALUD.models$F3.C4$LL - timeSALUD.models$F1.C1$LL)
mat.teststat['F2.C4','F1.C4'] =
  2 * (timeSALUD.models$F2.C4$LL - timeSALUD.models$F1.C4$LL)
mat.teststat['F2.C4','F2.C1'] =
  2 * (timeSALUD.models$F2.C4$LL - timeSALUD.models$F2.C1$LL)
mat.teststat['F2.C4','F1.C1'] =
  2 * (timeSALUD.models$F2.C4$LL - timeSALUD.models$F1.C1$LL)
mat.teststat['F1.C4','F1.C1'] =
  2 * (timeSALUD.models$F1.C4$LL - timeSALUD.models$F1.C1$LL)
mat.teststat['F3.C1','F2.C1'] =
  2 * (timeSALUD.models$F3.C1$LL - timeSALUD.models$F2.C1$LL)
mat.teststat['F3.C1','F1.C1'] =
  2 * (timeSALUD.models$F3.C1$LL - timeSALUD.models$F1.C1$LL)
mat.teststat['F2.C1','F1.C1'] =
  2 * (timeSALUD.models$F2.C1$LL - timeSALUD.models$F1.C1$LL)


# degrees of freedom
mat.df = matrix(NA,length(timeSALUD.models),length(timeSALUD.models))
row.names(mat.df) = names(timeSALUD.models)
colnames(mat.df) = names(timeSALUD.models)

mat.df['F3.C4','F2.C4'] =
  timeSALUD.models$F3.C4$k - timeSALUD.models$F2.C4$k
mat.df['F3.C4','F1.C4'] =
  timeSALUD.models$F3.C4$k - timeSALUD.models$F1.C4$k
mat.df['F3.C4','F3.C1'] =
  timeSALUD.models$F3.C4$k - timeSALUD.models$F3.C1$k
mat.df['F3.C4','F2.C1'] =
  timeSALUD.models$F3.C4$k - timeSALUD.models$F2.C1$k
mat.df['F3.C4','F1.C1'] =
  timeSALUD.models$F3.C4$k - timeSALUD.models$F1.C1$k
mat.df['F2.C4','F1.C4'] =
  timeSALUD.models$F2.C4$k - timeSALUD.models$F1.C4$k
mat.df['F2.C4','F2.C1'] =
  timeSALUD.models$F2.C4$k - timeSALUD.models$F2.C1$k
mat.df['F2.C4','F1.C1'] =
  timeSALUD.models$F2.C4$k - timeSALUD.models$F1.C1$k
mat.df['F1.C4','F1.C1'] =
  timeSALUD.models$F1.C4$k - timeSALUD.models$F1.C1$k
mat.df['F3.C1','F2.C1'] =
  timeSALUD.models$F3.C1$k - timeSALUD.models$F2.C1$k
mat.df['F3.C1','F1.C1'] =
  timeSALUD.models$F3.C1$k - timeSALUD.models$F1.C1$k
mat.df['F2.C1','F1.C1'] =
  timeSALUD.models$F2.C1$k - timeSALUD.models$F1.C1$k


# p value
mat.p = 1-pchisq(mat.teststat,mat.df)
