# negative log likelihood function for joint distribution of frequency and duration of visits
likTime = function(freq.mean, durn.mean, freq.std, durn.std, corr){
  -sum(dmvnorm(cbind(log(freq), log(durn)),
               c(freq.mean, durn.mean),
               matrix(c(freq.std ^ 2, corr * freq.std * durn.std,
                        corr * freq.std * durn.std, durn.std ^ 2), 2, 2), log = TRUE))
}

partLoc = partLoc.all[
  which(
    partLoc.all$landuse_class == 'TIENDAS' &
    partLoc.all$absolute_frequency != 0 &
    partLoc.all$absolute_duration != 0 &
    !is.na(partLoc.all$absolute_frequency) &
    !is.na(partLoc.all$absolute_duration)),]

timeTIENDAS.models = list()

# H:F3,C4
timeTIENDAS.models$F3.C4 = list()

# DEN
durn = partLoc$absolute_duration[paste(partLoc$pathogen,partLoc$category) == 'DEN Child']
freq = partLoc$absolute_frequency[paste(partLoc$pathogen,partLoc$category) == 'DEN Child']
timeTIENDAS.models$F3.C4$DEN.Child = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

durn = partLoc$absolute_duration[paste(partLoc$pathogen,partLoc$category) == 'DEN Homemaker']
freq = partLoc$absolute_frequency[paste(partLoc$pathogen,partLoc$category) == 'DEN Homemaker']
timeTIENDAS.models$F3.C4$DEN.Homemaker = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

durn = partLoc$absolute_duration[paste(partLoc$pathogen,partLoc$category) == 'DEN Other']
freq = partLoc$absolute_frequency[paste(partLoc$pathogen,partLoc$category) == 'DEN Other']
timeTIENDAS.models$F3.C4$DEN.Other = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

durn = partLoc$absolute_duration[paste(partLoc$pathogen,partLoc$category) == 'DEN Student']
freq = partLoc$absolute_frequency[paste(partLoc$pathogen,partLoc$category) == 'DEN Student']
timeTIENDAS.models$F3.C4$DEN.Student = mle2(
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
timeTIENDAS.models$F3.C4$FEV.Child = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

durn = partLoc$absolute_duration[paste(partLoc$pathogen,partLoc$category) == 'FEV Homemaker']
freq = partLoc$absolute_frequency[paste(partLoc$pathogen,partLoc$category) == 'FEV Homemaker']
timeTIENDAS.models$F3.C4$FEV.Homemaker = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

durn = partLoc$absolute_duration[paste(partLoc$pathogen,partLoc$category) == 'FEV Other']
freq = partLoc$absolute_frequency[paste(partLoc$pathogen,partLoc$category) == 'FEV Other']
timeTIENDAS.models$F3.C4$FEV.Other = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

durn = partLoc$absolute_duration[paste(partLoc$pathogen,partLoc$category) == 'FEV Student']
freq = partLoc$absolute_frequency[paste(partLoc$pathogen,partLoc$category) == 'FEV Student']
timeTIENDAS.models$F3.C4$FEV.Student = mle2(
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
timeTIENDAS.models$F3.C4$Healthy.Child = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

durn = partLoc$absolute_duration[paste(partLoc$pathogen,partLoc$category) == 'NA Homemaker']
freq = partLoc$absolute_frequency[paste(partLoc$pathogen,partLoc$category) == 'NA Homemaker']
timeTIENDAS.models$F3.C4$Healthy.Homemaker = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

durn = partLoc$absolute_duration[paste(partLoc$pathogen,partLoc$category) == 'NA Other']
freq = partLoc$absolute_frequency[paste(partLoc$pathogen,partLoc$category) == 'NA Other']
timeTIENDAS.models$F3.C4$Healthy.Other = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

durn = partLoc$absolute_duration[paste(partLoc$pathogen,partLoc$category) == 'NA Student']
freq = partLoc$absolute_frequency[paste(partLoc$pathogen,partLoc$category) == 'NA Student']
timeTIENDAS.models$F3.C4$Healthy.Student = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

timeTIENDAS.models$F3.C4$LL = sum(sapply(timeTIENDAS.models$F3.C4, function(ll) logLik(ll)))

timeTIENDAS.models$F3.C4$k = 5 * (length(timeTIENDAS.models$F3.C4) - 1)



# H:F2,C4
timeTIENDAS.models$F2.C4 = list()

# DEN + FEV
durn = partLoc$absolute_duration[paste(partLoc$pathogen,partLoc$category) %in% c('DEN Child', 'FEV Child')]
freq = partLoc$absolute_frequency[paste(partLoc$pathogen,partLoc$category) %in% c('DEN Child', 'FEV Child')]
timeTIENDAS.models$F2.C4$DENFEV.Child = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

durn = partLoc$absolute_duration[paste(partLoc$pathogen,partLoc$category) %in% c('DEN Homemaker', 'FEV Homemaker')]
freq = partLoc$absolute_frequency[paste(partLoc$pathogen,partLoc$category) %in% c('DEN Homemaker', 'FEV Homemaker')]
timeTIENDAS.models$F2.C4$DENFEV.Homemaker = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

durn = partLoc$absolute_duration[paste(partLoc$pathogen,partLoc$category) %in% c('DEN Other', 'FEV Other')]
freq = partLoc$absolute_frequency[paste(partLoc$pathogen,partLoc$category) %in% c('DEN Other', 'FEV Other')]
timeTIENDAS.models$F2.C4$DENFEV.Other = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

durn = partLoc$absolute_duration[paste(partLoc$pathogen,partLoc$category) %in% c('DEN Student', 'FEV Student')]
freq = partLoc$absolute_frequency[paste(partLoc$pathogen,partLoc$category) %in% c('DEN Student', 'FEV Student')]
timeTIENDAS.models$F2.C4$DENFEV.Student = mle2(
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
timeTIENDAS.models$F2.C4$Healthy.Child = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

durn = partLoc$absolute_duration[paste(partLoc$pathogen,partLoc$category) == 'NA Homemaker']
freq = partLoc$absolute_frequency[paste(partLoc$pathogen,partLoc$category) == 'NA Homemaker']
timeTIENDAS.models$F2.C4$Healthy.Homemaker = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

durn = partLoc$absolute_duration[paste(partLoc$pathogen,partLoc$category) == 'NA Other']
freq = partLoc$absolute_frequency[paste(partLoc$pathogen,partLoc$category) == 'NA Other']
timeTIENDAS.models$F2.C4$Healthy.Other = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

durn = partLoc$absolute_duration[paste(partLoc$pathogen,partLoc$category) == 'NA Student']
freq = partLoc$absolute_frequency[paste(partLoc$pathogen,partLoc$category) == 'NA Student']
timeTIENDAS.models$F2.C4$Healthy.Student = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

timeTIENDAS.models$F2.C4$LL = sum(sapply(timeTIENDAS.models$F2.C4, function(ll) logLik(ll)))

timeTIENDAS.models$F2.C4$k = 5 * (length(timeTIENDAS.models$F2.C4) - 1)



# H:F1,C4
durn = partLoc$absolute_duration[partLoc$category == 'Child']
freq = partLoc$absolute_frequency[partLoc$category == 'Child']
timeTIENDAS.models$F1.C4$Child = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

durn = partLoc$absolute_duration[partLoc$category == 'Homemaker']
freq = partLoc$absolute_frequency[partLoc$category == 'Homemaker']
timeTIENDAS.models$F1.C4$Homemaker = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

durn = partLoc$absolute_duration[partLoc$category == 'Other']
freq = partLoc$absolute_frequency[partLoc$category == 'Other']
timeTIENDAS.models$F1.C4$Other = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

durn = partLoc$absolute_duration[partLoc$category == 'Student']
freq = partLoc$absolute_frequency[partLoc$category == 'Student']
timeTIENDAS.models$F1.C4$Student = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

timeTIENDAS.models$F1.C4$LL = sum(sapply(timeTIENDAS.models$F1.C4, function(ll) logLik(ll)))

timeTIENDAS.models$F1.C4$k = 5 * (length(timeTIENDAS.models$F1.C4) - 1)



# H:F3,C1
durn = partLoc$absolute_duration[partLoc$pathogen == 'DEN']
freq = partLoc$absolute_frequency[partLoc$pathogen == 'DEN']
timeTIENDAS.models$F3.C1$DEN = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

durn = partLoc$absolute_duration[partLoc$pathogen == 'FEV']
freq = partLoc$absolute_frequency[partLoc$pathogen == 'FEV']
timeTIENDAS.models$F3.C1$FEV = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

durn = partLoc$absolute_duration[partLoc$pathogen == 'NA']
freq = partLoc$absolute_frequency[partLoc$pathogen == 'NA']
timeTIENDAS.models$F3.C1$Healthy = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

timeTIENDAS.models$F3.C1$LL = sum(sapply(timeTIENDAS.models$F3.C1, function(ll) logLik(ll)))

timeTIENDAS.models$F3.C1$k = 5 * (length(timeTIENDAS.models$F3.C1) - 1)



# H:F2,C1
durn = partLoc$absolute_duration[partLoc$pathogen %in% c('DEN', 'FEV')]
freq = partLoc$absolute_frequency[partLoc$pathogen %in% c('DEN', 'FEV')]
timeTIENDAS.models$F2.C1$DENFEV = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

durn = partLoc$absolute_duration[partLoc$pathogen == 'NA']
freq = partLoc$absolute_frequency[partLoc$pathogen == 'NA']
timeTIENDAS.models$F2.C1$Healthy = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))


timeTIENDAS.models$F2.C1$LL = sum(sapply(timeTIENDAS.models$F2.C1, function(ll) logLik(ll)))

timeTIENDAS.models$F2.C1$k = 5 * (length(timeTIENDAS.models$F2.C1) - 1)



# H:F1,C1
durn = partLoc$absolute_duration
freq = partLoc$absolute_frequency
timeTIENDAS.models$F1.C1$ALL = mle2(
  minuslogl = likTime,
  start = list(
    freq.mean = mean(log(freq)),
    durn.mean = mean(log(durn)),
    freq.std = sd(log(freq)),
    durn.std = sd(log(durn)),
    corr = 0))

timeTIENDAS.models$F1.C1$LL = sum(sapply(timeTIENDAS.models$F1.C1, function(ll) logLik(ll)))

timeTIENDAS.models$F1.C1$k = 5 * (length(timeTIENDAS.models$F1.C1) - 1)



# matrices summarizing hypothesis tests

# test statistics
mat.teststat = matrix(NA,length(timeTIENDAS.models),length(timeTIENDAS.models))
row.names(mat.teststat) = names(timeTIENDAS.models)
colnames(mat.teststat) = names(timeTIENDAS.models)

mat.teststat['F3.C4','F2.C4'] =
  2 * (timeTIENDAS.models$F3.C4$LL - timeTIENDAS.models$F2.C4$LL)
mat.teststat['F3.C4','F1.C4'] =
  2 * (timeTIENDAS.models$F3.C4$LL - timeTIENDAS.models$F1.C4$LL)
mat.teststat['F3.C4','F3.C1'] =
  2 * (timeTIENDAS.models$F3.C4$LL - timeTIENDAS.models$F3.C1$LL)
mat.teststat['F3.C4','F2.C1'] =
  2 * (timeTIENDAS.models$F3.C4$LL - timeTIENDAS.models$F2.C1$LL)
mat.teststat['F3.C4','F1.C1'] =
  2 * (timeTIENDAS.models$F3.C4$LL - timeTIENDAS.models$F1.C1$LL)
mat.teststat['F2.C4','F1.C4'] =
  2 * (timeTIENDAS.models$F2.C4$LL - timeTIENDAS.models$F1.C4$LL)
mat.teststat['F2.C4','F2.C1'] =
  2 * (timeTIENDAS.models$F2.C4$LL - timeTIENDAS.models$F2.C1$LL)
mat.teststat['F2.C4','F1.C1'] =
  2 * (timeTIENDAS.models$F2.C4$LL - timeTIENDAS.models$F1.C1$LL)
mat.teststat['F1.C4','F1.C1'] =
  2 * (timeTIENDAS.models$F1.C4$LL - timeTIENDAS.models$F1.C1$LL)
mat.teststat['F3.C1','F2.C1'] =
  2 * (timeTIENDAS.models$F3.C1$LL - timeTIENDAS.models$F2.C1$LL)
mat.teststat['F3.C1','F1.C1'] =
  2 * (timeTIENDAS.models$F3.C1$LL - timeTIENDAS.models$F1.C1$LL)
mat.teststat['F2.C1','F1.C1'] =
  2 * (timeTIENDAS.models$F2.C1$LL - timeTIENDAS.models$F1.C1$LL)


# degrees of freedom
mat.df = matrix(NA,length(timeTIENDAS.models),length(timeTIENDAS.models))
row.names(mat.df) = names(timeTIENDAS.models)
colnames(mat.df) = names(timeTIENDAS.models)

mat.df['F3.C4','F2.C4'] =
  timeTIENDAS.models$F3.C4$k - timeTIENDAS.models$F2.C4$k
mat.df['F3.C4','F1.C4'] =
  timeTIENDAS.models$F3.C4$k - timeTIENDAS.models$F1.C4$k
mat.df['F3.C4','F3.C1'] =
  timeTIENDAS.models$F3.C4$k - timeTIENDAS.models$F3.C1$k
mat.df['F3.C4','F2.C1'] =
  timeTIENDAS.models$F3.C4$k - timeTIENDAS.models$F2.C1$k
mat.df['F3.C4','F1.C1'] =
  timeTIENDAS.models$F3.C4$k - timeTIENDAS.models$F1.C1$k
mat.df['F2.C4','F1.C4'] =
  timeTIENDAS.models$F2.C4$k - timeTIENDAS.models$F1.C4$k
mat.df['F2.C4','F2.C1'] =
  timeTIENDAS.models$F2.C4$k - timeTIENDAS.models$F2.C1$k
mat.df['F2.C4','F1.C1'] =
  timeTIENDAS.models$F2.C4$k - timeTIENDAS.models$F1.C1$k
mat.df['F1.C4','F1.C1'] =
  timeTIENDAS.models$F1.C4$k - timeTIENDAS.models$F1.C1$k
mat.df['F3.C1','F2.C1'] =
  timeTIENDAS.models$F3.C1$k - timeTIENDAS.models$F2.C1$k
mat.df['F3.C1','F1.C1'] =
  timeTIENDAS.models$F3.C1$k - timeTIENDAS.models$F1.C1$k
mat.df['F2.C1','F1.C1'] =
  timeTIENDAS.models$F2.C1$k - timeTIENDAS.models$F1.C1$k


# p value
mat.p = 1-pchisq(mat.teststat,mat.df)
