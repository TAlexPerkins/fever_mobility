# negative log likelihood function for joint distribution of frequency and duration of visits home
likTimeHome = function(freq.mean, durn.mean, freq.std, durn.std, corr){
  -sum(dmvnorm(cbind(log(freqHome), log(durnHome)),
               c(freq.mean, durn.mean),
               matrix(c(freq.std ^ 2, corr * freq.std * durn.std,
                        corr * freq.std * durn.std, durn.std ^ 2), 2, 2), log = TRUE))
}

timeHome.models = list()

# H:F3,C4
timeHome.models$F3.C4 = list()

# DEN
durnHome = partHome$durnHome[paste(partHome$pathogen,partHome$category) == 'DEN Child']
freqHome = partHome$freqHome[paste(partHome$pathogen,partHome$category) == 'DEN Child']
timeHome.models$F3.C4$DEN.Child = mle2(
  minuslogl = likTimeHome,
  start = list(
    freq.mean = mean(log(freqHome)),
    durn.mean = mean(log(durnHome)),
    freq.std = sd(log(freqHome)),
    durn.std = sd(log(durnHome)),
    corr = 0))

durnHome = partHome$durnHome[paste(partHome$pathogen,partHome$category) == 'DEN Homemaker']
freqHome = partHome$freqHome[paste(partHome$pathogen,partHome$category) == 'DEN Homemaker']
timeHome.models$F3.C4$DEN.Homemaker = mle2(
  minuslogl = likTimeHome,
  start = list(
    freq.mean = mean(log(freqHome)),
    durn.mean = mean(log(durnHome)),
    freq.std = sd(log(freqHome)),
    durn.std = sd(log(durnHome)),
    corr = 0))

durnHome = partHome$durnHome[paste(partHome$pathogen,partHome$category) == 'DEN Other']
freqHome = partHome$freqHome[paste(partHome$pathogen,partHome$category) == 'DEN Other']
timeHome.models$F3.C4$DEN.Other = mle2(
  minuslogl = likTimeHome,
  start = list(
    freq.mean = mean(log(freqHome)),
    durn.mean = mean(log(durnHome)),
    freq.std = sd(log(freqHome)),
    durn.std = sd(log(durnHome)),
    corr = 0))

durnHome = partHome$durnHome[paste(partHome$pathogen,partHome$category) == 'DEN Student']
freqHome = partHome$freqHome[paste(partHome$pathogen,partHome$category) == 'DEN Student']
timeHome.models$F3.C4$DEN.Student = mle2(
  minuslogl = likTimeHome,
  start = list(
    freq.mean = mean(log(freqHome)),
    durn.mean = mean(log(durnHome)),
    freq.std = sd(log(freqHome)),
    durn.std = sd(log(durnHome)),
    corr = 0))

# FEV
durnHome = partHome$durnHome[paste(partHome$pathogen,partHome$category) == 'FEV Child']
freqHome = partHome$freqHome[paste(partHome$pathogen,partHome$category) == 'FEV Child']
timeHome.models$F3.C4$FEV.Child = mle2(
  minuslogl = likTimeHome,
  start = list(
    freq.mean = mean(log(freqHome)),
    durn.mean = mean(log(durnHome)),
    freq.std = sd(log(freqHome)),
    durn.std = sd(log(durnHome)),
    corr = 0))

durnHome = partHome$durnHome[paste(partHome$pathogen,partHome$category) == 'FEV Homemaker']
freqHome = partHome$freqHome[paste(partHome$pathogen,partHome$category) == 'FEV Homemaker']
timeHome.models$F3.C4$FEV.Homemaker = mle2(
  minuslogl = likTimeHome,
  start = list(
    freq.mean = mean(log(freqHome)),
    durn.mean = mean(log(durnHome)),
    freq.std = sd(log(freqHome)),
    durn.std = sd(log(durnHome)),
    corr = 0))

durnHome = partHome$durnHome[paste(partHome$pathogen,partHome$category) == 'FEV Other']
freqHome = partHome$freqHome[paste(partHome$pathogen,partHome$category) == 'FEV Other']
timeHome.models$F3.C4$FEV.Other = mle2(
  minuslogl = likTimeHome,
  start = list(
    freq.mean = mean(log(freqHome)),
    durn.mean = mean(log(durnHome)),
    freq.std = sd(log(freqHome)),
    durn.std = sd(log(durnHome)),
    corr = 0))

durnHome = partHome$durnHome[paste(partHome$pathogen,partHome$category) == 'FEV Student']
freqHome = partHome$freqHome[paste(partHome$pathogen,partHome$category) == 'FEV Student']
timeHome.models$F3.C4$FEV.Student = mle2(
  minuslogl = likTimeHome,
  start = list(
    freq.mean = mean(log(freqHome)),
    durn.mean = mean(log(durnHome)),
    freq.std = sd(log(freqHome)),
    durn.std = sd(log(durnHome)),
    corr = 0))

# Healthy
durnHome = partHome$durnHome[paste(partHome$pathogen,partHome$category) == 'NA Child']
freqHome = partHome$freqHome[paste(partHome$pathogen,partHome$category) == 'NA Child']
timeHome.models$F3.C4$Healthy.Child = mle2(
  minuslogl = likTimeHome,
  start = list(
    freq.mean = mean(log(freqHome)),
    durn.mean = mean(log(durnHome)),
    freq.std = sd(log(freqHome)),
    durn.std = sd(log(durnHome)),
    corr = 0))

durnHome = partHome$durnHome[paste(partHome$pathogen,partHome$category) == 'NA Homemaker']
freqHome = partHome$freqHome[paste(partHome$pathogen,partHome$category) == 'NA Homemaker']
timeHome.models$F3.C4$Healthy.Homemaker = mle2(
  minuslogl = likTimeHome,
  start = list(
    freq.mean = mean(log(freqHome)),
    durn.mean = mean(log(durnHome)),
    freq.std = sd(log(freqHome)),
    durn.std = sd(log(durnHome)),
    corr = 0))

durnHome = partHome$durnHome[paste(partHome$pathogen,partHome$category) == 'NA Other']
freqHome = partHome$freqHome[paste(partHome$pathogen,partHome$category) == 'NA Other']
timeHome.models$F3.C4$Healthy.Other = mle2(
  minuslogl = likTimeHome,
  start = list(
    freq.mean = mean(log(freqHome)),
    durn.mean = mean(log(durnHome)),
    freq.std = sd(log(freqHome)),
    durn.std = sd(log(durnHome)),
    corr = 0))

durnHome = partHome$durnHome[paste(partHome$pathogen,partHome$category) == 'NA Student']
freqHome = partHome$freqHome[paste(partHome$pathogen,partHome$category) == 'NA Student']
timeHome.models$F3.C4$Healthy.Student = mle2(
  minuslogl = likTimeHome,
  start = list(
    freq.mean = mean(log(freqHome)),
    durn.mean = mean(log(durnHome)),
    freq.std = sd(log(freqHome)),
    durn.std = sd(log(durnHome)),
    corr = 0))

timeHome.models$F3.C4$LL = sum(sapply(timeHome.models$F3.C4, function(ll) logLik(ll)))

timeHome.models$F3.C4$k = 5 * (length(timeHome.models$F3.C4) - 1)



# H:F2,C4
timeHome.models$F2.C4 = list()

# DEN + FEV
durnHome = partHome$durnHome[paste(partHome$pathogen,partHome$category) %in% c('DEN Child', 'FEV Child')]
freqHome = partHome$freqHome[paste(partHome$pathogen,partHome$category) %in% c('DEN Child', 'FEV Child')]
timeHome.models$F2.C4$DENFEV.Child = mle2(
  minuslogl = likTimeHome,
  start = list(
    freq.mean = mean(log(freqHome)),
    durn.mean = mean(log(durnHome)),
    freq.std = sd(log(freqHome)),
    durn.std = sd(log(durnHome)),
    corr = 0))

durnHome = partHome$durnHome[paste(partHome$pathogen,partHome$category) %in% c('DEN Homemaker', 'FEV Homemaker')]
freqHome = partHome$freqHome[paste(partHome$pathogen,partHome$category) %in% c('DEN Homemaker', 'FEV Homemaker')]
timeHome.models$F2.C4$DENFEV.Homemaker = mle2(
  minuslogl = likTimeHome,
  start = list(
    freq.mean = mean(log(freqHome)),
    durn.mean = mean(log(durnHome)),
    freq.std = sd(log(freqHome)),
    durn.std = sd(log(durnHome)),
    corr = 0))

durnHome = partHome$durnHome[paste(partHome$pathogen,partHome$category) %in% c('DEN Other', 'FEV Other')]
freqHome = partHome$freqHome[paste(partHome$pathogen,partHome$category) %in% c('DEN Other', 'FEV Other')]
timeHome.models$F2.C4$DENFEV.Other = mle2(
  minuslogl = likTimeHome,
  start = list(
    freq.mean = mean(log(freqHome)),
    durn.mean = mean(log(durnHome)),
    freq.std = sd(log(freqHome)),
    durn.std = sd(log(durnHome)),
    corr = 0))

durnHome = partHome$durnHome[paste(partHome$pathogen,partHome$category) %in% c('DEN Student', 'FEV Student')]
freqHome = partHome$freqHome[paste(partHome$pathogen,partHome$category) %in% c('DEN Student', 'FEV Student')]
timeHome.models$F2.C4$DENFEV.Student = mle2(
  minuslogl = likTimeHome,
  start = list(
    freq.mean = mean(log(freqHome)),
    durn.mean = mean(log(durnHome)),
    freq.std = sd(log(freqHome)),
    durn.std = sd(log(durnHome)),
    corr = 0))

# Healthy
durnHome = partHome$durnHome[paste(partHome$pathogen,partHome$category) == 'NA Child']
freqHome = partHome$freqHome[paste(partHome$pathogen,partHome$category) == 'NA Child']
timeHome.models$F2.C4$Healthy.Child = mle2(
  minuslogl = likTimeHome,
  start = list(
    freq.mean = mean(log(freqHome)),
    durn.mean = mean(log(durnHome)),
    freq.std = sd(log(freqHome)),
    durn.std = sd(log(durnHome)),
    corr = 0))

durnHome = partHome$durnHome[paste(partHome$pathogen,partHome$category) == 'NA Homemaker']
freqHome = partHome$freqHome[paste(partHome$pathogen,partHome$category) == 'NA Homemaker']
timeHome.models$F2.C4$Healthy.Homemaker = mle2(
  minuslogl = likTimeHome,
  start = list(
    freq.mean = mean(log(freqHome)),
    durn.mean = mean(log(durnHome)),
    freq.std = sd(log(freqHome)),
    durn.std = sd(log(durnHome)),
    corr = 0))

durnHome = partHome$durnHome[paste(partHome$pathogen,partHome$category) == 'NA Other']
freqHome = partHome$freqHome[paste(partHome$pathogen,partHome$category) == 'NA Other']
timeHome.models$F2.C4$Healthy.Other = mle2(
  minuslogl = likTimeHome,
  start = list(
    freq.mean = mean(log(freqHome)),
    durn.mean = mean(log(durnHome)),
    freq.std = sd(log(freqHome)),
    durn.std = sd(log(durnHome)),
    corr = 0))

durnHome = partHome$durnHome[paste(partHome$pathogen,partHome$category) == 'NA Student']
freqHome = partHome$freqHome[paste(partHome$pathogen,partHome$category) == 'NA Student']
timeHome.models$F2.C4$Healthy.Student = mle2(
  minuslogl = likTimeHome,
  start = list(
    freq.mean = mean(log(freqHome)),
    durn.mean = mean(log(durnHome)),
    freq.std = sd(log(freqHome)),
    durn.std = sd(log(durnHome)),
    corr = 0))

timeHome.models$F2.C4$LL = sum(sapply(timeHome.models$F2.C4, function(ll) logLik(ll)))

timeHome.models$F2.C4$k = 5 * (length(timeHome.models$F2.C4) - 1)



# H:F1,C4
durnHome = partHome$durnHome[partHome$category == 'Child']
freqHome = partHome$freqHome[partHome$category == 'Child']
timeHome.models$F1.C4$Child = mle2(
  minuslogl = likTimeHome,
  start = list(
    freq.mean = mean(log(freqHome)),
    durn.mean = mean(log(durnHome)),
    freq.std = sd(log(freqHome)),
    durn.std = sd(log(durnHome)),
    corr = 0))

durnHome = partHome$durnHome[partHome$category == 'Homemaker']
freqHome = partHome$freqHome[partHome$category == 'Homemaker']
timeHome.models$F1.C4$Homemaker = mle2(
  minuslogl = likTimeHome,
  start = list(
    freq.mean = mean(log(freqHome)),
    durn.mean = mean(log(durnHome)),
    freq.std = sd(log(freqHome)),
    durn.std = sd(log(durnHome)),
    corr = 0))

durnHome = partHome$durnHome[partHome$category == 'Other']
freqHome = partHome$freqHome[partHome$category == 'Other']
timeHome.models$F1.C4$Other = mle2(
  minuslogl = likTimeHome,
  start = list(
    freq.mean = mean(log(freqHome)),
    durn.mean = mean(log(durnHome)),
    freq.std = sd(log(freqHome)),
    durn.std = sd(log(durnHome)),
    corr = 0))

durnHome = partHome$durnHome[partHome$category == 'Student']
freqHome = partHome$freqHome[partHome$category == 'Student']
timeHome.models$F1.C4$Student = mle2(
  minuslogl = likTimeHome,
  start = list(
    freq.mean = mean(log(freqHome)),
    durn.mean = mean(log(durnHome)),
    freq.std = sd(log(freqHome)),
    durn.std = sd(log(durnHome)),
    corr = 0))

timeHome.models$F1.C4$LL = sum(sapply(timeHome.models$F1.C4, function(ll) logLik(ll)))

timeHome.models$F1.C4$k = 5 * (length(timeHome.models$F1.C4) - 1)



# H:F3,C1
durnHome = partHome$durnHome[partHome$pathogen == 'DEN']
freqHome = partHome$freqHome[partHome$pathogen == 'DEN']
timeHome.models$F3.C1$DEN = mle2(
  minuslogl = likTimeHome,
  start = list(
    freq.mean = mean(log(freqHome)),
    durn.mean = mean(log(durnHome)),
    freq.std = sd(log(freqHome)),
    durn.std = sd(log(durnHome)),
    corr = 0))

durnHome = partHome$durnHome[partHome$pathogen == 'FEV']
freqHome = partHome$freqHome[partHome$pathogen == 'FEV']
timeHome.models$F3.C1$FEV = mle2(
  minuslogl = likTimeHome,
  start = list(
    freq.mean = mean(log(freqHome)),
    durn.mean = mean(log(durnHome)),
    freq.std = sd(log(freqHome)),
    durn.std = sd(log(durnHome)),
    corr = 0))

durnHome = partHome$durnHome[partHome$pathogen == 'NA']
freqHome = partHome$freqHome[partHome$pathogen == 'NA']
timeHome.models$F3.C1$Healthy = mle2(
  minuslogl = likTimeHome,
  start = list(
    freq.mean = mean(log(freqHome)),
    durn.mean = mean(log(durnHome)),
    freq.std = sd(log(freqHome)),
    durn.std = sd(log(durnHome)),
    corr = 0))

timeHome.models$F3.C1$LL = sum(sapply(timeHome.models$F3.C1, function(ll) logLik(ll)))

timeHome.models$F3.C1$k = 5 * (length(timeHome.models$F3.C1) - 1)



# H:F2,C1
durnHome = partHome$durnHome[partHome$pathogen %in% c('DEN', 'FEV')]
freqHome = partHome$freqHome[partHome$pathogen %in% c('DEN', 'FEV')]
timeHome.models$F2.C1$DENFEV = mle2(
  minuslogl = likTimeHome,
  start = list(
    freq.mean = mean(log(freqHome)),
    durn.mean = mean(log(durnHome)),
    freq.std = sd(log(freqHome)),
    durn.std = sd(log(durnHome)),
    corr = 0))

durnHome = partHome$durnHome[partHome$pathogen == 'NA']
freqHome = partHome$freqHome[partHome$pathogen == 'NA']
timeHome.models$F2.C1$Healthy = mle2(
  minuslogl = likTimeHome,
  start = list(
    freq.mean = mean(log(freqHome)),
    durn.mean = mean(log(durnHome)),
    freq.std = sd(log(freqHome)),
    durn.std = sd(log(durnHome)),
    corr = 0))


timeHome.models$F2.C1$LL = sum(sapply(timeHome.models$F2.C1, function(ll) logLik(ll)))

timeHome.models$F2.C1$k = 5 * (length(timeHome.models$F2.C1) - 1)



# H:F1,C1
durnHome = partHome$durnHome
freqHome = partHome$freqHome
timeHome.models$F1.C1$ALL = mle2(
  minuslogl = likTimeHome,
  start = list(
    freq.mean = mean(log(freqHome)),
    durn.mean = mean(log(durnHome)),
    freq.std = sd(log(freqHome)),
    durn.std = sd(log(durnHome)),
    corr = 0))

timeHome.models$F1.C1$LL = sum(sapply(timeHome.models$F1.C1, function(ll) logLik(ll)))

timeHome.models$F1.C1$k = 5 * (length(timeHome.models$F1.C1) - 1)



# matrices summarizing hypothesis tests

# test statistics
mat.teststat = matrix(NA,length(timeHome.models),length(timeHome.models))
row.names(mat.teststat) = names(timeHome.models)
colnames(mat.teststat) = names(timeHome.models)

mat.teststat['F3.C4','F2.C4'] =
  2 * (timeHome.models$F3.C4$LL - timeHome.models$F2.C4$LL)
mat.teststat['F3.C4','F1.C4'] =
  2 * (timeHome.models$F3.C4$LL - timeHome.models$F1.C4$LL)
mat.teststat['F3.C4','F3.C1'] =
  2 * (timeHome.models$F3.C4$LL - timeHome.models$F3.C1$LL)
mat.teststat['F3.C4','F2.C1'] =
  2 * (timeHome.models$F3.C4$LL - timeHome.models$F2.C1$LL)
mat.teststat['F3.C4','F1.C1'] =
  2 * (timeHome.models$F3.C4$LL - timeHome.models$F1.C1$LL)
mat.teststat['F2.C4','F1.C4'] =
  2 * (timeHome.models$F2.C4$LL - timeHome.models$F1.C4$LL)
mat.teststat['F2.C4','F2.C1'] =
  2 * (timeHome.models$F2.C4$LL - timeHome.models$F2.C1$LL)
mat.teststat['F2.C4','F1.C1'] =
  2 * (timeHome.models$F2.C4$LL - timeHome.models$F1.C1$LL)
mat.teststat['F1.C4','F1.C1'] =
  2 * (timeHome.models$F1.C4$LL - timeHome.models$F1.C1$LL)
mat.teststat['F3.C1','F2.C1'] =
  2 * (timeHome.models$F3.C1$LL - timeHome.models$F2.C1$LL)
mat.teststat['F3.C1','F1.C1'] =
  2 * (timeHome.models$F3.C1$LL - timeHome.models$F1.C1$LL)
mat.teststat['F2.C1','F1.C1'] =
  2 * (timeHome.models$F2.C1$LL - timeHome.models$F1.C1$LL)


# degrees of freedom
mat.df = matrix(NA,length(timeHome.models),length(timeHome.models))
row.names(mat.df) = names(timeHome.models)
colnames(mat.df) = names(timeHome.models)

mat.df['F3.C4','F2.C4'] =
  timeHome.models$F3.C4$k - timeHome.models$F2.C4$k
mat.df['F3.C4','F1.C4'] =
  timeHome.models$F3.C4$k - timeHome.models$F1.C4$k
mat.df['F3.C4','F3.C1'] =
  timeHome.models$F3.C4$k - timeHome.models$F3.C1$k
mat.df['F3.C4','F2.C1'] =
  timeHome.models$F3.C4$k - timeHome.models$F2.C1$k
mat.df['F3.C4','F1.C1'] =
  timeHome.models$F3.C4$k - timeHome.models$F1.C1$k
mat.df['F2.C4','F1.C4'] =
  timeHome.models$F2.C4$k - timeHome.models$F1.C4$k
mat.df['F2.C4','F2.C1'] =
  timeHome.models$F2.C4$k - timeHome.models$F2.C1$k
mat.df['F2.C4','F1.C1'] =
  timeHome.models$F2.C4$k - timeHome.models$F1.C1$k
mat.df['F1.C4','F1.C1'] =
  timeHome.models$F1.C4$k - timeHome.models$F1.C1$k
mat.df['F3.C1','F2.C1'] =
  timeHome.models$F3.C1$k - timeHome.models$F2.C1$k
mat.df['F3.C1','F1.C1'] =
  timeHome.models$F3.C1$k - timeHome.models$F1.C1$k
mat.df['F2.C1','F1.C1'] =
  timeHome.models$F2.C1$k - timeHome.models$F1.C1$k


# p value
mat.p = 1-pchisq(mat.teststat,mat.df)
