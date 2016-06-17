# define function to normalize a vector
normalize = function(x){x / sum(x)}


# define function to find which weighting functions are most similar
which.sim = function(x){
  x1 = 1
  x2 = 2
  
  stat.min = sum(abs(dexp(bins ^ x[2, x1], x[1, x1]) - dexp(bins ^ x[2, x2], x[1, x2])))
  for(ii in 1 : ncol(x)){
    for(jj in 1 : ncol(x)){
      stat.ij = sum(abs(dexp(bins ^ x[2, ii], x[1, ii]) - dexp(bins ^ x[2, jj], x[1, jj])))
      if(ii != jj & stat.ij < stat.min){
        x1 = ii
        x2 = jj
        stat.min = stat.ij
      }
    }
  }
  
  return(list(x1, x2))
}


# index of which participant-location pairs have a distance from the participant's home
hasDist = which(!is.na(partLoc$dist_home))


# consider bins of distance from home every 10 meters
bins = seq(0, max(as.vector(dist)[!is.na(as.vector(dist))]) + 10, 10)


# define initial classification of location_types as finely as possible
location_types = sort(unique(partLoc$landuse_class[hasDist]))
eval(parse(text = paste('
                        location_types = list(',
                        paste(sort(unique(partLoc$landuse_class[hasDist])), ' = c("',
                              sort(unique(partLoc$landuse_class[hasDist])), '")', sep = '', collapse = ', '),
                        ')', sep = '')))
n = length(location_types)


hhHist = list()
for(tt in 1 : n){
  hhHist[[tt]] = matrix(0, nrow(dist), length(bins) - 1)
  for(hh in 1 : nrow(dist))
    hhHist[[tt]][hh, ] =
    hist(
      dist[hh, which(Locations$landuse_class == location_types[tt])],
      breaks = bins, plot = FALSE)$density
}
names(hhHist) = location_types


likWhere.type = function(rate, type){
  # define how distance from home affects the probability of choosing a location in a repertoire of the given location type
  probMoveDist = pexp(bins[2 : length(bins)], rate) - pexp(bins[1 : (length(bins) - 1)], rate)
  
  # establish probability of choosing a location of a certain distance from each participant home
  probLocDist = matrix(0, nrow(dist), length(probMoveDist))
  eval(parse(text = paste('
  for(hh in 1 : nrow(dist))
    probLocDist[hh, ] = normalize(hhHist$', type, '[hh, ] * probMoveDist)', sep = '')))
  
  # evaluate the negative log likelihood of each participant-location pair where the distance from home is known
  NLL = 0
  for(pl in which(partLoc$landuse_class == type)){
    lik = probLocDist[
      which(rownames(dist) == partLoc$home_code[pl])[1],
      max(which(bins <= partLoc$dist_home[pl]))]
    if(lik > 0 & !is.na(lik)){
      NLL = NLL - log(lik)
    }else{
      NLL = NLL + 20
    }
  }
  
  return(NLL)
}


likWhere.all = function(){
  par.mle = vector('list',length(location_types))
  names(par.mle) = names(location_types)

  for(ll in names(par.mle)){
    ii=1
    par.mle[[ll]] = optimize(
      function(par){likWhere.type(par,ll)},
      interval = 10^c(-ii,-ii-1))
    
    for(ii in 2:7){
      par.temp = optimize(
        function(par){likWhere.type(par,ll)},
        interval = 10^c(-ii,-ii-1))
      if(par.temp$objective < par.mle[[ll]]$objective)
        par.mle[[ll]] = par.temp
    }
  }
  
  return(par.mle)
}



# get reasonable starting points for the optimization procedure
partLoc = partLoc.all

initial.guess = rep(1e-1,length(location_types))
names(initial.guess) = names(location_types)
initial.guess.NLL = sapply(names(location_types),
  function(lt) likWhere.type(1e-1,lt))

for(ii in 2:7){
  for(lt in 1:length(location_types)){
    where.optim = optim(par=10^(-ii),function(par){likWhere.type(par,location_types[[lt]])})
    if(where.optim$value < initial.guess.NLL[lt]){
      initial.guess[lt] = where.optim$par
      initial.guess.NLL[lt] = where.optim$value
    }
  }
}




where.models = list()

# H:F3,C4
where.models$F3.C4 = list()

# DEN
partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Child',]
where.models$F3.C4$DEN.Child = likWhere.all()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Homemaker',]
where.models$F3.C4$DEN.Homemaker = likWhere.all()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Other',]
where.models$F3.C4$DEN.Other = likWhere.all()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'DEN Student',]
where.models$F3.C4$DEN.Student = likWhere.all()

# FEV
partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Child',]
where.models$F3.C4$FEV.Child = likWhere.all()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Homemaker',]
where.models$F3.C4$FEV.Homemaker = likWhere.all()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Other',]
where.models$F3.C4$FEV.Other = likWhere.all()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'FEV Student',]
where.models$F3.C4$FEV.Student = likWhere.all()

# Healthy
partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Child',]
where.models$F3.C4$Healthy.Child = likWhere.all()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Homemaker',]
where.models$F3.C4$Healthy.Homemaker = likWhere.all()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Other',]
where.models$F3.C4$Healthy.Other = likWhere.all()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) == 'NA Student',]
where.models$F3.C4$Healthy.Student = likWhere.all()

# calculate and store degress of freedom and log likelihood
where.models$F3.C4$LL = sum(
  sapply(where.models$F3.C4, function(mm)
    sum(sapply(mm, function(ll) -ll$objective))))

where.models$F3.C4$k = sum(
  sapply(where.models$F3.C4, function(mm) length(mm))) - 1



# H:F2,C4
where.models$F2.C4 = list()

# DEN + FEV
partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) %in% c('DEN Child', 'FEV Child'),]
where.models$F2.C4$DENFEV.Child = likWhere.all()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) %in% c('DEN Homemaker', 'FEV Homemaker'),]
where.models$F2.C4$DENFEV.Homemaker = likWhere.all()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) %in% c('DEN Other', 'FEV Other'),]
where.models$F2.C4$DENFEV.Other = likWhere.all()

partLoc = partLoc.all[paste(partLoc.all$pathogen,partLoc.all$category) %in% c('DEN Student', 'FEV Student'),]
where.models$F2.C4$DENFEV.Student = likWhere.all()

# Healthy
where.models$F2.C4$Healthy.Child = where.models$F3.C4$Healthy.Child
where.models$F2.C4$Healthy.Homemaker = where.models$F3.C4$Healthy.Homemaker
where.models$F2.C4$Healthy.Other = where.models$F3.C4$Healthy.Other
where.models$F2.C4$Healthy.Student = where.models$F3.C4$Healthy.Student

# calculate and store degress of freedom and log likelihood
where.models$F2.C4$LL = sum(
  sapply(where.models$F2.C4, function(mm)
    sum(sapply(mm, function(ll) -ll$objective))))

where.models$F2.C4$k = sum(
  sapply(where.models$F2.C4, function(mm) length(mm))) - 1



# H:F1,C4
where.models$F1.C4 = list()

# DEN + FEV + Healthy
partLoc = partLoc.all[partLoc.all$category == 'Child',]
where.models$F1.C4$Child = likWhere.all()

partLoc = partLoc.all[partLoc.all$category == 'Homemaker',]
where.models$F1.C4$Homemaker = likWhere.all()

partLoc = partLoc.all[partLoc.all$category == 'Other',]
where.models$F1.C4$Other = likWhere.all()

partLoc = partLoc.all[partLoc.all$category == 'Student',]
where.models$F1.C4$Student = likWhere.all()

# calculate and store degress of freedom and log likelihood
where.models$F1.C4$LL = sum(
  sapply(where.models$F1.C4, function(mm)
    sum(sapply(mm, function(ll) -ll$objective))))

where.models$F1.C4$k = sum(
  sapply(where.models$F1.C4, function(mm) length(mm))) - 1



# H:F3,C1
where.models$F3.C1 = list()

# Child + Homemaker + Other + Student
partLoc = partLoc.all[partLoc.all$pathogen == 'DEN',]
where.models$F3.C1$DEN = likWhere.all()

partLoc = partLoc.all[partLoc.all$pathogen == 'FEV',]
where.models$F3.C1$FEV = likWhere.all()

partLoc = partLoc.all[partLoc.all$pathogen == 'NA',]
where.models$F3.C1$Healthy = likWhere.all()

# calculate and store degress of freedom and log likelihood
where.models$F3.C1$LL = sum(
  sapply(where.models$F3.C1, function(mm)
    sum(sapply(mm, function(ll) -ll$objective))))

where.models$F3.C1$k = sum(
  sapply(where.models$F3.C1, function(mm) length(mm))) - 1



# H:F2,C1
where.models$F2.C1 = list()

# DEN + FEV + Healthy
partLoc = partLoc.all[partLoc.all$pathogen %in% c('DEN', 'FEV'),]
where.models$F2.C1$DEN = likWhere.all()

where.models$F2.C1$Healthy = where.models$F3.C1$Healthy

# calculate and store degress of freedom and log likelihood
where.models$F2.C1$LL = sum(
  sapply(where.models$F2.C1, function(mm)
    sum(sapply(mm, function(ll) -ll$objective))))

where.models$F2.C1$k = sum(
  sapply(where.models$F2.C1, function(mm) length(mm))) - 1



# H:F1,C1
where.models$F1.C1 = list()

# all
partLoc = partLoc.all
where.models$F1.C1$all = likWhere.all()

# calculate and store degress of freedom and log likelihood
where.models$F1.C1$LL = sum(
  sapply(where.models$F1.C1, function(mm)
    sum(sapply(mm, function(ll) -ll$objective))))

where.models$F1.C1$k = sum(
  sapply(where.models$F1.C1, function(mm) length(mm))) - 1




# matrices summarizing hypothesis tests

# test statistics
mat.teststat = matrix(NA,length(where.models),length(where.models))
row.names(mat.teststat) = names(where.models)
colnames(mat.teststat) = names(where.models)

mat.teststat['F3.C4','F2.C4'] =
  2 * (where.models$F3.C4$LL - where.models$F2.C4$LL)
mat.teststat['F3.C4','F1.C4'] =
  2 * (where.models$F3.C4$LL - where.models$F1.C4$LL)
mat.teststat['F3.C4','F3.C1'] =
  2 * (where.models$F3.C4$LL - where.models$F3.C1$LL)
mat.teststat['F3.C4','F2.C1'] =
  2 * (where.models$F3.C4$LL - where.models$F2.C1$LL)
mat.teststat['F3.C4','F1.C1'] =
  2 * (where.models$F3.C4$LL - where.models$F1.C1$LL)
mat.teststat['F2.C4','F1.C4'] =
  2 * (where.models$F2.C4$LL - where.models$F1.C4$LL)
mat.teststat['F2.C4','F2.C1'] =
  2 * (where.models$F2.C4$LL - where.models$F2.C1$LL)
mat.teststat['F2.C4','F1.C1'] =
  2 * (where.models$F2.C4$LL - where.models$F1.C1$LL)
mat.teststat['F1.C4','F1.C1'] =
  2 * (where.models$F1.C4$LL - where.models$F1.C1$LL)
mat.teststat['F3.C1','F2.C1'] =
  2 * (where.models$F3.C1$LL - where.models$F2.C1$LL)
mat.teststat['F3.C1','F1.C1'] =
  2 * (where.models$F3.C1$LL - where.models$F1.C1$LL)
mat.teststat['F2.C1','F1.C1'] =
  2 * (where.models$F2.C1$LL - where.models$F1.C1$LL)


# degrees of freedom
mat.df = matrix(NA,length(where.models),length(where.models))
row.names(mat.df) = names(where.models)
colnames(mat.df) = names(where.models)

mat.df['F3.C4','F2.C4'] =
  where.models$F3.C4$k - where.models$F2.C4$k
mat.df['F3.C4','F1.C4'] =
  where.models$F3.C4$k - where.models$F1.C4$k
mat.df['F3.C4','F3.C1'] =
  where.models$F3.C4$k - where.models$F3.C1$k
mat.df['F3.C4','F2.C1'] =
  where.models$F3.C4$k - where.models$F2.C1$k
mat.df['F3.C4','F1.C1'] =
  where.models$F3.C4$k - where.models$F1.C1$k
mat.df['F2.C4','F1.C4'] =
  where.models$F2.C4$k - where.models$F1.C4$k
mat.df['F2.C4','F2.C1'] =
  where.models$F2.C4$k - where.models$F2.C1$k
mat.df['F2.C4','F1.C1'] =
  where.models$F2.C4$k - where.models$F1.C1$k
mat.df['F1.C4','F1.C1'] =
  where.models$F1.C4$k - where.models$F1.C1$k
mat.df['F3.C1','F2.C1'] =
  where.models$F3.C1$k - where.models$F2.C1$k
mat.df['F3.C1','F1.C1'] =
  where.models$F3.C1$k - where.models$F1.C1$k
mat.df['F2.C1','F1.C1'] =
  where.models$F2.C1$k - where.models$F1.C1$k


# p value
mat.p = 1-pchisq(mat.teststat,mat.df)
