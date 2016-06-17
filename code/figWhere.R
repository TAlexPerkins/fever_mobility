
location_types.eng = c(
  HOUSE = 'Residential',
  TIENDAS = 'Commercial',
  RECREACION = 'Recreation',
  EDUCACION = 'Education',
  SALUD = 'Health',
  INSTITUCIONES = 'Institutions',
  IGLESIAS = 'Religion',
  OTROS = 'Others')


pdf('../output/where_mle.pdf',width=6.5,height=3.25)

par(mar = c(2, 3, 2, .5), oma = c(0, 2, 0, 0))
layout(matrix(1 : 8, 2, 4, byrow = TRUE))

for(lt in 1:length(location_types)){
  bar.heights = matrix(0,3,4)
  
  bar.heights[1,1] = where.models$F3.C4$Healthy.Child[[lt]]$minimum
  bar.heights[2,1] = where.models$F3.C4$DEN.Child[[lt]]$minimum
  bar.heights[3,1] = where.models$F3.C4$FEV.Child[[lt]]$minimum
  bar.heights[1,2] = where.models$F3.C4$Healthy.Student[[lt]]$minimum
  bar.heights[2,2] = where.models$F3.C4$DEN.Student[[lt]]$minimum
  bar.heights[3,2] = where.models$F3.C4$FEV.Student[[lt]]$minimum
  bar.heights[1,3] = where.models$F3.C4$Healthy.Homemaker[[lt]]$minimum
  bar.heights[2,3] = where.models$F3.C4$DEN.Homemaker[[lt]]$minimum
  bar.heights[3,3] = where.models$F3.C4$FEV.Homemaker[[lt]]$minimum
  bar.heights[1,4] = where.models$F3.C4$Healthy.Other[[lt]]$minimum
  bar.heights[2,4] = where.models$F3.C4$DEN.Other[[lt]]$minimum
  bar.heights[3,4] = where.models$F3.C4$FEV.Other[[lt]]$minimum

  barplot(
    bar.heights,
    beside=T,
    col=c(rgb(0,0,1,.5),rgb(0,1,0,.5),rgb(1,0,0,.5)))

  mtext(location_types.eng[lt],side=3,line=.5,adj=0,cex=.8)
  
#   if(lt > 4){
    mtext('C',side=1,line=1,at=2.5,cex=.8)
    mtext('S',side=1,line=1,at=6.5,cex=.8)
    mtext('H',side=1,line=1,at=10.5,cex=.8)
    mtext('W',side=1,line=1,at=14.5,cex=.8)
#   }
  
  if(lt == 1){
    mtext(
      'MLE parameter value',
      side=2,line=3,at=-.00025,cex=.8)
  }
}

dev.off()



pdf('../output/where_mean.pdf',width=6.5,height=3.25)

par(mar = c(2, 3, 2, .5), oma = c(0, 2, 0, 0))
layout(matrix(1 : 8, 2, 4, byrow = TRUE))

for(lt in 1:length(location_types)){
  bar.heights = matrix(0,3,4)

  dists = partLoc.all$dist_home[
    partLoc.all$pathogen == 'NA' &
      partLoc.all$category == 'Child' &
      partLoc.all$landuse_class == location_types[lt]]
  bar.heights[1,1] = mean(dists[!is.na(dists)])
  dists = partLoc.all$dist_home[
    partLoc.all$pathogen == 'DEN' &
      partLoc.all$category == 'Child' &
      partLoc.all$landuse_class == location_types[lt]]
  bar.heights[2,1] = mean(dists[!is.na(dists)])
  dists = partLoc.all$dist_home[
    partLoc.all$pathogen == 'FEV' &
      partLoc.all$category == 'Child' &
      partLoc.all$landuse_class == location_types[lt]]
  bar.heights[3,1] = mean(dists[!is.na(dists)])
  dists = partLoc.all$dist_home[
    partLoc.all$pathogen == 'NA' &
      partLoc.all$category == 'Student' &
      partLoc.all$landuse_class == location_types[lt]]
  bar.heights[1,2] = mean(dists[!is.na(dists)])
  dists = partLoc.all$dist_home[
    partLoc.all$pathogen == 'DEN' &
      partLoc.all$category == 'Student' &
      partLoc.all$landuse_class == location_types[lt]]
  bar.heights[2,2] = mean(dists[!is.na(dists)])
  dists = partLoc.all$dist_home[
    partLoc.all$pathogen == 'FEV' &
      partLoc.all$category == 'Student' &
      partLoc.all$landuse_class == location_types[lt]]
  bar.heights[3,2] = mean(dists[!is.na(dists)])
  dists = partLoc.all$dist_home[
    partLoc.all$pathogen == 'NA' &
      partLoc.all$category == 'Homemaker' &
      partLoc.all$landuse_class == location_types[lt]]
  bar.heights[1,3] = mean(dists[!is.na(dists)])
  dists = partLoc.all$dist_home[
    partLoc.all$pathogen == 'DEN' &
      partLoc.all$category == 'Homemaker' &
      partLoc.all$landuse_class == location_types[lt]]
  bar.heights[2,3] = mean(dists[!is.na(dists)])
  dists = partLoc.all$dist_home[
    partLoc.all$pathogen == 'FEV' &
      partLoc.all$category == 'Homemaker' &
      partLoc.all$landuse_class == location_types[lt]]
  bar.heights[3,3] = mean(dists[!is.na(dists)])
  dists = partLoc.all$dist_home[
    partLoc.all$pathogen == 'NA' &
      partLoc.all$category == 'Other' &
      partLoc.all$landuse_class == location_types[lt]]
  bar.heights[1,4] = mean(dists[!is.na(dists)])
  dists = partLoc.all$dist_home[
    partLoc.all$pathogen == 'DEN' &
      partLoc.all$category == 'Other' &
      partLoc.all$landuse_class == location_types[lt]]
  bar.heights[2,4] = mean(dists[!is.na(dists)])
  dists = partLoc.all$dist_home[
    partLoc.all$pathogen == 'FEV' &
      partLoc.all$category == 'Other' &
      partLoc.all$landuse_class == location_types[lt]]
  bar.heights[3,4] = mean(dists[!is.na(dists)])
  
  barplot(
    bar.heights,
    beside=T,
    col=c(rgb(0,0,1,.5),rgb(0,1,0,.5),rgb(1,0,0,.5)))
  
  mtext(location_types.eng[lt],side=3,line=.5,adj=0,cex=.8)
  
#   if(lt > 4){
    mtext('C',side=1,line=1,at=2.5,cex=.8)
    mtext('S',side=1,line=1,at=6.5,cex=.8)
    mtext('H',side=1,line=1,at=10.5,cex=.8)
    mtext('W',side=1,line=1,at=14.5,cex=.8)
#   }
  
  if(lt == 1){
    mtext(
      'Mean distance from home',
      side=2,line=3,at=-.00025,cex=.8)
  }
}

dev.off()
