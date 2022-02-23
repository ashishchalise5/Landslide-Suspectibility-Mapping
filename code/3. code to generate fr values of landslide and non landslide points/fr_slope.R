library(sf)
library(sp)
library(rgdal)
library(stars)
library(raster)
library(spData)
library(terra)
library(remotes)
library(maptools)
library(spatstat)
library(rpart)
library(foreign)
library(rgeos)
library(geosphere)
slope_reclassification=raster("D:/nou_rprogramming/new/final data/slope_reclassified.tif")
a=levels(slope_reclassification)[[1]]
a
a$fr=c(0,0.3195984,
       0.1950177,
       0.3383416,
       0.9969687,
       2.3754901,
       2.6008332)
a
levels(slope_reclassification)=a
levels(slope_reclassification)
slope_reclassification
#for landslide
landslide_point=st_read("D:/nou_rprogramming/new/final data/landslide_point.shp")
landslide_point
#extracting value of landslide_point
h=extract(slope_reclassification,landslide_point)
h
l=list()
l=NULL
l=append(l,h)
print(l)
m=list()
is.na(l)
m=NULL
for(i in l){
  if(i==1){
    i=0.3195984
    
  }else if(i==2){
    i= 0.1950177
    
  }else if(i==3){
    i= 0.3383416
    
    
  }else if(i==4){
    i=0.9969687

  }else if(i==5){
    i=2.3754901

  }else if(i==6){
    i=2.6008332

  }
  m=append(m,i)
}
print(m)
r=matrix(c(m),ncol=1,byrow=TRUE)
colnames(r)="FR_SLOPE"
r

i=read.csv("D:/nou_rprogramming/new/final data/m.csv")

o=cbind(i,r)
class(o)
write.csv(o,"D:/nou_rprogramming/new/final data/m.csv",append=TRUE)
#for non_landslide
non_landslide_points=st_read("D:/nou_rprogramming/new/final data/non_landslide_point3.shp")
non_landslide_points=st_transform(non_landslide_points,crs=crs(landslide_point))
h=extract(slope_reclassification,non_landslide_points)
h
l=list()
l=NULL
l=append(l,h)
print(l)
m=list()
is.na(l)
m=NULL
for(i in l){
  if(i==1){
    i=0.3195984
    
  }else if(i==2){
    i= 0.1950177
    
  }else if(i==3){
    i= 0.3383416
    
    
  }else if(i==4){
    i=0.9969687
    
  }else if(i==5){
    i=2.3754901
    
  }else if(i==6){
    i=2.6008332
    
  }
  m=append(m,i)
}
print(m)
r=matrix(c(m),ncol=1,byrow=TRUE)
colnames(r)="FR_SLOPE"
r
j=read.csv("D:/nou_rprogramming/new/final data/m5.csv")
o=cbind(j,r)
o
write.csv(o,"D:/nou_rprogramming/new/final data/m5.csv",append=TRUE)
