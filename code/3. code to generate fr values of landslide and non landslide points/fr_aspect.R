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
aspect_reclassification=raster("D:/nou_rprogramming/new/final data/aspect_reclassified.tif")
a=levels(aspect_reclassification)[[1]]
a
a$fr=c(0,0.3694509,
       0.3144547,
       0.8341093,
       1.6127546,
       2.6913475,
       1.4867935,
       0.434473,
       0.1010573)
a
levels(aspect_reclassification)=a
levels(aspect_reclassification)
aspect_reclassification
#for landslide
landslide_point=st_read("D:/nou_rprogramming/new/final data/landslide_point.shp")
landslide_point
#extracting value of landslide_point
h=extract(aspect_reclassification,landslide_point)
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
    i=0.3694509
    
  }else if(i==2){
    i= 0.3144547
    
  }else if(i==3){
    i= 0.8341093
    
    
  }else if(i==4){
    i=1.6127546
  }else if(i==5){
    i=2.6913475
  }else if(i==6){
    i=1.4867935
  }else if(i==7){
    i=0.434473
  }else if(i==8){
    i=0.1010573
  }
  m=append(m,i)
}
print(m)
r=matrix(c(m),ncol=1,byrow=TRUE)
colnames(r)="FR_ASPECT"
r

i=read.csv("D:/nou_rprogramming/new/final data/m.csv")

o=cbind(i,r)
class(o)
write.csv(o,"D:/nou_rprogramming/new/final data/m.csv",append=TRUE)
#for non_landslide
non_landslide_points=st_read("D:/nou_rprogramming/new/final data/non_landslide_point3.shp")
non_landslide_points=st_transform(non_landslide_points,crs=crs(landslide_point))
h=extract(aspect_reclassification,non_landslide_points)
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
    i=0.3694509
    
  }else if(i==2){
    i= 0.3144547
    
  }else if(i==3){
    i= 0.8341093
    
    
  }else if(i==4){
    i=1.6127546
  }else if(i==5){
    i=2.6913475
  }else if(i==6){
    i=1.4867935
  }else if(i==7){
    i=0.434473
  }else if(i==8){
    i=0.1010573
  }
  m=append(m,i)
}
print(m)
r=matrix(c(m),ncol=1,byrow=TRUE)
colnames(r)="FR_ASPECT"
r
j=read.csv("D:/nou_rprogramming/new/final data/m5.csv")
o=cbind(j,r)
o
write.csv(r,"D:/nou_rprogramming/new/final data/m5.csv",append=TRUE)
