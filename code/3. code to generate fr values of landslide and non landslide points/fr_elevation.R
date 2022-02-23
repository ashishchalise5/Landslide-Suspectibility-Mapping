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
elevation_reclassification=raster("D:/nou_rprogramming/new/final data/dem_reclassified.tif")
a=levels(elevation_reclassification)[[1]]
a
a$fr=c(0,3.879018,
       0.8663991,
       1.6745937,
       0.6143717,
       0.5503264,
       0.410817,
       0.4149268,
       0)
a
levels(elevation_reclassification)=a
levels(elevation_reclassification)
elevation_reclassification
#for landslide
landslide_point=st_read("D:/nou_rprogramming/new/final data/landslide_point.shp")
landslide_point
#extracting value of landslide_point
h=extract(elevation_reclassification,landslide_point)
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
    i=3.879018
    
  }else if(i==2){
    i= 0.8663991
    
  }else if(i==3){
    i= 1.6745937

  }else if(i==4){
    i=0.6143717

  }else if(i==5){
    i=0.5503264

  }else if(i==6){
    i=0.410817

  }else if(i==7){
    i=0.4149268

  }else if(i==8){
    i=0
  }
  m=append(m,i)
}
print(m)
r=matrix(c(m),ncol=1,byrow=TRUE)
colnames(r)="FR_ELEVATION"
r

i=read.csv("D:/nou_rprogramming/new/final data/m.csv")

o=cbind(i,r)
class(o)
write.csv(o,"D:/nou_rprogramming/new/final data/m.csv",append=TRUE)
#for non_landslide

non_landslide_points=st_read("D:/nou_rprogramming/new/final data/non_landslide_point3.shp")
non_landslide_points=st_transform(non_landslide_points,crs=crs(landslide_point))
h=extract(elevation_reclassification,non_landslide_points)
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
    i=3.879018
    
  }else if(i==2){
    i= 0.8663991
    
  }else if(i==3){
    i= 1.6745937
    
  }else if(i==4){
    i=0.6143717
    
  }else if(i==5){
    i=0.5503264
    
  }else if(i==6){
    i=0.410817
    
  }else if(i==7){
    i=0.4149268
    
  }else if(i==8){
    i=0
  }
  m=append(m,i)
}
print(m)
r=matrix(c(m),ncol=1,byrow=TRUE)
colnames(r)="FR_ELEVATION"
r

j=read.csv("D:/nou_rprogramming/new/final data/m5.csv")
o=cbind(j,r)
write.csv(o,"D:/nou_rprogramming/new/final data/m5.csv",append=TRUE)
