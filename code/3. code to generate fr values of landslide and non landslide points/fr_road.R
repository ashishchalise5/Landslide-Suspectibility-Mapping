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
road_reclassification=raster("D:/nou_rprogramming/new/road_reclassified.tif")
a=levels(road_reclassification)[[1]]
a$fr=c(0,2.1414729,
       1.7400014,
       2.1504129,
       0.7885413,
       0.3147398,
       1.1096872,
       0.8665411)
a
levels(road_reclassification)=a
levels(road_reclassification)
road_reclassification


landslide_point=st_read("D:/nou_rprogramming/new/final data/landslide_point.shp")
example_raster1 <- raster(crs = crs(road_reclassification), vals = 0, resolution = c(30,30), ext = extent(c(683501.6, 762821.6, 3107189, 3170489)))
#converting sp object to raster
landslide_spatial=as(landslide_point,"Spatial")
#rasterizing and reclassify
landslide_raster=rasterize(coordinates(landslide_spatial)[,1:2],example_raster1)
landslide_raster
?extract
h=extract(road_reclassification,landslide_point)

l=list()
l=NULL
l=append(l,h)
print(l)
m=list()
is.na(l)
m=NULL
for(i in l){
  if(i==1){
    i=2.1414729
    
  }else if(i==2){
    i= 1.7400014
    
  }else if(i==3){
    i= 2.1504129
    
  }else if(i==4){
    i= 0.7885413
    
  }else if(i==5){
    i= 0.3147398
    
  }else if(i==6){
    i=1.1096872
    
  }else if(i==7){
    i=0.8665411
    
  }
  m=append(m,i)
}
print(m)
r=matrix(c(m),ncol=1,byrow=TRUE)
colnames(r)="FR_ROAD"
#write.csv(r,"D:/nou_rprogramming/new/final data/m.csv")

#for nonlandslide

non_landslide_points=st_read("D:/nou_rprogramming/new/final data/non_landslide_point3.shp")
non_landslide_points=st_transform(non_landslide_points,crs=crs(landslide_point))
h=extract(road_reclassification,non_landslide_points)
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
    i=2.1414729
    
  }else if(i==2){
    i= 1.7400014
    
  }else if(i==3){
    i= 2.1504129
    
  }else if(i==4){
    i= 0.7885413
    
  }else if(i==5){
    i= 0.3147398
    
  }else if(i==6){
    i=1.1096872
    
  }else if(i==7){
    i=0.8665411
    
  }
  m=append(m,i)
}
print(m)
r=matrix(c(m),ncol=1,byrow=TRUE)
colnames(r)="FR_ROAD"
r
j=read.csv("D:/nou_rprogramming/new/final data/m5.csv")
o=cbind(j,r)
o
write.csv(o,"D:/nou_rprogramming/new/final data/m5.csv",append=TRUE)
