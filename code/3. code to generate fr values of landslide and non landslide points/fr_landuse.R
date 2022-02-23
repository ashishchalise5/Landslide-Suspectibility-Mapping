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
landuse_reclassification=raster("D:/nou_rprogramming/new/raw data/baglung_landuse.tif")
a=levels(landuse_reclassification)[[1]]
a
a$fr=c(0.462853,
       0.9027724,
       1.4252597,
       1.9963479,
       0,
       0,
       0,
       0)

a
levels(landuse_reclassification)=a
levels(landuse_reclassification)
landuse_reclassification


landslide_point=st_read("D:/nou_rprogramming/new/final data/landslide_point.shp")
example_raster1 <- raster(crs = crs(landuse_reclassification), vals = 0, resolution = c(30,30), ext = extent(c(683501.6, 762821.6, 3107189, 3170489)))
#converting sp object to raster
landslide_spatial=as(landslide_point,"Spatial")
#rasterizing and reclassify
landslide_raster=rasterize(coordinates(landslide_spatial)[,1:2],example_raster1)
landslide_raster

h=extract(landuse_reclassification,landslide_point)
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
    i=0.462853
    
  }else if(i==2){
    i= 0.9027724
    
  }else if(i==3){
    i= 1.4252597
    
  }else if(i==4){
    i= 1.9963479
    
  }else if(i==5){
    i= 0
    
  }else if(i==6){
    i= 0
    
  }else if(i==7){
    i= 0
    
  }else if(i==8){
    i=0
  }
  m=append(m,i)
}
print(m)
r=matrix(c(m),ncol=1,byrow=TRUE)
colnames(r)="FR_LANDUSE"
r

i=read.csv("D:/nou_rprogramming/new/final data/m.csv")

o=cbind(i,r)
class(o)
write.csv(o,"D:/nou_rprogramming/new/final data/m.csv",append=TRUE)
#for nonlandslide

non_landslide_points=st_read("D:/nou_rprogramming/new/final data/non_landslide_point3.shp")
non_landslide_points=st_transform(non_landslide_points,crs=crs(landslide_point))
h=extract(landuse_reclassification,non_landslide_points)
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
    i=0.462853
    
  }else if(i==2){
    i= 0.9027724
    
  }else if(i==3){
    i= 1.4252597
    
  }else if(i==4){
    i= 1.9963479
    
  }else if(i==5){
    i= 0
    
  }else if(i==6){
    i= 0
    
  }else if(i==7){
    i= 0
    
  }else if(i==8){
    i=0
  }
  m=append(m,i)
}
print(m)
r=matrix(c(m),ncol=1,byrow=TRUE)
colnames(r)="FR_LANDUSE"
r
j=read.csv("D:/nou_rprogramming/new/final data/m5.csv")
o=cbind(j,r)
o
write.csv(o,"D:/nou_rprogramming/new/final data/m5.csv",append=TRUE)
