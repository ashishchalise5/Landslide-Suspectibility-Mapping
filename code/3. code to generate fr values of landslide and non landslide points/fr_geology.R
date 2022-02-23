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
geology_reclassification=raster("D:/nou_rprogramming/new/final data/geology_raster.tif")
a=levels(geology_reclassification)[[1]]
a
a$fr=c(0,1.396831,
       1.8481656,
       0.8982615,
       0,
       0,
       0,
       0.8392127,
       0.6088582,
       1.564281,
       0,
       1.2006897)
a
levels(geology_reclassification)=a
levels(geology_reclassification)
geology_reclassification
#for landslide
landslide_point=st_read("D:/nou_rprogramming/new/final data/landslide_point.shp")
landslide_point
#extracting value of landslide_point
h=extract(geology_reclassification,landslide_point)
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
    i=1.396831
    
  }else if(i==2){
    i= 1.8481656
    
  }else if(i==3){
    i= 0.8882808
    
    
  }else if(i==4){
    i=0
  }else if(i==5){
    i=0
  }else if(i==6){
    i=0
  }else if(i==7){
    i=0.8392127
  }else if(i==8){
    i=0.6088582
  }else if(i==9){
    i=1.564281
  }else if(i==10){
    i=0
  }else if(i==11){
    i=1.2006897
  }
  m=append(m,i)
}
print(m)
r=matrix(c(m),ncol=1,byrow=TRUE)
colnames(r)="FR_GEOLOGY"
r

i=read.csv("D:/nou_rprogramming/new/final data/m.csv")

o=cbind(i,r)
class(o)
write.csv(o,"D:/nou_rprogramming/new/final data/m.csv",append=TRUE)
#for non_landslide
non_landslide_points=st_read("D:/nou_rprogramming/new/final data/non_landslide_point3.shp")
non_landslide_points=st_transform(non_landslide_points,crs=crs(landslide_point))
h=extract(geology_reclassification,non_landslide_points)
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
    i=1.396831
    
  }else if(i==2){
    i= 1.8481656
    
  }else if(i==3){
    i= 0.8882808
    
    
  }else if(i==4){
    i=0
  }else if(i==5){
    i=0
  }else if(i==6){
    i=0
  }else if(i==7){
    i=0.8392127
  }else if(i==8){
    i=0.6088582
  }else if(i==9){
    i=1.564281
  }else if(i==10){
    i=0
  }else if(i==11){
    i=1.2006897
  }
  m=append(m,i)
}
print(m)
r=matrix(c(m),ncol=1,byrow=TRUE)
colnames(r)="FR_GEOLOGY"
r
j=read.csv("D:/nou_rprogramming/new/final data/m5.csv")
o=cbind(j,r)
o
write.csv(o,"D:/nou_rprogramming/new/final data/m5.csv",append=TRUE)
