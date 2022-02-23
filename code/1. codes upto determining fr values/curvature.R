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
v=raster("D:/nou_rprogramming/new/final data/curvature_baglung.tif")
curvature_reclassification=reclassify(v,matrix(c(-Inf,-0.05,1,-0.05,0.05,2,0.05,Inf,3),byrow = TRUE,ncol=3),right=T)
curvature_reclassification=ratify(curvature_reclassification,count=TRUE)
curvature_reclassification

plot(curvature_reclassification)

writeRaster(curvature_reclassification,"D:/nou_rprogramming/new/final data/curvature_reclassified.tif")

landslide_point=st_read("D:/nou_rprogramming/new/final data/landslide_point.shp")
landslide_point
example_raster1 <- raster(crs = crs(curvature_reclassification), vals = 0, resolution = c(30,30), ext = extent(c(683501.6, 762821.6, 3107189, 3170489)))
#converting sp object to raster
landslide_spatial=as(landslide_point,"Spatial")
#rasterizing and reclassify
landslide_raster=rasterize(coordinates(landslide_spatial)[,1:2],example_raster1)
landslide_reclassified=ratify(landslide_raster,count=TRUE)

#tabulate area
f=raster::zonal(landslide_reclassified,curvature_reclassification,'count')
f
z=levels(curvature_reclassification)[[1]]$COUNT
sum(z)
#pixel count
class_pixel=c(z)
class_pixel
#combining
for_fr=cbind(f,class_pixel)
for_fr
#total landslide pixel:
sum_landslide=sum(for_fr[,2])
sum_landslide
#total pixel
sum_pixel=sum(for_fr[,3])
sum_pixel
#fr model
fr=(for_fr[,2]/sum_landslide)/(for_fr[,3]/sum_pixel)
fr_curvature=matrix(c((for_fr[,1]),fr),byrow="FALSE",ncol=2)
fr_curvature