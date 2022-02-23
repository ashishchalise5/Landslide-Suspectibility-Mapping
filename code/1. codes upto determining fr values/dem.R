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

#load data

landslide_point=st_read("D:/nou_rprogramming/new/final data/landslide_point.shp")
landuse_baglung=raster("D:/nou_rprogramming/new/final data/landuse_reclassified.tif")
dem=raster("D:/nou_rprogramming/new/Raw data/mask.tif")
dem_baglung=projectRaster(dem,landuse_baglung)
dem_baglung
writeRaster(dem_baglung,"D:/nou_rprogramming/new/final data/dem_baglung.tif")
dem_reclassified=reclassify(dem_baglung,matrix(c(-Inf,1100,1,1100,1600,2,1600,2100,3,2100,2600,4,2600,3100,5,3100,3600,6,3600,4100,7,4100,Inf,8),ncol=3,byrow=TRUE),right=T)
plot(dem_reclassified)
#generate raster attribute table
dem_reclassified=ratify(dem_reclassified,count=TRUE)
dem_reclassified
#exporting an raster
writeRaster(dem_reclassified,"D:/nou_rprogramming/new/final data/dem_reclassified.tif",overwrite=TRUE)
z=levels(dem_reclassified)[[1]]$COUNT
sum(z)
#creating an  raster to convert landslide_point to raster
example_raster1 <- raster(crs = crs(dem_baglung), vals = 0, resolution = c(30,30), ext = extent(c(683501.6, 762821.6, 3107189, 3170489)))
#converting sf to sp
landslide_spatial=as(landslide_point,"Spatial")
#rasterizing 
landslide_raster=rasterize(coordinates(landslide_spatial)[,1:2],example_raster1,field=landslide_spatial$OID_)
landslide_raster=ratify(landslide_raster,count=TRUE)
#tabulate area
f=raster::zonal(landslide_raster,dem_reclassified,'count')
f
#pixel count of road_buffer(distance buffer)
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
fr_elevation=matrix(c((for_fr[,1]),fr),byrow="FALSE",ncol=2)
fr_elevation

