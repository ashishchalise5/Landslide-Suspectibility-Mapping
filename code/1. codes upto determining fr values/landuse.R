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
baglung_landuse=raster("D:/nou_rprogramming/new/Raw data/baglung_landuse.tif")
a=levels(baglung_landuse)[[1]]

landslide_point=st_read("D:/nou_rprogramming/new/final data/landslide_point.shp")
dem=raster("D:/nou_rprogramming/new/final data/dem_baglung.tif")

#transform coordinate system
landuse_baglung=projectRaster(baglung_landuse,crs=crs(dem))
landuse_baglung=resample(landuse_baglung,dem)
landuse_baglung=mask(landuse_baglung,dem)
landuse_reclassified=reclassify(landuse_baglung,matrix(c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8),ncol=2,byrow=TRUE),right=T)
landuse_reclassified=ratify(landuse_reclassified,count=TRUE)
levels(landuse_reclassified)

#exporting an raster
writeRaster(landuse_reclassified,"D:/nou_rprogramming/new/final data/landuse_reclassified.tif",overwrite=TRUE)
z=levels(landuse_reclassified)[[1]]$COUNT
z
#creating an  raster to convert landslide_point to raster
example_raster1 <- raster(crs = crs(dem), vals = 0, resolution = c(30,30), ext = extent(c(683501.6, 762821.6, 3107189, 3170489)))
#converting sf to sp 
landslide_spatial=as(landslide_point,"Spatial")
#rasterizing 
landslide_raster=rasterize(coordinates(landslide_spatial)[,1:2],example_raster1,field=landslide_spatial$OID_)
landslide_reclassified=ratify(landslide_raster,count=TRUE)
landslide_raster

f=raster::zonal(landslide_reclassified,landuse_reclassified,'count')
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
fr_landuse=matrix(c((for_fr[,1]),fr),byrow="FALSE",ncol=2)
fr_landuse

