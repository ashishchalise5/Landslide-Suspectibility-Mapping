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

#load shapefile
road=st_read("D:/nou_rprogramming/new/Raw data/data/Road.shp")
baglung=st_read("D:/nou_rprogramming/new/Raw data/baglung.shp")
landslide_point=st_read("D:/nou_rprogramming/new/final data/landslide_point.shp")
dem=raster("D:/nou_rprogramming/new/final data/dem_baglung.tif")
dem
#transform coordinate system
baglung=st_transform(baglung,crs=st_crs(landslide_point))
road1=st_transform(road,crs=st_crs(landslide_point))
road1
#export
st_write(baglung,"D:/nou_rprogramming/new/final data/baglung.shp",append=FALSE)
#perform clip operation
road_baglung=st_intersection(road1,baglung)
st_write(road_baglung,"D:/nou_rprogramming/new/Raw data/road_baglung.shp",append=FALSE)
#subsetting by attribute:
r1=road_baglung[road_baglung$TYPE =="Main Trail",]
st_write(r1,"D:/nou_rprogramming/new/final data/road_baglung_final.shp",append=FALSE)
#converting sf to sp
r=as(r1,"Spatial")

#creating an  raster to convert vector to raster
example_raster <- raster(crs = crs(dem), vals = 0, resolution = c(30,30), ext = extent(c(683501.6, 762821.6, 3107189, 3170489)))
#converting sp object to raster
road_raster=rasterize(r,example_raster)
#exporting an raster
writeRaster(road_raster,"D:/nou_rprogramming/new/Final data/road_raster.tif",overwrite=TRUE)
#reprojecting a raster
#a=projectRaster(rr, crs = crs(road))
#buffering takes long time so buffering in arcmap
#road_buffer= buffer(road_raster,20000)
#plot(road_buffer)
#road_buffer
#importing a raster
buffer_raster=raster("D:/nou_rprogramming/new/Raw data/Buffer_raster.tif")
plot(buffer_raster)
buffer_raster
b=as(baglung,'Spatial')
crop_road=crop(buffer_raster,b)
plot(crop_road)
mask_road_buffer=mask(buffer_raster,b)
plot(mask_road_buffer)
writeRaster(mask_road_buffer,"D:/nou_rprogramming/new/final data/road_buffer.tif",overwrite=TRUE)
#for reclassification
#problem: doesnt provide raster with attribute table
mask_road_buffer
road_reclassified=reclassify(mask_road_buffer,matrix(c(-Inf,75,1,75,150,2,150,225,3,225,300,4,300,375,5,375,450,6,450,Inf,7),ncol=3,byrow=TRUE),right=T)
road_reclassified=ratify(road_reclassified,count=TRUE)
road_reclassified
z=levels(road_reclassified)[[1]]$COUNT
write.dbf(levels(road_reclassified)[[1]],"D:/nou_rprogramming/new/final data/road_reclassifed.tif.dbf")
writeRaster(road_reclassified,"D:/nou_rprogramming/new/final data/road_reclassified.tif",format="GTiff",overwrite=TRUE)
road_reclassified
#creating an  raster to convert landslide_point to raster
example_raster1 <- raster(crs = crs(dem), vals = 0, resolution = c(30,30), ext = extent(c(683501.6, 762821.6, 3107189, 3170489)))
#converting sp object to raster
landslide_spatial=as(landslide_point,"Spatial")
#rasterizing and reclassify
landslide_raster=rasterize(coordinates(landslide_spatial)[,1:2],example_raster1,field=landslide_spatial$OID_)
landslide_reclassified=ratify(landslide_raster,count=TRUE)

landslide_raster
writeRaster(landslide_reclassified,"D:/nou_rprogramming/new/final data/landslide_reclassifed.tif",format="GTiff",overwrite=TRUE)
#tabulate area
f=raster::zonal(landslide_reclassified,road_reclassified,'count')
f
#pixel count of road_buffer(distance buffer)
class_pixel=c(z)
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
fr_road=matrix(c((for_fr[,1]),fr),byrow="FALSE",ncol=2)
fr_road




