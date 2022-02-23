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
fault=st_read("D:/nou_rprogramming/new/Raw data/data/fault.shp")
baglung=st_read("D:/nou_rprogramming/new/Raw data/baglung_for_mask.shp")
landslide_point=st_read("D:/nou_rprogramming/new/final data/landslide_point.shp")
dem=raster("D:/nou_rprogramming/new/final data/dem_baglung.tif")
dem
#transform coordinate system
baglung=st_transform(baglung,crs=st_crs(landslide_point))
fault1=st_transform(fault,crs=st_crs(landslide_point))
#perform clip operation
fault_baglung=st_intersection(fault1,baglung)
plot(fault_baglung)
st_write(fault_baglung,"D:/nou_rprogramming/new/final data/fault_baglung.shp",append=FALSE)

#converting sf to sp
r=as(fault_baglung,"Spatial")
#creating an  raster to convert vector to raster
example_raster <- raster(crs = crs(dem), vals = 0, resolution = c(30,30), ext = extent(c(683501.6, 762821.6, 3107189, 3170489)))
#converting sp object to raster
fault_raster=rasterize(r,example_raster)
plot(fault_raster)
#exporting an raster
writeRaster(fault_raster,"D:/nou_rprogramming/new/final data/fault_raster.tif",overwrite=TRUE)
#importing a raster
buffer_raster=raster("D:/nou_rprogramming/new/Raw data/buffer_fault.tif")
plot(buffer_raster)
buffer_raster
b=st_read("D:/nou_rprogramming/new/final data/baglung.shp")
b=as(b,'Spatial')
mask_fault_buffer=mask(buffer_raster,dem)
plot(mask_fault_buffer)
writeRaster(mask_fault_buffer,"D:/nou_rprogramming/new/final data/fault_buffer.tif",overwrite=TRUE)
#reclassify
fault_reclassified=reclassify(mask_fault_buffer,matrix(c(-Inf,75,1,75,150,2,150,225,3,225,300,4,300,375,5,375,450,6,450,Inf,7),ncol=3,byrow=TRUE),right=T)
fault_reclassified=ratify(fault_reclassified,count=TRUE)
plot(fault_reclassified)
z=levels(fault_reclassified)[[1]]$COUNT
z
writeRaster(fault_reclassified,"D:/nou_rprogramming/new/final data/fault_reclassified.tif",format="GTiff",overwrite=TRUE)
#creating an  raster to convert landslide_point to raster
example_raster1 <- raster(crs = crs(dem), vals = 0, resolution = c(30,30), ext = extent(c(683501.6, 762821.6, 3107189, 3170489)))
#converting sp object to raster
landslide_spatial=as(landslide_point,"Spatial")
#rasterizing and reclassify
landslide_raster=rasterize(coordinates(landslide_spatial)[,1:2],example_raster1,field=landslide_spatial$OID_)
landslide_raster=ratify(landslide_raster,count=TRUE)

#writeRaster(landslide_reclassified,"D:/nou_rprogramming/new/final data/landslide_reclassifed.tif",format="GTiff",overwrite=TRUE)
#tabulate area
f=raster::zonal(landslide_raster,fault_reclassified,'count')
f
#pixel count of fault_buffer(distance buffer)
class_pixel=c(z)
#combining
for_fr=cbind(f,class_pixel)
for_fr
#total landslide pixel:
sum_landslide=sum(for_fr[,2])
#total pixel
sum_pixel=sum(for_fr[,3])
sum_pixel
#fr model
fr=(for_fr[,2]/sum_landslide)/(for_fr[,3]/sum_pixel)
fr_fault=matrix(c((for_fr[,1]),fr),byrow="FALSE",ncol=2)
fr_fault
