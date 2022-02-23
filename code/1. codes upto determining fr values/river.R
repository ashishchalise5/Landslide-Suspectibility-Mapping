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
river=st_read("D:/nou_rprogramming/new/Raw data/data/River.shp")
baglung=st_read("D:/nou_rprogramming/new/Raw data/baglung_for_mask.shp")
landslide_point=st_read("D:/nou_rprogramming/new/final data/landslide_point.shp")
dem=raster("D:/nou_rprogramming/new/final data/dem_baglung.tif")
dem
#transform coordinate system
baglung=st_transform(baglung,crs=st_crs(landslide_point))
river1=st_transform(river,crs=st_crs(landslide_point))
plot(river1)
#export
#st_write(baglung,"D:/nou_rprogramming/new/final data/baglung.shp",append=FALSE)
#perform clip operation
river_baglung=st_intersection(river1,baglung)
st_write(river_baglung,"D:/nou_rprogramming/new/Raw data/river_baglung.shp",append=FALSE)
#the data was edited in arcmap
river_final=st_read("D:/nou_rprogramming/new/final data/river_baglung_final.shp")
#subsetting by attribute:
river_baglung$RIVER_CODE
#converting sf to sp
r=as(river_final,"Spatial")

#creating an  raster to convert vector to raster
example_raster <- raster(crs = crs(dem), vals = 0, resolution = c(30,30), ext = extent(c(683501.6, 762821.6, 3107189, 3170489)))
#converting sp object to raster
river_raster=rasterize(r,example_raster)
#exporting an raster
writeRaster(river_raster,"D:/nou_rprogramming/new/Final data/river_raster.tif",overwrite=TRUE)

buffer_raster=raster("D:/nou_rprogramming/new/Raw data/buffer_river1.tif")
plot(buffer_raster)
buffer_raster
b=st_read("D:/nou_rprogramming/new/final data/baglung.shp")
b=as(b,'Spatial')
mask_river_buffer=mask(buffer_raster,b)
plot(mask_river_buffer)
writeRaster(mask_river_buffer,"D:/nou_rprogramming/new/final data/river_buffer.tif",overwrite=TRUE)
#for reclassification

river_reclassified=reclassify(mask_river_buffer,matrix(c(-Inf,75,1,75,150,2,150,225,3,225,300,4,300,375,5,375,450,6,450,Inf,7),ncol=3,byrow=TRUE),right=T)
river_reclassified=ratify(river_reclassified,count=TRUE)
plot(river_reclassified)
z=levels(river_reclassified)[[1]]$COUNT
write.dbf(levels(river_reclassified)[[1]],"D:/nou_rprogramming/new/final data/river_reclassifed.tif.dbf")
writeRaster(river_reclassified,"D:/nou_rprogramming/new/final data/river_reclassified.tif",format="GTiff",overwrite=TRUE)
#creating an  raster to convert landslide_point to raster
example_raster1 <- raster(crs = crs(dem), vals = 0, resolution = c(30,30), ext = extent(c(683501.6, 762821.6, 3107189, 3170489)))
#converting sf to sp
landslide_spatial=as(landslide_point,"Spatial")
#rasterizing 
landslide_raster=rasterize(coordinates(landslide_spatial)[,1:2],example_raster1,field=landslide_spatial$OID_)
landslide_reclassified=ratify(landslide_raster,count=TRUE)
landslide_raster
#tabulate area
f=raster::zonal(landslide_reclassified,river_reclassified,'count')
f
#pixel count of road_buffer(distance buffer)
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
fr_river=matrix(c((for_fr[,1]),fr),byrow="FALSE",ncol=2)
fr_river
