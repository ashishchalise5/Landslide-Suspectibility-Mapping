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
library(spatialEco)
#load shapefile

landslide_point=st_read("D:/nou_rprogramming/new/final data/landslide_point.shp")
dem=raster("D:/nou_rprogramming/new/final data/dem_baglung.tif")
slope_baglung=slopeAspect(dem,out="slope",unit="degrees")
plot(slope_baglung)
writeRaster(slope_baglung,"D:/nou_rprogramming/new/final data/slope_baglung.tif")
slope_baglung$slope

slope_reclassified=reclassify(slope_baglung,matrix(c(-Inf,10,1,10,20,2,20,30,3,30,40,4,40,55,5,55,Inf,6),ncol=3,byrow=TRUE),right=T)
plot(slope_reclassified)
slope_reclassified=ratify(slope_reclassified,count=TRUE)
slope_reclassified

#exporting an raster
writeRaster(slope_reclassified,"D:/nou_rprogramming/new/final data/slope_reclassified.tif",overwrite=TRUE)
z=levels(slope_reclassified)[[1]]$COUNT
z
#creating an  raster to convert landslide_point to raster
example_raster1 <- raster(crs = crs(dem), vals = 0, resolution = c(30,30), ext = extent(c(683501.6, 762821.6, 3107189, 3170489)))
#converting sp object to raster
landslide_spatial=as(landslide_point,"Spatial")
#rasterizing and reclassify
landslide_raster=rasterize(coordinates(landslide_spatial)[,1:2],example_raster1,field=landslide_spatial$OID_)
landslide_reclassified=ratify(landslide_raster,count=TRUE)
#writeRaster(landslide_reclassified,"D:/nou_rprogramming/new/final data/landslide_reclassifed.tif",format="GTiff",overwrite=TRUE)
#tabulate area
f=raster::zonal(landslide_reclassified,slope_reclassified,'count')
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
fr_slope=matrix(c((for_fr[,1]),fr),byrow="FALSE",ncol=2)
fr_slope

