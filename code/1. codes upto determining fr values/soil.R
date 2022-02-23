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
soil=st_read("D:/nou_rprogramming/new/Raw data/data/soil.shp")
baglung=st_read("D:/nou_rprogramming/new/final data/baglung.shp")
landslide_point=st_read("D:/nou_rprogramming/new/final data/landslide_point.shp")
dem=raster("D:/nou_rprogramming/new/final data/dem_baglung.tif")
dem
#transform coordinate system
baglung=st_transform(baglung,crs=st_crs(landslide_point))
soil1=st_transform(soil,crs=st_crs(landslide_point))
plot(soil1)


#perform clip operation
soil_baglung=st_intersection(soil1,baglung)
st_write(soil_baglung,"D:/nou_rprogramming/new/final data/soil_baglung.shp",append=FALSE)

#subsetting by attribute:

#converting sf to sp
r=as(soil_baglung,"Spatial")


#creating an  raster to convert vector to raster
example_raster <- raster(crs = crs(dem), vals = 0, resolution = c(30,30), ext = extent(c(683501.6, 762821.6, 3107189, 3170489)))
#converting sp object to raster
?rasterize
g=as.factor(r$TYPE)
g
soil_raster=rasterize(r,example_raster,g)
soil_raster
plot(soil_raster)
soil_raster=ratify(soil_raster,count=TRUE)

soil_raster
#exporting an raster
writeRaster(soil_raster,"D:/nou_rprogramming/new/final data/soil_raster.tif",overwrite=TRUE)

z=levels(soil_raster)[[1]]$COUNT
z
#creating an  raster to convert landslide_point to raster
example_raster1 <- raster(crs = crs(dem), vals = 0, resolution = c(30,30), ext = extent(c(683501.6, 762821.6, 3107189, 3170489)))
#converting sp object to raster
landslide_spatial=as(landslide_point,"Spatial")
#rasterizing and reclassify
landslide_raster=rasterize(coordinates(landslide_spatial)[,1:2],example_raster1,field=landslide_spatial$OID_)
landslide_reclassified=ratify(landslide_raster,count=TRUE)
landslide_raster
#tabulate area
f=raster::zonal(landslide_reclassified,soil_raster,'count')
f
#pixel count of road_buffer(distance buffer)
class_pixel=c(z)
class_pixel
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
fr_soil=matrix(c((for_fr[,1]),fr),byrow="FALSE",ncol=2)
fr_soil
