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
geology=st_read("D:/nou_rprogramming/new/Raw data/data/Geology.shp")
baglung=st_read("D:/nou_rprogramming/new/Raw data/baglung_for_mask.shp")
landslide_point=st_read("D:/nou_rprogramming/new/final data/landslide_point.shp")
dem=raster("D:/nou_rprogramming/new/final data/dem_baglung.tif")
#transform coordinate system
baglung=st_transform(baglung,crs=st_crs(landslide_point))
geology1=st_transform(geology,crs=st_crs(landslide_point))
clipping
geology_baglung=st_intersection(geology1,baglung)
st_write(geology_baglung,"D:/nou_rprogramming/new/final data/geology_baglung.shp",append=FALSE)

#converting sf to sp
r=as(geology_baglung,"Spatial")
g=as.factor(r$CLASS)
levels(g)
plot(r)
#creating an  raster to convert vector to raster
example_raster <- raster(crs = crs(dem), vals = 0, resolution = c(30,30), ext = extent(c(683501.6, 762821.6, 3107189, 3170489)))
#converting sp object to raster
geology_raster=rasterize(r,example_raster,g)
geology_raster
plot(geology_raster)
geology_raster=ratify(geology_raster,count=TRUE)

geology_raster
#exporting an raster
writeRaster(geology_raster,"D:/nou_rprogramming/new/final data/geology_raster.tif",overwrite=TRUE)


geology_raster=mask(geology_raster,dem)


z=levels(geology_raster)[[1]]$COUNT
sum(z)
#creating an  raster to convert landslide_point to raster
example_raster1 <- raster(crs = crs(dem), vals = 0, resolution = c(30,30), ext = extent(c(683501.6, 762821.6, 3107189, 3170489)))
#converting sp object to raster
landslide_spatial=as(landslide_point,"Spatial")
#rasterizing and reclassify
landslide_raster=rasterize(coordinates(landslide_spatial)[,1:2],example_raster1,field=landslide_spatial$OID_)
landslide_reclassified=ratify(landslide_raster,count=TRUE)
landslide_raster
#tabulate area
f=raster::zonal(landslide_reclassified,geology_raster,'count')
f
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
fr_geology=matrix(c((for_fr[,1]),fr),byrow="FALSE",ncol=2)
fr_geology

