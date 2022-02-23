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

#load shapefile and raster

landslide_point=st_read("D:/nou_rprogramming/new/final data/landslide_point.shp")
landuse_baglung=raster("D:/nou_rprogramming/new/final data/landuse_baglung.tif")
dem=raster("D:/nou_rprogramming/new/final data/dem_baglung.tif")
aspect_baglung=slopeAspect(dem,out="aspect",unit="degrees")
plot(aspect_baglung)
writeRaster(aspect_baglung,"D:/nou_rprogramming/new/final data/aspect_baglung.tif")


aspect_reclassified=reclassify(aspect_baglung,matrix(c(-Inf,22.5,1,22.5,67.5,2,67.5,112.5,3,112.5,157.5,4,157.5,202.5,5,202.5,247.5,6,247.5,292.5,7,292.5,337.5,8,337.5,360,1),ncol=3,byrow=TRUE),right=T)
plot(aspect_reclassified)
aspect_reclassified=ratify(aspect_reclassified,count=TRUE)
aspect_reclassified

#exporting an raster
writeRaster(aspect_reclassified,"D:/nou_rprogramming/new/final data/aspect_reclassified.tif",overwrite=TRUE)
z=levels(aspect_reclassified)[[1]]$COUNT
z
#creating an  raster to convert landslide_point to raster
example_raster1 <- raster(crs = crs(dem), vals = 0, resolution = c(30,30), ext = extent(c(683501.6, 762821.6, 3107189, 3170489)))
#converting sp object to raster
landslide_spatial=as(landslide_point,"Spatial")
#rasterizing and reclassify
landslide_raster=rasterize(coordinates(landslide_spatial)[,1:2],example_raster1,field=landslide_spatial$OID_)
landslide_reclassified=ratify(landslide_raster,count=TRUE)
#tabulate area
f=raster::zonal(landslide_reclassified,aspect_reclassified,'count')
f
#pixel count of road_buffer(distance buffer)
z=levels(aspect_reclassified)[[1]]$COUNT
z
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
fr_aspect=matrix(c((for_fr[,1]),fr),byrow="FALSE",ncol=2)
fr_aspect


