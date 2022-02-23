y=read.csv("D:/nou_rprogramming/new/final data/model4.csv")


rf_river=y$FR_RIVER/8.5070863
rf_road=y$FR_ROAD/9.1113966
rf_fault=y$FR_FAULT/7.083621
rf_geology=y$FR_GEOLOGY.1/8.3562997
rf_soil=y$FR_SOIL/4.1477072
rf_elevation=y$FR_ELEVATION.1/8.4104527
rf_landuse=y$FR_LANDUSE/4.787233
rf_slope=y$FR_SLOPE.1/6.8262497
rf_aspect=y$FR_ASPECT.1/7.8444408
rf_curvature=y$FR_CURVATURE.1/2.526759
class(y)
rf=cbind(rf_river,rf_road,rf_fault,rf_geology,rf_soil,rf_elevation,rf_landuse,rf_slope,rf_aspect,rf_curvature,y$hazard)
rf
write.csv(rf,"D:/nou_rprogramming/new/final data/rf_model4.csv")
#fault
a=raster("D:/nou_rprogramming/new/final data/fault_reclassified.tif")
plot(a)
b=levels(a)[[1]]
b
b$fr=c(0,0,
       0,
       2.10041,
       1.969416,
       2.013108,
       0,
       1.000687)
b
levels(a)=b
levels(a)
a
sum(levels(a)[[1]]$fr)
rf=levels(a)[[1]]$fr/sum(levels(a)[[1]]$fr)
q=levels(a)[[1]]$ID
class(q)
q[-1]
c=reclassify(a,matrix(c(q[-1],rf[-1]),byrow=FALSE,ncol=2),right=T)
c
writeRaster(c,"D:/nou_rprogramming/new/final data/rf_fault_reclassified.tif")

#river
a=raster("D:/nou_rprogramming/new/final data/river_reclassified.tif")
plot(a)
b=levels(a)[[1]]
b
b$fr=c(0,2.121759,
       0.9794475,
       0.6079564,
       1.828186,
       1.2439851,
       0.8247548,
       0.9009975)
b
levels(a)=b
levels(a)
a
sum(levels(a)[[1]]$fr)
rf=levels(a)[[1]]$fr/sum(levels(a)[[1]]$fr)
q=levels(a)[[1]]$ID
class(q)
q[-1]
c=reclassify(a,matrix(c(q[-1],rf[-1]),byrow=FALSE,ncol=2),right=T)
c
writeRaster(c,"D:/nou_rprogramming/new/final data/rf_river_reclassified.tif")

#aspect

a=raster("D:/nou_rprogramming/new/final data/aspect_reclassified.tif")
plot(a)
b=levels(a)[[1]]
b
b$fr=c(0,0.3694509,
       0.3144547,
       0.8341093,
       1.6127546,
       2.6913475,
       1.4867935,
       0.434473,
       0.1010573)
b
levels(a)=b
levels(a)
a
sum(levels(a)[[1]]$fr)
rf=levels(a)[[1]]$fr/sum(levels(a)[[1]]$fr)
q=levels(a)[[1]]$ID
class(q)
q[-1]
c=reclassify(a,matrix(c(q[-1],rf[-1]),byrow=FALSE,ncol=2),right=T)
c
writeRaster(c,"D:/nou_rprogramming/new/final data/rf_aspect_reclassified.tif")

#slope
a=raster("D:/nou_rprogramming/new/final data/slope_reclassified.tif")
plot(a)
b=levels(a)[[1]]
b
b$fr=c(0,0.3195984,
       0.1950177,
       0.3383416,
       0.9969687,
       2.3754901,
       2.6008332)
b
levels(a)=b
levels(a)
a
sum(levels(a)[[1]]$fr)
rf=levels(a)[[1]]$fr/sum(levels(a)[[1]]$fr)
q=levels(a)[[1]]$ID
class(q)
q[-1]
c=reclassify(a,matrix(c(q[-1],rf[-1]),byrow=FALSE,ncol=2),right=T)
c
writeRaster(c,"D:/nou_rprogramming/new/final data/rf_slope_reclassified.tif")

#curvature
a=raster("D:/nou_rprogramming/new/final data/curvature_reclassified.tif")
plot(a)
b=levels(a)[[1]]
b
b$fr=c(0,1.2431665,
       0.4676373,
       0.8159552)
b
levels(a)=b
levels(a)
a
sum(levels(a)[[1]]$fr)
rf=levels(a)[[1]]$fr/sum(levels(a)[[1]]$fr)
q=levels(a)[[1]]$ID
class(q)
q[-1]
c=reclassify(a,matrix(c(q[-1],rf[-1]),byrow=FALSE,ncol=2),right=T)
c
writeRaster(c,"D:/nou_rprogramming/new/final data/rf_curvature_reclassified.tif")

#elevation
a=raster("D:/nou_rprogramming/new/final data/dem_reclassified.tif")
plot(a)
b=levels(a)[[1]]
b
b$fr=c(0,3.879018,
       0.8663991,
       1.6745937,
       0.6143717,
       0.5503264,
       0.410817,
       0.4149268,
       0)
b
levels(a)=b
levels(a)
a
sum(levels(a)[[1]]$fr)
rf=levels(a)[[1]]$fr/sum(levels(a)[[1]]$fr)
q=levels(a)[[1]]$ID
class(q)
q[-1]
c=reclassify(a,matrix(c(q[-1],rf[-1]),byrow=FALSE,ncol=2),right=T)
c
writeRaster(c,"D:/nou_rprogramming/new/final data/rf_elevation_reclassified.tif")

#landuse
a=raster("D:/nou_rprogramming/new/final data/landuse_reclassified.tif")
plot(a)
b=levels(a)[[1]]
b
b$fr=c(0.462853,
       0.9027724,
       1.4252597,
       1.9963479,
       0,
       0,
       0,
       0)
b
levels(a)=b
levels(a)
a
sum(levels(a)[[1]]$fr)
rf=levels(a)[[1]]$fr/sum(levels(a)[[1]]$fr)
rf
q=levels(a)[[1]]$ID
class(q)
q[-1]
rf[-1]
?reclassify
matrix(c(q[1:8],rf[1:8]),byrow=FALSE,ncol=2)
c=reclassify(a,matrix(c(q[1:8],2,3,4,5,6,7,8,9,rf[1:8]),byrow=FALSE,ncol=3),right=T)
c
plot(c)
writeRaster(c,"D:/nou_rprogramming/new/final data/rf_landuse_reclassified.tif")

#geology
a=raster("D:/nou_rprogramming/new/final data/geology_raster.tif")
plot(a)
b=levels(a)[[1]]
b
b$fr=c(0,1.396831,
       1.8481656,
       0.8982615,
       0,
       0,
       0,
       0.8392127,
       0.6088582,
       1.564281,
       0,
       1.2006897)
b
levels(a)=b
levels(a)
a
sum(levels(a)[[1]]$fr)
rf=levels(a)[[1]]$fr/sum(levels(a)[[1]]$fr)
q=levels(a)[[1]]$ID
class(q)
q[-1]
c=reclassify(a,matrix(c(q[-1],rf[-1]),byrow=FALSE,ncol=2),right=T)
c
writeRaster(c,"D:/nou_rprogramming/new/final data/rf_geology_reclassified.tif")

#soil
a=raster("D:/nou_rprogramming/new/final data/soil_raster.tif")
plot(a)
b=levels(a)[[1]]
b
b$fr=c(0,0.9957096,
       1.2932354,
       1.4717818,
       0.3869804)
b
levels(a)=b
levels(a)
a
sum(levels(a)[[1]]$fr)
rf=levels(a)[[1]]$fr/sum(levels(a)[[1]]$fr)
q=levels(a)[[1]]$ID
class(q)
q[-1]
c=reclassify(a,matrix(c(q[-1],rf[-1]),byrow=FALSE,ncol=2),right=T)
c
writeRaster(c,"D:/nou_rprogramming/new/final data/rf_soil_reclassified.tif")

#road
a=raster("D:/nou_rprogramming/new/final data/road_reclassified.tif")
plot(a)
b=levels(a)[[1]]
b
b$fr=c(0,2.1414729,
       1.7400014,
       2.1504129,
       0.7885413,
       0.3147398,
       1.1096872,
       0.8665411)
b
levels(a)=b
levels(a)
a
sum(levels(a)[[1]]$fr)
rf=levels(a)[[1]]$fr/sum(levels(a)[[1]]$fr)
q=levels(a)[[1]]$ID
class(q)
q[-1]
c=reclassify(a,matrix(c(q[-1],rf[-1]),byrow=FALSE,ncol=2),right=T)
c
writeRaster(c,"D:/nou_rprogramming/new/final data/rf_road_reclassified.tif")