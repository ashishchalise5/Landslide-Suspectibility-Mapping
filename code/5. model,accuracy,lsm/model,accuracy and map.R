library(rpart)
library(rpart.plot)
library(randomForest)
library(raster)
library(rattle)
library(ROCR)

data=read.csv("D:/nou_rprogramming/new/final data/rf_model4.csv")
str(data)
data$hazard=as.factor(data$hazard)
n = nrow(data)
random_data = t[sample(n),]
train = random_data[1:round(0.70* n),]
write.csv(train,"D:/nou_rprogramming/new/final data/train1.csv")
test = random_data[(round(0.70* n) + 1):n,]
write.csv(test,"D:/nou_rprogramming/new/final data/test1.csv")
#cart
cart_train = rpart(hazard ~ ., train, method = "class", control = rpart.control(cp=0.010))
summary(cart_train)
cart_train$variable.importance
pred = predict(cart_train, test, type = "class")
(confusion = table(test$hazard, pred))
sum(diag(confusion))/sum(confusion)
#cart auc_test
pred1=predict(cart_train,test,type="prob")[,2]
pred2<-prediction(pred1,test$hazard)
roc=performance(pred2,"tpr","fpr")
plot(roc,colorize=T,main="CART Prediction Rate Curve")
abline(a=0,b=1)
#auc
auc=performance(pred2,"auc")
auc=unlist(slot(auc,"y.values"))
auc=round(auc,2)
legend(.8,.2, auc,title="AUC - ",cex=1)
#cart auc_train
pred1=predict(cart_train,train,type="prob")[,2]
pred2<-prediction(pred1,train$hazard)
roc=performance(pred2,"tpr","fpr")
plot(roc,colorize=T,main="CART Sucess Rate Curve")
abline(a=0,b=1)
#auc
auc=performance(pred2,"auc")
auc=unlist(slot(auc,"y.values"))
auc=round(auc,2)
legend(.8,.2,auc,title="AUC - ",cex=1)
#rf
rf = randomForest(hazard ~ ., train)
summary(rf)
rf$importance
pred1=predict(rf,test,type="class")
(confusion = table(test$hazard, pred1))
sum(diag(confusion))/sum(confusion)
#rf auc
pred1=predict(rf,test,type="prob")[,2]
pred2<-prediction(pred1,test$hazard)
roc=performance(pred2,"tpr","fpr")
plot(roc,colorize=T,main="RF Prediction Rate Curve")
abline(a=0,b=1)
#auc
auc=performance(pred2,"auc")
auc=unlist(slot(auc,"y.values"))
auc=round(auc,2)
legend(.8,.2, auc,title="AUC - ",cex=1)
#rf auc_train
pred1=predict(rf,train,type="prob")[,2]
pred2<-prediction(pred1,train$hazard)
roc=performance(pred2,"tpr","fpr")
plot(roc,colorize=T,main="RF Success Rate Curve")
abline(a=0,b=1)
#auc
auc=performance(pred2,"auc")
auc=unlist(slot(auc,"y.values"))
auc=round(auc,2)
legend(.8,.2, auc,title="AUC - ",cex=1)

#for map
baglung=st_read("D:/nou_rprogramming/new/final data/baglung.shp")
river=raster("D:/nou_rprogramming/new/final data/rf_river_reclassified.tif")
road=raster("D:/nou_rprogramming/new/final data/rf_road_reclassified.tif")
fault=raster("D:/nou_rprogramming/new/final data/rf_fault_reclassified.tif")
geology=raster("D:/nou_rprogramming/new/final data/rf_geology_reclassified.tif")
soil=raster("D:/nou_rprogramming/new/final data/rf_soil_reclassified.tif")
landuse=raster("D:/nou_rprogramming/new/final data/rf_landuse_reclassified.tif")
elevation=raster("D:/nou_rprogramming/new/final data/rf_elevation_reclassified.tif")
curvature=raster("D:/nou_rprogramming/new/final data/rf_curvature_reclassified.tif")
slope=raster("D:/nou_rprogramming/new/final data/rf_slope_reclassified.tif")
aspect=raster("D:/nou_rprogramming/new/final data/rf_aspect_reclassified.tif")
#forcart
u=river*0.3157572+road*0.7556933+fault*0.1294887+geology*0.0381617+soil*0.4016218+elevation* 0.1241790+landuse*0.3923891+slope*1.6985024+aspect*1.2517241+curvature*0.1908085
u
plot(u)
prob = 1/(1+exp(u *(-1)))
prob
plot(prob)
#tm
tmap_mode("plot")
cart_map=tmap_style("white")+
  tm_shape(prob,name="Landslide Susceptibility Map, Cart",title="Landslide Susceptibility Map, Cart") +
  tm_raster(
    title="Suspectibility",
    palette = "Set1",style="quantile",n=4,
    breaks=c(0.55,0.642,0.693,0.74,0.88),
    labels=c("Low", "Medium", "High", "Very High"),
    legend.show = T)+
  tm_scale_bar(
    width=0.5, text.size=0.8,
    position = c("left","bottom"),lwd=1)+
  tm_compass(type="4star",position=c("right","top"))+
  
  tm_layout(scale=.8,
      main.title="Landslide Susceptibility Map (CART)",
      main.title.position="center",main.title.color="black",
      main.title.size=2,
      legend.position=c(.85,.6),
      legend.outside=FALSE,
      legend.text.size = 1,
      legend.width=.4, legend.height=.6)+
  tm_graticules(
    ticks=TRUE,lines=FALSE,
    labels.rot=c(0,0),lwd=1,
    labels.size=.7)
  
cart_map
tmap_save(cart_map,"D:/nou_rprogramming/new/outputs/landslide_susceptibility_cart.jpg",height=7)
writeRaster(p,"D:/nou_rprogramming/new/final data/landslide_susceptibility_cart_classified.tif")

#for rf
v=river*0.22364691+road*0.41075731+fault*0.08207446+geology*0.38538238+soil*0.41437370+elevation* 0.55454643+landuse*0.84776509+slope*1.42814362+aspect*1.40435311+curvature*0.30321441
v
plot(v)
prob1 = 1/(1+exp(v *(-1)))
prob1
#tm
tmap_mode("plot")
rf_map=tmap_style("white")+
  tm_shape(prob1,name="Landslide Susceptibility Map, RF",title="Landslide Susceptibility Map, RF") +
  tm_raster(
    title="Suspectibility",
    palette = "Set1",style="quantile",n=4,
    breaks=c(0.55,0.667,0.715,0.768,0.90),
    labels=c("Low", "Medium", "High", "Very High"),
    legend.show = T)+
  tm_scale_bar(
    width=0.5, text.size=0.8,
    position = c("left","bottom"),lwd=1)+
  tm_compass(type="4star",position=c("right","top"))+
  
  tm_layout(scale=.8,
            main.title="Landslide Susceptibility Map (RF)",
            main.title.position="center",main.title.color="black",
            main.title.size=2,
            legend.position=c(.85,.6),
            legend.outside=FALSE,
            legend.text.size = 1,
            legend.width=.4, legend.height=.6)+
  tm_graticules(
    ticks=TRUE,lines=FALSE,
    labels.rot=c(0,0),lwd=1,
    labels.size=.7)
rf_map
tmap_save(rf_map,"D:/nou_rprogramming/new/outputs/landslide_susceptibility_rf.jpg",height=7)

