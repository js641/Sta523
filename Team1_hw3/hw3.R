library(foreach)
library(doSNOW)
library(dplyr)
library(data.table)
library(raster)
library(rgeos)
library(e1071)

load("~/nyc_dt_hasStreetName_pluto")
d<-tbl_df(nyc_dt_hasStreetName_pluto)
d<-select(d,Borough.x,x,y)
d_test<-d
#d_test<-sample_n(d_test,size=0.001*nrow(d))
names(d_test)=c("borough","x","y")
d_test<-d_test[d_test$borough!="UNSPECIFIED",]


#creating a model
#BRONX BROOKLYN MANHATTAN QUEENS STATEN ISLAND
d_test$borough[d_test$borough=="BRONX"]<-"1"
d_test$borough[d_test$borough=="BROOKLYN"]<-"2"
d_test$borough[d_test$borough=="MANHATTAN"]<-"3"
d_test$borough[d_test$borough=="QUEENS"]<-"4"
d_test$borough[d_test$borough=="STATEN ISLAND"]<-"5"
d_test$borough=as.numeric(d_test$borough)

## Create raster for prediction locations
r = raster(nrows=500, ncols=500, 
           xmn=-74.3, xmx=-73.71, 
           ymn=40.49, ymx=40.92)
r[]=NA

pred_locs = data.frame(xyFromCell(r, 1:250000))
#plot(r)

svm_fit<-svm(borough~.,data=d_test)  

parallel_predictions<-function(fit,testing)  
{  
  cl<-makeCluster(2)
  registerDoSNOW(cl) 
  num_splits<-4  
  split_testing<-sort(rank(1:nrow(testing))%%4)  
  predictions<-foreach(i=unique(split_testing),  
                       .combine=c,.packages=c("e1071")) %dopar% {  
                         as.numeric(predict(fit,newdata=testing[split_testing==i,]))  
                       }  
  stopCluster(cl)  
  predictions  
}  

r[] = parallel_prediction(svm_fit,pred_locs)

#plot(r)

## Create Polygons
poly = rasterToPolygons(r,dissolve=TRUE)

short_to_long = c("2"="BROOKLYN", 
                  "1"="BRONX",
                  "3"="MANHATTAN",
                  "4"="QUEENS",
                  "5"="STATEN ISLAND")

names(poly@data) = "Name"
poly@data$Names = short_to_long[levels(pred)]

source("write_geojson.R")
write_geojson(poly,"boroughs.json")
