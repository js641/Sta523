###
rm(list=ls())
library(tree)
library(raster)
library(class)
# 
# load("/Users/jordan/HW3/nyc_dt_hasStreetName_pluto.Rdata")
# load("/Users/jordan/HW3/nyc_dt_notHasStreetName_hasCross12_data.Rdata")
# load("/Users/jordan/HW3/nyc_dt_only_interceptions12_data.Rdata")
# 
# d1 = nyc_dt_hasStreetName_pluto[, c("Borough.x", "y", "x")]
# names(d1) = c("Borough", "latitude", "longitude")
# d2 = nyc_dt_notHasStreetName_hasCross12_data[, c("Borough", "latitude", "longitude")]
# d3 = nyc_dt_only_interceptions12_data[, c("Borough", "latitude", "longitude")]
# 
# nyc = rbind(d1, d2, d3)
# save(nyc, file="/Users/jordan/HW3/nyc.Rdata")
# rm(d1, d2, d3, nyc_dt_hasStreetName_pluto, 
#    nyc_dt_notHasStreetName_hasCross12_data, 
#    nyc_dt_only_interceptions12_data)

load("/Users/jordan/HW3/nyc.Rdata")

nyc$ID = seq(1, length(nyc$Borough), 1)
# 
# set.seed(1)
# small = sample(1:nrow(nyc), .005*nrow(nyc))
# nycSmall = nyc[small, ]
# nycSmall$Borough = as.factor(nycSmall$Borough)

# nycSmall = na.omit(nycSmall)
# nycSmall$Borough[nycSmall$Borough=="BROOKLYN"] = 1
# nycSmall$Borough[nycSmall$Borough=="QUEENS"] = 2
# nycSmall$Borough[nycSmall$Borough=="BRONX"] = 3
# nycSmall$Borough[nycSmall$Borough=="MANHATTAN"] = 4
# nycSmall$Borough[nycSmall$Borough=="STATEN ISLAND"] = 5
# nycSmall$Borough = as.numeric(nycSmall$Borough)
# nycTrain = nycSmall[sample(1:nrow(nycSmall), .8*nrow(nycSmall)), ]
# nycTest = nycSmall[!(nycSmall$ID %in% nycTrain$ID), ]
# 
# k1 = knn.cv(nycTrain, cl=nycTrain$Borough, k=10)






library(randomForest)

set.seed(1)
small = sample(1:nrow(nyc), .0015*nrow(nyc))
nycSmall = nyc[small, ]
nycSmall = nycSmall[nycSmall$Borough!="UNSPECIFIED" & 
                      is.finite(nycSmall$latitude) &
                      is.finite(nycSmall$longitude), ]
nycTrain = nycSmall[sample(1:nrow(nycSmall), .667*nrow(nycSmall)), ]
nycTest = nycSmall[!(unique(nycSmall$ID) %in% unique(nycTrain$ID)), ]

RF = randomForest(as.factor(Borough) ~ latitude + longitude, data=nycSmall, 
            mtry=2, importance=TRUE, 
            control=tree.control(nobs=length(nycSmall$Borough), 
                         mincut = 1,
                         minsize = 2,
                         mindev = 0.00001))
summary(RF)

yHatBagR = predict(RF, newdata=nycTest)
table(yHatBagR, nycTest$Borough)
sum(diag(table(yHatBagR, nycTest$Borough)))/length(nycTest$Borough)



# save(tree, file="/Users/jordan/HW3/tree.Rdata")
# 
# load("/Users/jordan/HW3/tree.Rdata")
# 

# plot(tree)
# text(tree,pretty=0)
# tree






r = raster(nrows=500, ncols=500, 
           xmn=-74.3, xmx=-73.71,
           ymn=40.49, ymx=40.92)
r[] = NA
plot(r)
points(nycSmall$longitude, nycSmall$latitude, pch=16, cex=0.1)

pred_locs = data.frame(xyFromCell(r, 1:250000))
names(pred_locs) = c("longitude", "latitude")

r[] = predict(RF, pred_locs)
r
plot(r)
points(nycSmall$longitude, nycSmall$latitude, pch=16, cex=0.1)

poly = rasterToPolygons(r, dissolve=TRUE)
plot(poly,col=1:5)

names(poly@data) = "Name"
pred = predict(tree, pred_locs)
shortToLong = c("BK"="Brooklyn",
                "BX"="Bronx",
                "MN"="Manhattan",
                "QN"="Queens",
                "SI"="Staten Island")
poly@data$Name = shortToLong(levels(pred))

source("write_geojson.R")
write_geojson(poly, "boroughs.json")
