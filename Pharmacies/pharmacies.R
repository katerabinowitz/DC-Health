setwd("/Users/katerabinowitz/Documents/DataLensDC Org")
library(rgdal)
#Read in pharmacy data, add icon
pharmacy= readOGR("http://opendata.dc.gov/datasets/2335ba275c3f4320a3113f13181eab56_9.geojson",
                           "OGRGeoJSON")
pharmData<-cbind(pharmacy@data, pharmacy@coords)
pharmacy@data$icon<-rep("pharmacy",122)

#Read in ward pop data
cbd<-readOGR("http://opendata.dc.gov/datasets/32143ca8983d4476b64f4202162bf61e_12.geojson",
             "OGRGeoJSON")
addAll<-SpatialPoints(pharmLatLong, proj4string=CRS(as.character("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")))
pharmCBD <- over(addAll, cbd)

pharmNoCBD<-cbind(pharmData,pharmCBD)
pharmNoCBD<-subset(pharmNoCBD,is.na(pharmNoCBD$ID))
table(pharmNoCBD$WARD)
