setwd("/Users/katerabinowitz/Documents/DataLensDC/DC-Health/Urgent Care + Pharmacies")
library(rgdal)
library(tidyr)
library(dplyr)
library(ggmap)
### Read in clinic OpenDataDC data and subset to clinics serving general population ###
### Read in clinic OpenDataDC data and subset to clinics serving general population ###
### Read in clinic OpenDataDC data and subset to clinics serving general population ###
clinicGeo = readOGR("http://opendata.dc.gov/datasets/018890d7399245759f05c7932261ef44_7.geojson",
                  "OGRGeoJSON")
clinic<-cbind(clinicGeo@data, clinicGeo@coords)
colnames(clinic)
table(clinic$PrimaryCarePtWALKIN_UNSCHEDULED)
table(clinic$PRIMARY_CARE_INFOLIMITED_ACCESS)
walkIn<-filter(clinic,(grepl("Yes|Monday",PrimaryCarePtWALKIN_UNSCHEDULED) & 
                         !(grepl("call",PrimaryCarePtWALKIN_UNSCHEDULED))) & 
                 PRIMARY_CARE_INFOOPEN_TO_PUBLIC=="Yes")
#remove homeless centers 
healthClinic<-filter(walkIn, !(grepl("Unity",PrimaryCarePtNAME) &
     grepl("801 East|Blair|Central Union|Christ House|Federal City|Friendship Place|
      |Harbor Light|N Street Village|New York Ave|Pathways to Housing",PrimaryCarePtNAME)))
healthClinic<-healthClinic[c(2:3,128:129)]
colnames(healthClinic)<-c("name","address","lon","lat")

#remove clinics that are not walk ins
### I checked the websites and called clinics to verify that they accepted walk-ins; 
### only 3 do (that are not also hospitals which I pull in the next round)
healthClinic<-filter(healthClinic,grepl("Minnesota|Brentwood|Upper Cardozo",name))
healthClinic$type<-rep("Community Health\nCenter Walk-In",3)

### Read in Hospital data from OpenDataDC ###
### Read in Hospital data from OpenDataDC ###
### Read in Hospital data from OpenDataDC ###
hosGeo = readOGR("http://opendata.dc.gov/datasets/6c18bb76d8644bc1bf53cac2d2199564_4.geojson","OGRGeoJSON")
hosp<-cbind(hosGeo@data, hosGeo@coords)
str(hosp)
#only keep hospitals that provide care for general population (i.e., non-specialty)
hosp<-filter(hosp,TYPE=="Hospital" & !(is.na(ADULT_MEDICAL)))
hosp<-hosp[c(3:4,17:18)]
colnames(hosp)<-c("name","address","lon","lat")
hosp$type<-rep("hospital",7)

### Urgent care clinics not covered on OpenDataDC ###
### Urgent care clinics not covered on OpenDataDC ###
### Urgent care clinics not covered on OpenDataDC ###
#this dataset was scraped from https://www.urgentcarelocations.com/
#it was a good starting point, but required filtering and updates 
ucc<-read.csv("clinicLocRaw.csv",fill = FALSE, strip.white = TRUE,stringsAsFactors=FALSE)[c(2)]

ucc<-separate(ucc,uc,c("name","address"),sep=" - ")
ucc$name<-gsub("\n","",ucc$name)

#remove clinics that do not accept walk ins or are closed
ucc<-filter(ucc,!(name %in% c("A Doc At Your Door","Ivan L. Robinson Urgent Care","K Street Medical Care"))
                  & address !="4225 Connecticut Ave, Washington, DC 20008")

#add urgent care clinics that have opened since list
name<-c("MinuteClinic","MinuteClinic","Metro Immediate & Primary Care")
address<-c("6 Dupont Circle NW, Washington, DC 20036","1275 Pennsylvania Avenue NW, Washington, DC 20004",
           "1101 15th St NW, Washington, DC 20005")
newuc<-data.frame(name, address)

ucc<-rbind(ucc,newuc)

uccAdd<-geocode(ucc$address)

ucc<-cbind(ucc,uccAdd)
ucc$address<-gsub("\\,.*","",ucc$address)

ucc$type<-ifelse(grepl("Walgreens|MinuteClinic",ucc$name),"Retail Clinic","Urgent Care Clinic")

### All Together Now ###
### All Together Now ###
### All Together Now ###
uc<-rbind(healthClinic, hosp,ucc)

ucll<-uc[c(3:4)]
ward<-readOGR("http://opendata.dc.gov/datasets/a4442c906559456eb6ef3ea0898fe994_32.geojson",
              "OGRGeoJSON")
addAll<-SpatialPoints(ucll, proj4string=CRS(as.character("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")))
ucWard <- over(addAll, ward)[c(4)]

uc<-cbind(uc,ucWard)

table(uc$WARD_ID, uc$type)

write.csv(uc,"urgentCare.csv",row.names=FALSE)
