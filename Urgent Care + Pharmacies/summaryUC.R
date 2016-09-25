library(dplyr)
library(plyr)
library(ggplot2)
library(rgdal)

### Read in Data ###
### Read in Data ###
### Read in Data ###
pharm<-read.csv("/Users/katerabinowitz/Documents/DataLensDC/FOIA-Requests/Pharmacy Locations/DCPharmacyLocations2016.csv",
                fill = FALSE, strip.white = TRUE,stringsAsFactors=FALSE)

uc<-read.csv("/Users/katerabinowitz/Documents/DataLensDC/DC-Health/Urgent Care + Pharmacies/urgentCare.csv",
             fill = FALSE, strip.white = TRUE,stringsAsFactors=FALSE)
uc<-filter(uc,type!="hospital")
table(uc$WARD_ID) 

pharmVac = readOGR("http://opendata.dc.gov/datasets/2335ba275c3f4320a3113f13181eab56_9.geojson",
                   "OGRGeoJSON")
pharmVac<-cbind(pharmVac@data,pharmVac@coords)
colnames(pharmVac)[c(15)]<-"WARD_ID"
pharmVac<-arrange(pharmVac,NAME)
#remove pharmacies that are closed or exclusive (i.e, schools, private practice, etc.)
noPharm<-c("American University Student Health Center","Arthritis and Rheumatism Associates, PC","Capital Pulmonary Internists, P.C.",
           "Chevy Chase Pediatric Center","Children's Pediatricians and Associates- Drs. Hudson & Simrel","Georgetown Day School Lower/Middle School Campus",
           "Mark M. Sklar, M.D.","MedStar Physician Partners- Washington Primary Care Physicians","Morton's Pharmacy",
           "Rite Aid Pharmacy #2710","SAFEWAY PHARMACY #0271","SAFEWAY PHARMACY #1395","SAFEWAY PHARMACY #4873","Spring Valley Health and Wellness",
           "4225 CONNECTICUT AVENUE NW")

pharmVac<-filter(pharmVac,!(NAME %in% noPharm | ADDRESS %in% noPharm))

pvSum<-cbind(plyr::count(pharm,"WARD_ID"),plyr::count(pharmVac,"WARD_ID"))

ucSum<-plyr::count(uc,"WARD_ID")
ward8UC<-data.frame(8,0)
colnames(ward8UC)<-c("WARD_ID","freq")
ucSum<-rbind(ucSum,ward8UC)

ucpSum<-cbind(pvSum,ucSum)[c(1,2,4,6)]
colnames(ucpSum)<-c("ward","pharmacy","pharmacyVaccine","urgentCare")

ucpSum<-mutate(ucpSum,ward=factor(ward))

### Bar graphs ###
### Bar graphs ###
### Bar graphs ###
ggplot(data = ucpSum, aes(x = ward, y = pharmacyVaccine)) + 
  geom_bar(stat="identity", colour="#38c7c7",fill="#38c7c7") + 
  scale_y_continuous(limits = c(0,40),expand=c(0,0)) +
  scale_x_discrete(expand=c(0,0)) +
  theme(axis.text.x = element_text()) +
  theme(axis.text.y = element_text()) +
  theme(plot.background = element_rect(fill = '#EFF0F1'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(text = element_text(size=13)) +
  theme(axis.title.x = element_text(color="#505050")) +
  theme(axis.title.y = element_text(color="#505050")) +
  theme(plot.title=element_text(family="Helvetica", size=15, face="bold")) +
  theme(plot.subtitle=element_text(color="#505050")) +
  theme(plot.caption=element_text(color="#808080")) +
  labs(x="Ward", y="Pharmacies administering vaccines", 
       title="Vaccine options vary dramatically by ward",
       subtitle="Number of pharmacies administering vaccines by ward",
       caption="\nSource: DC Open Data, DataLensDC")

ggplot(data = ucpSum, aes(x = ward, y = pharmacy)) + 
  geom_bar(stat="identity", colour="#38c7c7",fill="#38c7c7") + 
  scale_y_continuous(limits = c(0,40),expand=c(0,0)) +
  scale_x_discrete(expand=c(0,0)) +
  theme(axis.text.x = element_text()) +
  theme(axis.text.y = element_text()) +
  theme(plot.background = element_rect(fill = '#EFF0F1'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(text = element_text(size=13)) +
  theme(axis.title.x = element_text(color="#505050")) +
  theme(axis.title.y = element_text(color="#505050")) +
  theme(plot.title=element_text(family="Helvetica", size=15, face="bold")) +
  theme(plot.subtitle=element_text(color="#505050")) +
  theme(plot.caption=element_text(color="#808080")) +
  labs(x="Ward", y="Pharmacies", 
       title="Fewer Pharmacies East of the Anacostia",
       subtitle="Number of pharmacies by ward",
       caption="\nSource: DC Department of Health, DataLensDC")