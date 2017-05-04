library(ggplot2)
library(acs)
library(RSocrata)
library(dplyr)
library(rgdal)
library(ggmap)
library(ggalt)
library(reshape2)

setwd("/Users/katerabinowitz/Documents/DataLensDC/DC-Health/Wealth-Heath-Gap")

### Read in DC Income and Health data ###
### Read in DC Income and Health data ###
### Read in DC Income and Health data ###
dc <- geo.make(state="DC",county="District of Columbia",tract="*")
inc15 <- acs.fetch(geography=dc, endyear=2015, table.number="B19013", col.names="pretty") 
incMed <- (as.data.frame(cbind(inc15@geography, inc15@estimate)))
colnames(incMed)[5] <- "medianInc"

incMed <- incMed %>% subset(medianInc<35000 | medianInc>110000) %>%
                    mutate(tractfips=as.numeric(paste0(state,'00',county,tract)),
                           incomeLevel=ifelse(medianInc<35000,"low","high")) %>%
                    select(NAME, medianInc, tractfips,incomeLevel,medianInc) %>%
                    arrange(incomeLevel)

health <- read.socrata("https://chronicdata.cdc.gov/resource/csmm-fdhi.csv?stateabbr=DC",app_token="fGAEKedAlpmFLvQGYFT6X6Dr2")
health <- health %>% subset(geographiclevel=="Census Tract")

### Merge and summarise to find income health disparaties ###
### Merge and summarise to find income health disparaties ###
### Merge and summarise to find income health disparaties ###
healthInc <- inner_join(incMed,health,by="tractfips")

healthIncSum <- healthInc %>% group_by(measure,incomeLevel) %>%
                              summarise(medProp=median(data_value))

healthIncSum <- dcast(healthIncSum, measure ~ incomeLevel) %>%
                mutate(ind=ifelse(measure=="Arthritis among adults aged >=18 Years","Arthritis",
                                  ifelse(measure=="Current asthma among adults aged >=18 Years","Asthma",
                                         ifelse(measure=="Diagnosed diabetes among adults aged >=18 Years","Diabetes",
                                                ifelse(measure=="High blood pressure among adults aged >=18 Years","High blood pressure",
                                                       ifelse(measure=="Mental health not good for >=14 days among adults aged >=18 Years","Mental",
                                                              ifelse(measure=="No leisure-time physical activity among adults aged >=18 Years","No leisure-time\nphysical activity",
                                                                     ifelse(measure=="Obesity among adults aged >=18 Years","Obesity",
                                                                            ifelse(measure=="Physical health not good for >=14 days among adults aged >=18 Years","Physical",
                                                                                   ifelse(measure=="Sleeping less than 7 hours among adults aged >=18 Years","Sleeping under\n7 hours",
                                                                                          measure)))))))))) %>%
                arrange(desc(low))

activity <- healthIncSum %>% filter(grepl("leisure|Sleep|Obesity",measure))
feel <- healthIncSum %>% filter(ind %in% c("Mental","Physical"))
illness <- healthIncSum %>% filter(ind %in% c("Arthritis","Asthma","Diabetes","High blood pressure"))

pdf("activity.pdf",width=6, height=4)
ggplot(activity, aes(x=high, xend=low, y=reorder(ind, low))) +
      geom_dumbbell(colour="#52616B",
      colour_x="#E1B16A",
      size_x=4,
      colour_xend="#78A5A3",
      size_xend=4) +
      scale_x_continuous(limits = c(0,50)) +
      theme_bw() +
      theme(axis.ticks=element_blank(),
            panel.grid.minor=element_blank(),
            panel.border=element_blank()) +
  labs(y="", x="", 
       title="A Wide Wealth Gap for Healthy Living",
       caption="\nSource: CDC 500 Cities Project")
dev.off()

pdf("health.pdf",width=6, height=4)
ggplot(feel, aes(x=high, xend=low, y=reorder(ind, low))) +
  geom_dumbbell(colour="#52616B",
                colour_x="#E1B16A",
                size_x=4,
                colour_xend="#78A5A3",
                size_xend=4) +
  scale_x_continuous(limits = c(0,20)) +
  theme_bw() +
  theme(axis.ticks=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank()) +
  labs(y="Poor Health for over 14 days", x="", 
       title="Higher Rates of Poor Health Among\nDC's Poorest Neighborhoods",
       caption="\nSource: CDC 500 Cities Project")
dev.off()

pdf("chronic.pdf",width=6, height=4)
ggplot(illness, aes(x=high, xend=low, y=reorder(ind, low))) +
  geom_dumbbell(colour="#52616B",
                colour_x="#E1B16A",
                size_x=4,
                colour_xend="#78A5A3",
                size_xend=4) +
  scale_x_continuous(limits = c(0,40)) +
  theme_bw() +
  theme(axis.ticks=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank()) +
  labs(y="", x="", 
       title="Greater Rates of Chronic Conditions in Poorest Communities",
       caption="\nSource: CDC 500 Cities Project")
dev.off()


### map lowest/highest income tracts for reference ###
tracts <- readOGR("http://opendata.dc.gov/datasets/6969dd63c5cb4d6aa32f15effb8311f3_8.geojson","OGRGeoJSON")

tract <- fortify(tracts, region="GEOID")
incMed$id <- as.character(incMed$tractfips)
mapTract <- left_join(tract,incMed,by="id") %>% 
            mutate(incomeLevel=ifelse(is.na(incomeLevel),"NA",incomeLevel))

Palette = c('#E1B16A','#78A5A3','#DFE0D4')

ggplot() +
  geom_polygon(data = mapTract, 
               aes(x = long, y = lat, group = group, fill = incomeLevel), 
               color = '#DFE0D4', size = 0.05) + 
               scale_fill_manual(values = Palette) +
               coord_map() +
               theme_nothing(legend = TRUE)
