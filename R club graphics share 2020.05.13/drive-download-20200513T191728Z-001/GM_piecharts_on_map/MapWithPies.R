library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggmap)
library(scatterpie)

#set working directory to wherever you're saving files
setwd("C:/Rclub/GM_piecharts_on_map/")

#plot map of haplotype frequencies in Alaska

#load Yhap frequencies and population locations
popInfo<-read_excel("YhapFrequencies.xlsx",sheet="Yhap Frequencies")


#compute a bounding box
YhapPops_bbox<-make_bbox(lat=Lat,lon=Lon,data=popInfo)
YhapPops_bbox
#adjust bounding box for better framing of map
YhapPops_bbox[1]<-YhapPops_bbox[1]-2
YhapPops_bbox[2]<-YhapPops_bbox[2]-2
YhapPops_bbox[3]<-YhapPops_bbox[3]+2
YhapPops_bbox[4]<-YhapPops_bbox[4]+1

#make stamen map
YhapPops_stamenMap<-get_stamenmap(bbox=YhapPops_bbox, maptype="terrain",zoom=7)
#the zoom option controls the resultion, lower values give lower resolution and higher values give higher resolution
ggmap(YhapPops_stamenMap)


#load text placement for population labels
textLoc<-read_excel("YhapFrequencies.xlsx",sheet="Label Locations")
head(textLoc)

#save popInfo as new object called pie
#add radius column to specify size of pie charts
pie<-popInfo %>% mutate(radius=0.2) %>%select(-Population)
head(pie)

colnames(pie)<-c("Y1","Y2","Y3","Y4","F","U","lat","lon","radius")
pie<- pie %>% select(lon,lat,everything()) %>% select(-"F",-"U")

pie.list <- pie  %>%
  tidyr::gather(type, value, -lon, -lat, -radius) %>%
  tidyr::nest(type, value) %>%
  
  # make a pie chart from each row, & convert to grob
  mutate(pie.grob = purrr::map(data,
                               function(d) ggplotGrob(ggplot(d, 
                                                             aes(x = 1, y = value, fill = type)) +
                                                        geom_col(color = "black",
                                                                 show.legend = FALSE) +
                                                        coord_polar(theta = "y") +
                                                        theme_void()))) %>%
  
  # convert each grob to an annotation_custom layer. I've also adjusted the radius
  # value to a reasonable size (based on my screen resolutions).
  rowwise() %>%
  mutate(radius = radius * 4) %>%
  mutate(subgrob = list(annotation_custom(grob = pie.grob,
                                          xmin = lon - radius, xmax = lon + radius,
                                          ymin = lat - radius, ymax = lat + radius)))


ggmap(YhapPops_stamenMap)+geom_point(data=popInfo,aes(x=Lon,y=Lat),size=0,alpha=0)+
  coord_quickmap()+
  geom_tile(data = pie %>% tidyr::gather(type, value, -lon, -lat, -radius),
            aes(x = lon, y = lat, fill = type), 
            color = "black", width = 0.01, height = 0.01, 
            inherit.aes = FALSE)+
  pie.list$subgrob+
  labs(fill='Haplogroup') +
  theme(legend.title = element_text(size=16),legend.text=element_text(size=14))+
  geom_text(data=textLoc,aes(x=newLon,y=newLat,label=PopNum),size=6)

