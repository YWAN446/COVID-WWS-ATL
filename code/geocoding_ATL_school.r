#package
library(ggmap)
library(ggrepel) #for in plot labeling;
library(ggplot2)
library(leaflet)

#register for google API; Please do not share the API key with others;
register_google(key="ADD YOUR API KEY")

geocode_apply<-function(x){
  geocode(paste0(x," School, Atlanta, Georgia"), output = "all")
}

list.school <- read.csv("./data/list of Atlanta schools.csv",stringsAsFactors=FALSE)

#do it only when you need to do the geocoding for address vectors;
# geocode_results<-sapply(list.school[,1], geocode_apply, simplify = F)
# 
# geocode_results0 <- geocode_results
# save(geocode_results0,file="./data/Atl_school.rda")
load("./data/Atl_school.rda")
geocode_results <- geocode_results0

condition_a <- sapply(geocode_results, function(x) x$status=="OK")
geocode_results<-geocode_results[condition_a]

results_b<-lapply(geocode_results, as.data.frame)

results_c<-lapply(results_b,function(x) subset(x, select=c("results.geometry.location.lat",
                                                           "results.geometry.location.lng")))
library(data.table)
results_d<-rbindlist(results_c)
results_e<-cbind(names(results_b),results_d)
names(results_e)[1] <- "school.name"
school.loc <- results_e
school.loc <- merge(school.loc,list.school,by.x="school.name",by.y="School",all.x=T)

school.loc$School.Group[which(school.loc$School.Group=="EM")] <- "ES"
school.loc$School.Group[which(school.loc$School.Group=="HM")] <- "HS"
school.loc$School.Group <- as.factor(school.loc$School.Group)
#map
ATL.school.map <- get_googlemap(center = c(lon = -84.42022, lat = 33.7703),
                             zoom = 11, scale = 2,
                             maptype ='roadmap',
                             color = 'color')
ATL.school.map12 <- get_googlemap(center = c(lon = -84.42022, lat = 33.7703),
                               zoom = 12, scale = 2,
                               maptype ='roadmap',
                               color = 'color')
p <- ggmap(ATL.school.map)

school_map <- leaflet() %>%
  addTiles() %>%  # use the default base map which is OpenStreetMap tiles
  addMarkers(lng=school.loc$results.geometry.location.lng, lat=school.loc$results.geometry.location.lat,
             popup=school.loc$school.name, group=school.loc$School.Group)
school_map

p <- ggmap(ATL.school.map)
p + geom_point(aes(x = results.geometry.location.lng, y = results.geometry.location.lat, fill=School.Group), data = school.loc, pch=21, size = 3.5) +
  geom_label_repel(aes(x=results.geometry.location.lng, y=results.geometry.location.lat, label = school.name),
                   data = school.loc,
                   box.padding   = 0.35, 
                   point.padding = 0.2,
                   size = 2,
                   segment.color = 'grey50')
