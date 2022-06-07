library(raster)
library(rgdal)
library(rgeos)
library(ggplot2)
library(ggmap)
library(geosphere)
library(tidyverse)
library(sf)
library(maps)
library(igraph)
library(readxl)
library(gridExtra)
library(grid)
library(concaveman)
library(viridis)
library(RColorBrewer)
library(patchwork)

data.combined.dir <- '~/stat/students/Haisu Code summarization/Atlanta wastewater surveillance/Data updating/ALL-RESULTS_4.11.22.xlsx'

#Import data
dat.site <- read.csv("./data/wws_site.csv",stringsAsFactors = F)

tab_names <- c("All Upstream","All Influent")
dat.comb1 <- read_excel(data.combined.dir,sheet = tab_names[1])

#remove empty rows;
dat.comb1 <- dat.comb1[-which(is.na(dat.comb1$Manhole_location)),]
dat.comb1$location_type <- 'Community'
dat.comb1$location_type[which(dat.comb1$Location_type=="Correctional Facility")] <- "Correctional Facility"
dat.comb1$location_type[which(dat.comb1$Location_type %in% c("College","Elementary & Primary School","Elementary School","Elementary/Middle School","High School","Middle School"))] <- "School"
dat.comb1$location_type[which(dat.comb1$Manhole_location %in% c("Atlanta Domestic Airport 1","Atlanta Domestic Airport 1.2","Atlanta Domestic Airport 1.3","Atlanta Domestic Airport 1.4","Atlanta International Airport 1"))] <- "Airport"

dat.comb1 <- dat.comb1[,c("Manhole_location","location_type","Sample_type","Collect_date",'Concen_meth',"N1_ct1","N1_ct2","manhole_ID","Location_type")]

dat.comb2 <- read_excel(data.combined.dir,sheet = tab_names[2])
dat.comb2$location_type <- "Influent Line"
dat.comb2 <- dat.comb2[,c("Influent_Location","location_type","Sample_type","Collect_date",'Concen_meth',"N1_ct1","N1_ct2")]
dat.comb2$manhole_ID <- NA
dat.comb2$Location_type <- 'Influent Line'

names(dat.comb1)[1] <- "location"
names(dat.comb2)[1] <- "location"
dat.comb <- rbind(dat.comb1,dat.comb2) #There is a typo in dat.comb2 collection date;

#site name sub;
name.sub <- read.csv("./data/sample_site_name_sub.csv",stringsAsFactors = F)[,c("site.name","new.name")]
dat.comb <- merge(dat.comb,name.sub,by.x="location",by.y="site.name",all.x=T)
dat.comb$location[which(dat.comb$new.name!="")] <- dat.comb$new.name[which(dat.comb$new.name!="")]
dat.comb <- dat.comb[,-which(names(dat.comb)=="new.name")]
dat.comb$location[which(substr(dat.comb$location,1,8)=="Wildwood")] <- "Wildwood Townhomes"

dat.1 <- dat.comb[,c('location','Sample_type','Collect_date','Concen_meth','N1_ct1','N1_ct2','location_type')]

#Transfer the ct data.
dat.1$N1_ct1 <- as.numeric(dat.1$N1_ct1)
dat.1$N1_ct2 <- as.numeric(dat.1$N1_ct2)
dat.1$positive <- 1
dat.1$positive[which(dat.1$N1_ct1 >36 & dat.1$N1_ct2 > 36)] <- 2
dat.1$positive[which(dat.1$N1_ct1 <=36 & dat.1$N1_ct2 <= 36)] <- 3

#Filter the data to grab and NanoTrap
dat.1$Concen_meth[which(dat.1$Concen_meth %in% c("Nanotrap","nanoTrap"))] <- "NanoTrap"  #Make value consistent, run the table statement again to check
dat.1$Concen_meth[which(dat.1$Concen_meth %in% c("memfiltration","MemFiltration","Memfiltration"))] <- "memFiltration"
dat.1$Concen_meth[which(dat.1$Concen_meth=="Skimmed milk")] <- "Skimmed Milk"

#Not filter the results based on the methods;
dat.2 <- dat.1
dat.2$Collect_date <- as.Date(dat.2$Collect_date) #Date format need to be transformed

#Processing the data in same day
dat.2 <- dat.2 %>% group_by(location_type,location,Collect_date)
dat.3 <- dat.2 %>% summarise(positive = max(positive,na.rm = T))
dat.3$positive <- factor(dat.3$positive, levels=c(1,2,3),labels = c("Negative","Weak Positive","Positive"))
dat.3$color <- NA
dat.3$color[which(dat.3$positive=="Negative")] <- "lightblue"
dat.3$color[which(dat.3$positive=="Weak Positive")] <- "tomato"
dat.3$color[which(dat.3$positive=="Positive")] <- "red"

#Import case data
temp <- tempfile()
download.file("https://ga-covid19.ondemand.sas.com/docs/ga_covid_data.zip",temp)
fulton_case <- read.csv(unz(temp, "epicurve_rpt_date.csv"))
#fulton_case <- read.csv('Data updating/covid/epicurve_rpt_date.csv')
unlink(temp)
fulton_case <- fulton_case %>% filter(county %in% 'Fulton')
fulton_case <- fulton_case[,c('county','report_date','cases','moving_avg_cases')]
fulton_case$report_date <- as.Date(fulton_case$report_date)

#Plotting
temp <- dat.3[-which(duplicated(dat.3$location)),]
dat.3$location <- factor(dat.3$location,levels = temp$location[order(temp$location_type,temp$location)])

p1 <- ggplot(data=dat.3 %>% filter(Collect_date >= '2021-03-30' & Collect_date <= "2022-04-15") %>% filter(!is.na(location_type))) +
  geom_point(aes(x=Collect_date, y=location, col=positive)) +
  scale_colour_manual(values=c("Sky Blue","Orange","Red")) +
  facet_grid(location_type ~ ., scales = "free_y", space='free_y')
p1

p2_PhiLee <- ggplot(data=dat.3 %>% filter(Collect_date >= '2021-08-01' & Collect_date <= "2022-04-15") 
             %>% filter(!is.na(location_type))
             %>% filter(location_type %in% c("Community","Influent Line"))
             %>% filter(location %in% c("Phillip Lee","Peyton Woods Trail","Plainville Trail","Benjamin E Mays High School","Chatham Ave","Walmart","Larchwood"))) +
  geom_point(aes(x=Collect_date, y=location, col=positive)) +
  scale_colour_manual(values=c("Sky Blue","Orange","Red")) +
  facet_grid(location_type ~ ., scales = "free_y", space='free_y')

p2_SouFul <- ggplot(data=dat.3 %>% filter(Collect_date >= '2021-08-01' & Collect_date <= "2022-04-15") 
                    %>% filter(!is.na(location_type)) 
                    %>% filter(location %in% c("South Fulton","Mt. Gilead Rd","Guilford Forest Dr","Cascade Rd","Village Dr","Dodson Dr","Ivydale"))) +
  geom_point(aes(x=Collect_date, y=location, col=positive)) +
  scale_colour_manual(values=c("Sky Blue","Orange","Red")) +
  facet_grid(location_type ~ ., scales = "free_y", space='free_y')

p2_Jone <- ggplot(data=dat.3 %>% filter(Collect_date >= '2021-08-01' & Collect_date <= "2022-04-15") 
                    %>% filter(!is.na(location_type)) %>% filter(location_type!="School")
                    %>% filter(location %in% c("Jonesboro","Forest Park Rd -- North Site","Forest Park Rd -- South Site","South Atlanta High School 2","Southside Industrial Parkway","Ruby Harper Blvd"))) +
  geom_point(aes(x=Collect_date, y=location, col=positive)) +
  scale_colour_manual(values=c("Sky Blue","Orange","Red")) +
  facet_grid(location_type ~ ., scales = "free_y", space='free_y')

p2_Intr <- ggplot(data=dat.3 %>% filter(Collect_date >= '2021-08-01' & Collect_date <= "2022-04-15") 
                  %>% filter(!is.na(location_type)) 
                  %>% filter(location %in% c("Intrenchment","Lilac Lane","Oakview Rd","Montgomery St and Woodbine Ave"))) +
  geom_point(aes(x=Collect_date, y=location, col=positive)) +
  scale_colour_manual(values=c("Sky Blue","Orange","Red")) +
  facet_grid(location_type ~ ., scales = "free_y", space='free_y')


p2 <- grid.arrange(p2_PhiLee,p2_SouFul,p2_Jone,p2_Intr,nrow = 4,heights = c(0.275,0.275,0.25,0.2))

#mapping
source("./code/lucy-master/R/misc-utilities.R")
source("./code/lucy-master/R/graph-plots.R")
source("./code/lucy-master/R/graph-sim.R")
source("./code/lucy-master/R/lucy.R")
source("./code/lucy-master/R/propagation.R")

load("./net.rda")

#load sewage shapefiles;
sewage1 <- readOGR("./data/shapefiles/","SanitarySewerMains")
sewage2 <- readOGR("./data/shapefiles/","SanitarySewerStructures")

sewage_1 <- spTransform(sewage1, CRS("+proj=longlat +datum=WGS84"))
sewage_2 <- spTransform(sewage2, CRS("+proj=longlat +datum=WGS84"))

select.sites <- c(which(V(net)$name=="13760400901"),which(V(net)$name=="13760401701"),which(V(net)$name=="13760400801"),
                  which(V(net)$name=="23330211001"),which(V(net)$name=="23340422106"),which(V(net)$name=="23330209701"))
target.n.sites <- 7
site.names <- c("Phillip Lee","Old Winn Dixie","South Fulton","Jonesboro","Flint","Intrenchment")

#temp.col <- viridis(length(select.sites))
temp.col <- heat.colors(length(select.sites))
temp.col <- c("tomato","palegreen","gold","hotpink","cyan","red2")
select.sites.ID <- V(net)$name[select.sites]
col.select.sites <- rep("black",length(V(net)))
for (i in 1:length(select.sites)){
  col.select.sites[c(which(V(net)$name==select.sites.ID[i]), get_upstream_nodes(net,which(V(net)$name==select.sites.ID[i])))] <- temp.col[i]
}
#col.select.sites[select.sites] <- "red"
size.select.sites <- rep(0.2,length(V(net)))
size.select.sites[select.sites] <- 1
# pdf(file=paste0("./plots/SewageSite_withCatch",target.pop,".pdf"),height=40,width=40)
# plot(net,vertex.size=size.select.sites, edge.arrow.size=0.1, vertex.label=NA,vertex.color=col.select.sites,vertex.frame.color=col.select.sites,layout=as.matrix(nodes[,2:3]))
# dev.off()

label.select.sites <- rep("",length(V(net)))
for (i in 1:length(select.sites)){
  label.select.sites[which(V(net)$name == select.sites.ID[i])] <- select.sites.ID[i]
}


###################################################
#Phillip Lee;
i.site=1

influ.net.node.ind <- which(col.select.sites==temp.col[i.site])
influ.net <- induced_subgraph(net,influ.net.node.ind)
#count how many nodes upstream
influ.cnt.up.nodes <- c()
for (i in 1:length(V(influ.net))){
  print(i)
  influ.cnt.up.nodes[i] <- length(get_upstream_nodes(influ.net,i))
  cat("\014")
}
influ.net.manholes <- data.frame(cbind(V(net)$name[influ.net.node.ind],influ.cnt.up.nodes))
colnames(influ.net.manholes) <- c("ID","cnt")
influ.net.manholes$cnt <- as.numeric(influ.net.manholes$cnt)

philip_lee_manholes <- dat.manhole[which(col.select.sites==temp.col[i.site]),]
sub.net.manholes <- philip_lee_manholes
philip_lee_catch_area <- data.frame(concaveman(as.matrix(philip_lee_manholes[,c("long","lat")]),1))
names(philip_lee_catch_area) <- c("long","lat")

philip_lee_polygon <- philip_lee_catch_area %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
philip_lee_polygon <- st_buffer(philip_lee_polygon, dist = 0)

k.site.ID <- V(net)$name[select.sites[i.site]]

sub.net.node.ind <- sort(c(which(V(influ.net)$name==k.site.ID), get_upstream_nodes(influ.net,which(V(influ.net)$name==k.site.ID))))
sub.net <- induced_subgraph(influ.net,sub.net.node.ind)

#by design;
#selected_sites_ID <- c("23150132801","13950102701","13860410101","13950104101","23060312401")
selected_sites_ID <- c("23060310701","13860413001","13950110501","23150132801","13950102901","23060408001")

selected_sites_ID_nodes <- list()
selected_sites_ID_n_nodes <- c()
for (i in 1:length(selected_sites_ID)){
  selected_sites_ID_nodes[[i]] <- c(which(V(sub.net)$name==selected_sites_ID[i]),get_upstream_nodes(sub.net,which(V(sub.net)$name==selected_sites_ID[i])))
  selected_sites_ID_n_nodes[i] <- length(selected_sites_ID_nodes[[i]])
}

philip_lee_sewage <- sewage_1[which(sewage_1$UNITID %in% philip_lee_manholes$UNITID & sewage_1$UNITID2 %in% philip_lee_manholes$UNITID),]

pl_catch_area1 <- data.frame(concaveman(as.matrix(philip_lee_manholes[selected_sites_ID_nodes[[1]],c("long","lat")]),1))
names(pl_catch_area1) <- c("long","lat")

pl_polygon1 <- pl_catch_area1 %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
pl_polygon1 <- st_buffer(pl_polygon1, dist = 0)

pl_catch_area2 <- data.frame(concaveman(as.matrix(philip_lee_manholes[selected_sites_ID_nodes[[2]],c("long","lat")]),1))
names(pl_catch_area2) <- c("long","lat")

pl_polygon2 <- pl_catch_area2 %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
pl_polygon2 <- st_buffer(pl_polygon2, dist = 0)

pl_catch_area3 <- data.frame(concaveman(as.matrix(philip_lee_manholes[selected_sites_ID_nodes[[3]],c("long","lat")]),1))
names(pl_catch_area3) <- c("long","lat")

pl_polygon3 <- pl_catch_area3 %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
pl_polygon3 <- st_buffer(pl_polygon3, dist = 0)

pl_catch_area4 <- data.frame(concaveman(as.matrix(philip_lee_manholes[selected_sites_ID_nodes[[4]],c("long","lat")]),1))
names(pl_catch_area4) <- c("long","lat")

pl_polygon4 <- pl_catch_area4 %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
pl_polygon4 <- st_buffer(pl_polygon4, dist = 0)

pl_catch_area5 <- data.frame(concaveman(as.matrix(philip_lee_manholes[selected_sites_ID_nodes[[5]],c("long","lat")]),1))
names(pl_catch_area5) <- c("long","lat")

pl_polygon5 <- pl_catch_area5 %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
pl_polygon5 <- st_buffer(pl_polygon5, dist = 0)

pl_catch_area6 <- data.frame(concaveman(as.matrix(philip_lee_manholes[selected_sites_ID_nodes[[6]],c("long","lat")]),1))
names(pl_catch_area6) <- c("long","lat")

pl_polygon6 <- pl_catch_area6 %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
pl_polygon6 <- st_buffer(pl_polygon6, dist = 0)

# p_map1 <- ggplot() + geom_sf(data = philip_lee_polygon, fill="gray") +
#   geom_path(philip_lee_sewage,mapping = aes(x=long, y=lat,group=group)) +
#   geom_sf(data = philip_lee_cases %>% filter(report_date>=as.Date("03/15/2021",format = "%m/%d/%Y")), fill=NA, color="gold")

pl_sites <- c("Peyton Woods Trail","Plainville Trail","Benjamin E Mays High School","Chatham Ave","Walmart","Larchwood")
map_PhiLee <- ggplot() + geom_sf(data = philip_lee_polygon, fill="gray") +
  geom_sf(data = pl_polygon1, aes(fill=pl_sites[1])) +
  geom_sf(data = pl_polygon2, aes(fill=pl_sites[2])) +
  geom_sf(data = pl_polygon3, aes(fill=pl_sites[3])) +
  geom_sf(data = pl_polygon4, aes(fill=pl_sites[4])) +
  geom_sf(data = pl_polygon5, aes(fill=pl_sites[5])) +
  geom_sf(data = pl_polygon6, aes(fill=pl_sites[6])) +
  geom_point(data=philip_lee_manholes[which(sub.net.manholes$UNITID %in% selected_sites_ID),],aes(x=long,y=lat),size=4,col="black")+
  geom_path(philip_lee_sewage,mapping = aes(x=long, y=lat,group=group)) +
  scale_fill_manual(values = c("Peyton Woods Trail"="tomato","Plainville Trail"="cyan","Benjamin E Mays High School"="palegreen","Chatham Ave"="hotpink","Walmart"="red2","Larchwood"="gold"),
                    breaks = c("Peyton Woods Trail","Plainville Trail","Benjamin E Mays High School","Chatham Ave","Walmart","Larchwood"),
                    name = "Catchment") +
  guides(fill = guide_legend(override.aes = list(color = "black"))) +
  scale_color_identity(guide = "legend") +
  ggtitle("Phillip Lee")
map_PhiLee
PhiLee <- grid.arrange(map_PhiLee,p2_PhiLee,nrow = 2,heights = c(0.6,0.4))
ggsave(plot=PhiLee,file="./plots/res_PhiLee.pdf",height = 8,width = 8)

###################################################

###################################################
#South Fulton;
i.site=3

influ.net.node.ind <- which(col.select.sites==temp.col[i.site])
influ.net <- induced_subgraph(net,influ.net.node.ind)
#count how many nodes upstream
influ.cnt.up.nodes <- c()
for (i in 1:length(V(influ.net))){
  print(i)
  influ.cnt.up.nodes[i] <- length(get_upstream_nodes(influ.net,i))
  cat("\014")
}
influ.net.manholes <- data.frame(cbind(V(net)$name[influ.net.node.ind],influ.cnt.up.nodes))
colnames(influ.net.manholes) <- c("ID","cnt")
influ.net.manholes$cnt <- as.numeric(influ.net.manholes$cnt)

south_fulton_manholes <- dat.manhole[which(col.select.sites==temp.col[i.site]),]
sub.net.manholes <- south_fulton_manholes
south_fulton_catch_area <- data.frame(concaveman(as.matrix(south_fulton_manholes[,c("long","lat")]),1))
names(south_fulton_catch_area) <- c("long","lat")

south_fulton_polygon <- south_fulton_catch_area %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
south_fulton_polygon <- st_buffer(south_fulton_polygon, dist = 0)

k.site.ID <- V(net)$name[select.sites[i.site]]

sub.net.node.ind <- sort(c(which(V(influ.net)$name==k.site.ID), get_upstream_nodes(influ.net,which(V(influ.net)$name==k.site.ID))))
sub.net <- induced_subgraph(influ.net,sub.net.node.ind)

selected_sites_ID <- c("13940200401","13850103501","13850100701","13950301801","23040112201","23040107701")

selected_sites_ID_nodes <- list()
selected_sites_ID_n_nodes <- c()
for (i in 1:length(selected_sites_ID)){
  selected_sites_ID_nodes[[i]] <- c(which(V(sub.net)$name==selected_sites_ID[i]),get_upstream_nodes(sub.net,which(V(sub.net)$name==selected_sites_ID[i])))
  selected_sites_ID_n_nodes[i] <- length(selected_sites_ID_nodes[[i]])
}

south_fulton_sewage <- sewage_1[which(sewage_1$UNITID %in% south_fulton_manholes$UNITID & sewage_1$UNITID2 %in% south_fulton_manholes$UNITID),]

pl_catch_area1 <- data.frame(concaveman(as.matrix(south_fulton_manholes[selected_sites_ID_nodes[[1]],c("long","lat")]),1))
names(pl_catch_area1) <- c("long","lat")

pl_polygon1 <- pl_catch_area1 %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
pl_polygon1 <- st_buffer(pl_polygon1, dist = 0)

pl_catch_area2 <- data.frame(concaveman(as.matrix(south_fulton_manholes[selected_sites_ID_nodes[[2]],c("long","lat")]),1))
names(pl_catch_area2) <- c("long","lat")

pl_polygon2 <- pl_catch_area2 %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
pl_polygon2 <- st_buffer(pl_polygon2, dist = 0)

pl_catch_area3 <- data.frame(concaveman(as.matrix(south_fulton_manholes[selected_sites_ID_nodes[[3]],c("long","lat")]),1))
names(pl_catch_area3) <- c("long","lat")

pl_polygon3 <- pl_catch_area3 %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
pl_polygon3 <- st_buffer(pl_polygon3, dist = 0)

pl_catch_area4 <- data.frame(concaveman(as.matrix(south_fulton_manholes[selected_sites_ID_nodes[[4]],c("long","lat")]),1))
names(pl_catch_area4) <- c("long","lat")

pl_polygon4 <- pl_catch_area4 %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
pl_polygon4 <- st_buffer(pl_polygon4, dist = 0)

pl_catch_area5 <- data.frame(concaveman(as.matrix(south_fulton_manholes[selected_sites_ID_nodes[[5]],c("long","lat")]),1))
names(pl_catch_area5) <- c("long","lat")

pl_polygon5 <- pl_catch_area5 %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
pl_polygon5 <- st_buffer(pl_polygon5, dist = 0)

pl_catch_area6 <- data.frame(concaveman(as.matrix(south_fulton_manholes[selected_sites_ID_nodes[[6]],c("long","lat")]),1))
names(pl_catch_area6) <- c("long","lat")

pl_polygon6 <- pl_catch_area6 %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
pl_polygon6 <- st_buffer(pl_polygon6, dist = 0)

# p_map1 <- ggplot() + geom_sf(data = south_fulton_polygon, fill="gray") +
#   geom_path(south_fulton_sewage,mapping = aes(x=long, y=lat,group=group)) +
#   geom_sf(data = south_fulton_cases %>% filter(report_date>=as.Date("03/15/2021",format = "%m/%d/%Y")), fill=NA, color="gold")

pl_sites <- c("Mt. Gilead Rd","Guilford Forest Dr","Cascade Rd","Village Dr","Dodson Dr","Ivydale")
map_SouFul <- ggplot() + geom_sf(data = south_fulton_polygon, fill="gray") +
  geom_sf(data = pl_polygon1, aes(fill=pl_sites[1])) +
  geom_sf(data = pl_polygon2, aes(fill=pl_sites[2])) +
  geom_sf(data = pl_polygon3, aes(fill=pl_sites[3])) +
  geom_sf(data = pl_polygon4, aes(fill=pl_sites[4])) +
  geom_sf(data = pl_polygon5, aes(fill=pl_sites[5])) +
  geom_sf(data = pl_polygon6, aes(fill=pl_sites[6])) +
  geom_point(data=south_fulton_manholes[which(sub.net.manholes$UNITID %in% selected_sites_ID),],aes(x=long,y=lat),size=4,col="black")+
  geom_path(south_fulton_sewage,mapping = aes(x=long, y=lat,group=group)) +
  scale_fill_manual(values = c("Mt. Gilead Rd"="tomato","Guilford Forest Dr"="cyan","Cascade Rd"="palegreen","Village Dr"="hotpink","Dodson Dr"="red2","Ivydale"="gold"),
                    breaks = c("Mt. Gilead Rd","Guilford Forest Dr","Cascade Rd","Village Dr","Dodson Dr","Ivydale"),
                    name = "Catchment") +
  guides(fill = guide_legend(override.aes = list(color = "black"))) +
  scale_color_identity(guide = "legend") +
  ggtitle("South Fulton")
map_SouFul

SouFul <- grid.arrange(map_SouFul,p2_SouFul,nrow = 2,heights = c(0.6,0.4))
ggsave(plot=SouFul,file="./plots/res_SouFul.pdf",height = 8,width = 8)
###################################################

###################################################
#Jonesboro;
i.site=4

influ.net.node.ind <- which(col.select.sites==temp.col[i.site])
influ.net <- induced_subgraph(net,influ.net.node.ind)
#count how many nodes upstream
influ.cnt.up.nodes <- c()
for (i in 1:length(V(influ.net))){
  print(i)
  influ.cnt.up.nodes[i] <- length(get_upstream_nodes(influ.net,i))
  cat("\014")
}
influ.net.manholes <- data.frame(cbind(V(net)$name[influ.net.node.ind],influ.cnt.up.nodes))
colnames(influ.net.manholes) <- c("ID","cnt")
influ.net.manholes$cnt <- as.numeric(influ.net.manholes$cnt)

jonesboro_manholes <- dat.manhole[which(col.select.sites==temp.col[i.site]),]
sub.net.manholes <- jonesboro_manholes
jonesboro_catch_area <- data.frame(concaveman(as.matrix(jonesboro_manholes[,c("long","lat")]),1))
names(jonesboro_catch_area) <- c("long","lat")

jonesboro_polygon <- jonesboro_catch_area %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
jonesboro_polygon <- st_buffer(jonesboro_polygon, dist = 0)

k.site.ID <- V(net)$name[select.sites[i.site]]

sub.net.node.ind <- sort(c(which(V(influ.net)$name==k.site.ID), get_upstream_nodes(influ.net,which(V(influ.net)$name==k.site.ID))))
sub.net <- induced_subgraph(influ.net,sub.net.node.ind)

selected_sites_ID <- c("23330214201","23320205701","23330200801","23330305601","23320107101")

selected_sites_ID_nodes <- list()
selected_sites_ID_n_nodes <- c()
for (i in 1:length(selected_sites_ID)){
  selected_sites_ID_nodes[[i]] <- c(which(V(sub.net)$name==selected_sites_ID[i]),get_upstream_nodes(sub.net,which(V(sub.net)$name==selected_sites_ID[i])))
  selected_sites_ID_n_nodes[i] <- length(selected_sites_ID_nodes[[i]])
}

jonesboro_sewage <- sewage_1[which(sewage_1$UNITID %in% jonesboro_manholes$UNITID & sewage_1$UNITID2 %in% jonesboro_manholes$UNITID),]

pl_catch_area1 <- data.frame(concaveman(as.matrix(jonesboro_manholes[selected_sites_ID_nodes[[1]],c("long","lat")]),1))
names(pl_catch_area1) <- c("long","lat")

pl_polygon1 <- pl_catch_area1 %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
pl_polygon1 <- st_buffer(pl_polygon1, dist = 0)

pl_catch_area2 <- data.frame(concaveman(as.matrix(jonesboro_manholes[selected_sites_ID_nodes[[2]],c("long","lat")]),1))
names(pl_catch_area2) <- c("long","lat")

pl_polygon2 <- pl_catch_area2 %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
pl_polygon2 <- st_buffer(pl_polygon2, dist = 0)

pl_catch_area3 <- data.frame(concaveman(as.matrix(jonesboro_manholes[selected_sites_ID_nodes[[3]],c("long","lat")]),1))
names(pl_catch_area3) <- c("long","lat")

pl_polygon3 <- pl_catch_area3 %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
pl_polygon3 <- st_buffer(pl_polygon3, dist = 0)

pl_catch_area4 <- data.frame(concaveman(as.matrix(jonesboro_manholes[selected_sites_ID_nodes[[4]],c("long","lat")]),1))
names(pl_catch_area4) <- c("long","lat")

pl_polygon4 <- pl_catch_area4 %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
pl_polygon4 <- st_buffer(pl_polygon4, dist = 0)

pl_catch_area5 <- data.frame(concaveman(as.matrix(jonesboro_manholes[selected_sites_ID_nodes[[5]],c("long","lat")]),1))
names(pl_catch_area5) <- c("long","lat")

pl_polygon5 <- pl_catch_area5 %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
pl_polygon5 <- st_buffer(pl_polygon5, dist = 0)

# p_map1 <- ggplot() + geom_sf(data = jonesboro_polygon, fill="gray") +
#   geom_path(jonesboro_sewage,mapping = aes(x=long, y=lat,group=group)) +
#   geom_sf(data = jonesboro_cases %>% filter(report_date>=as.Date("03/15/2021",format = "%m/%d/%Y")), fill=NA, color="gold")

pl_sites <- c("Forest Park Rd -- North Site","Forest Park Rd -- South Site","South Atlanta High School 2","Southside Industrial Parkway","Ruby Harper Blvd")
map_Jone <- ggplot() + geom_sf(data = jonesboro_polygon, fill="gray") +
  geom_sf(data = pl_polygon1, aes(fill=pl_sites[1])) +
  geom_sf(data = pl_polygon2, aes(fill=pl_sites[2])) +
  geom_sf(data = pl_polygon3, aes(fill=pl_sites[3])) +
  geom_sf(data = pl_polygon4, aes(fill=pl_sites[4])) +
  geom_sf(data = pl_polygon5, aes(fill=pl_sites[5])) +
  geom_point(data=jonesboro_manholes[which(sub.net.manholes$UNITID %in% selected_sites_ID),],aes(x=long,y=lat),size=4,col="black")+
  geom_path(jonesboro_sewage,mapping = aes(x=long, y=lat,group=group)) +
  scale_fill_manual(values = c("Forest Park Rd -- North Site"="tomato","Forest Park Rd -- South Site"="cyan","South Atlanta High School 2"="palegreen","Southside Industrial Parkway"="hotpink","Ruby Harper Blvd"="red2"),
                    breaks = c("Forest Park Rd -- North Site","Forest Park Rd -- South Site","South Atlanta High School 2","Southside Industrial Parkway","Ruby Harper Blvd"),
                    name = "Catchment") +
  guides(fill = guide_legend(override.aes = list(color = "black"))) +
  scale_color_identity(guide = "legend") +
  ggtitle("Jonesboro")
map_Jone

Jone <- grid.arrange(map_Jone,p2_Jone,nrow = 2,heights = c(0.6,0.4))
ggsave(plot=Jone,file="./plots/res_Jone.pdf",height = 8,width = 8)

###################################################

###################################################
#Intrenchment;
i.site=6

influ.net.node.ind <- which(col.select.sites==temp.col[i.site])
influ.net <- induced_subgraph(net,influ.net.node.ind)
#count how many nodes upstream
influ.cnt.up.nodes <- c()
for (i in 1:length(V(influ.net))){
  print(i)
  influ.cnt.up.nodes[i] <- length(get_upstream_nodes(influ.net,i))
  cat("\014")
}
influ.net.manholes <- data.frame(cbind(V(net)$name[influ.net.node.ind],influ.cnt.up.nodes))
colnames(influ.net.manholes) <- c("ID","cnt")
influ.net.manholes$cnt <- as.numeric(influ.net.manholes$cnt)

intrenchment_manholes <- dat.manhole[which(col.select.sites==temp.col[i.site]),]
sub.net.manholes <- intrenchment_manholes
intrenchment_catch_area <- data.frame(concaveman(as.matrix(intrenchment_manholes[,c("long","lat")]),1))
names(intrenchment_catch_area) <- c("long","lat")

intrenchment_polygon <- intrenchment_catch_area %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
intrenchment_polygon <- st_buffer(intrenchment_polygon, dist = 0)

k.site.ID <- V(net)$name[select.sites[i.site]]

sub.net.node.ind <- sort(c(which(V(influ.net)$name==k.site.ID), get_upstream_nodes(influ.net,which(V(influ.net)$name==k.site.ID))))
sub.net <- induced_subgraph(influ.net,sub.net.node.ind)

selected_sites_ID <- c("23550100401","23560310401","23460337501")

selected_sites_ID_nodes <- list()
selected_sites_ID_n_nodes <- c()
for (i in 1:length(selected_sites_ID)){
  selected_sites_ID_nodes[[i]] <- c(which(V(sub.net)$name==selected_sites_ID[i]),get_upstream_nodes(sub.net,which(V(sub.net)$name==selected_sites_ID[i])))
  selected_sites_ID_n_nodes[i] <- length(selected_sites_ID_nodes[[i]])
}

intrenchment_sewage <- sewage_1[which(sewage_1$UNITID %in% intrenchment_manholes$UNITID & sewage_1$UNITID2 %in% intrenchment_manholes$UNITID),]

pl_catch_area1 <- data.frame(concaveman(as.matrix(intrenchment_manholes[selected_sites_ID_nodes[[1]],c("long","lat")]),1))
names(pl_catch_area1) <- c("long","lat")

pl_polygon1 <- pl_catch_area1 %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
pl_polygon1 <- st_buffer(pl_polygon1, dist = 0)

pl_catch_area2 <- data.frame(concaveman(as.matrix(intrenchment_manholes[selected_sites_ID_nodes[[2]],c("long","lat")]),1))
names(pl_catch_area2) <- c("long","lat")

pl_polygon2 <- pl_catch_area2 %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
pl_polygon2 <- st_buffer(pl_polygon2, dist = 0)

pl_catch_area3 <- data.frame(concaveman(as.matrix(intrenchment_manholes[selected_sites_ID_nodes[[3]],c("long","lat")]),1))
names(pl_catch_area3) <- c("long","lat")

pl_polygon3 <- pl_catch_area3 %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
pl_polygon3 <- st_buffer(pl_polygon3, dist = 0)

pl_sites <- c("Lilac Lane","Oakview Rd","Montgomery St and Woodbine Ave")
map_Intr <- ggplot() + geom_sf(data = intrenchment_polygon, fill="gray") +
  geom_sf(data = pl_polygon1, aes(fill=pl_sites[1])) +
  geom_sf(data = pl_polygon2, aes(fill=pl_sites[2])) +
  geom_sf(data = pl_polygon3, aes(fill=pl_sites[3])) +
  geom_point(data=intrenchment_manholes[which(sub.net.manholes$UNITID %in% selected_sites_ID),],aes(x=long,y=lat),size=4,col="black")+
  geom_path(intrenchment_sewage,mapping = aes(x=long, y=lat,group=group)) +
  scale_fill_manual(values = c("Lilac Lane"="tomato","Oakview Rd"="cyan","Montgomery St and Woodbine Ave"="palegreen"),
                    breaks = c("Lilac Lane","Oakview Rd","Montgomery St and Woodbine Ave"),
                    name = "Catchment") +
  guides(fill = guide_legend(override.aes = list(color = "black"))) +
  scale_color_identity(guide = "legend") +
  ggtitle("Intrenchment")
map_Intr
Intr <- grid.arrange(map_Intr,p2_Intr,nrow = 2,heights = c(0.8,0.2))
ggsave(plot=Intr,file="./plots/res_Intr.pdf",height = 8,width = 8)


###################################################

# PhiLee <- grid.arrange(map_PhiLee,p2_PhiLee,nrow = 2,heights = c(0.6,0.4))
# SouFul <- grid.arrange(map_SouFul,p2_SouFul,nrow = 2,heights = c(0.6,0.4))
# Jone <- grid.arrange(map_Jone,p2_Jone,nrow = 2,heights = c(0.6,0.4))
# Intr <- grid.arrange(map_Intr,p2_Intr,nrow = 2,heights = c(0.6,0.4))

###case data;
#read the geocoded data;
geocode.cases <- read.csv("./data/geocoded data/emoryGeocodeNCOV__20220510.csv",stringsAsFactors = F)
long.range <- range(dat.manhole$long)
lat.range <- range(dat.manhole$lat)
#subset the cases to the area;
geocode.cases <- geocode.cases[which(geocode.cases$RES_LON>=long.range[1] & geocode.cases$RES_LON<=long.range[2] &
                                       geocode.cases$RES_LAT>=lat.range[1] & geocode.cases$RES_LAT<=lat.range[2]),]

#read pui data;
pui.data <- read.csv("./data/geocoded data/pui_emory_extract_v2_202205081315.csv",stringsAsFactors = F)

###################################################
#Phillip Lee;
i.site=1

influ.net.node.ind <- which(col.select.sites==temp.col[i.site])
influ.net <- induced_subgraph(net,influ.net.node.ind)
#count how many nodes upstream
influ.cnt.up.nodes <- c()
for (i in 1:length(V(influ.net))){
  print(i)
  influ.cnt.up.nodes[i] <- length(get_upstream_nodes(influ.net,i))
  cat("\014")
}
influ.net.manholes <- data.frame(cbind(V(net)$name[influ.net.node.ind],influ.cnt.up.nodes))
colnames(influ.net.manholes) <- c("ID","cnt")
influ.net.manholes$cnt <- as.numeric(influ.net.manholes$cnt)

philip_lee_manholes <- dat.manhole[which(col.select.sites==temp.col[i.site]),]
sub.net.manholes <- philip_lee_manholes
philip_lee_catch_area <- data.frame(concaveman(as.matrix(philip_lee_manholes[,c("long","lat")]),1))
names(philip_lee_catch_area) <- c("long","lat")

philip_lee_polygon <- philip_lee_catch_area %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
philip_lee_polygon <- st_buffer(philip_lee_polygon, dist = 0)

sp.cases <- st_as_sf(geocode.cases, coords = c("RES_LON","RES_LAT"), crs = 4326)
philip_lee_cases <- st_intersection(sp.cases, philip_lee_polygon)

philip_lee_cases <- merge(philip_lee_cases,pui.data[,c("QARESPONSEID","Q174631","Q174656")],by="QARESPONSEID",all.x=T)
names(philip_lee_cases)[which(names(philip_lee_cases)=="Q174631")] <- "report_date"
names(philip_lee_cases)[which(names(philip_lee_cases)=="Q174656")] <- "symptom_onset"
philip_lee_cases$report_date <- as.Date(philip_lee_cases$report_date,format = "%m/%d/%Y")
philip_lee_cases$symptom_onset <- as.Date(philip_lee_cases$symptom_onset,format = "%m/%d/%Y")

k.site.ID <- V(net)$name[select.sites[i.site]]

sub.net.node.ind <- sort(c(which(V(influ.net)$name==k.site.ID), get_upstream_nodes(influ.net,which(V(influ.net)$name==k.site.ID))))
sub.net <- induced_subgraph(influ.net,sub.net.node.ind)

selected_sites_ID <- c("23060310701","13860413001","13950110501","23150132801","13950102901","23060408001")

selected_sites_ID_nodes <- list()
selected_sites_ID_n_nodes <- c()
for (i in 1:length(selected_sites_ID)){
  selected_sites_ID_nodes[[i]] <- c(which(V(sub.net)$name==selected_sites_ID[i]),get_upstream_nodes(sub.net,which(V(sub.net)$name==selected_sites_ID[i])))
  selected_sites_ID_n_nodes[i] <- length(selected_sites_ID_nodes[[i]])
}

philip_lee_sewage <- sewage_1[which(sewage_1$UNITID %in% philip_lee_manholes$UNITID & sewage_1$UNITID2 %in% philip_lee_manholes$UNITID),]

pl_catch_area1 <- data.frame(concaveman(as.matrix(philip_lee_manholes[selected_sites_ID_nodes[[1]],c("long","lat")]),1))
names(pl_catch_area1) <- c("long","lat")

pl_polygon1 <- pl_catch_area1 %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
pl_polygon1 <- st_buffer(pl_polygon1, dist = 0)
pl_cases1 <- st_intersection(philip_lee_cases, pl_polygon1)

pl_catch_area2 <- data.frame(concaveman(as.matrix(philip_lee_manholes[selected_sites_ID_nodes[[2]],c("long","lat")]),1))
names(pl_catch_area2) <- c("long","lat")

pl_polygon2 <- pl_catch_area2 %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
pl_polygon2 <- st_buffer(pl_polygon2, dist = 0)
pl_cases2 <- st_intersection(philip_lee_cases, pl_polygon2)

pl_catch_area3 <- data.frame(concaveman(as.matrix(philip_lee_manholes[selected_sites_ID_nodes[[3]],c("long","lat")]),1))
names(pl_catch_area3) <- c("long","lat")

pl_polygon3 <- pl_catch_area3 %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
pl_polygon3 <- st_buffer(pl_polygon3, dist = 0)
pl_cases3 <- st_intersection(philip_lee_cases, pl_polygon3)

pl_catch_area4 <- data.frame(concaveman(as.matrix(philip_lee_manholes[selected_sites_ID_nodes[[4]],c("long","lat")]),1))
names(pl_catch_area4) <- c("long","lat")

pl_polygon4 <- pl_catch_area4 %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
pl_polygon4 <- st_buffer(pl_polygon4, dist = 0)
pl_cases4 <- st_intersection(philip_lee_cases, pl_polygon4)

pl_catch_area5 <- data.frame(concaveman(as.matrix(philip_lee_manholes[selected_sites_ID_nodes[[5]],c("long","lat")]),1))
names(pl_catch_area5) <- c("long","lat")

pl_polygon5 <- pl_catch_area5 %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
pl_polygon5 <- st_buffer(pl_polygon5, dist = 0)
pl_cases5 <- st_intersection(philip_lee_cases, pl_polygon5)

pl_catch_area6 <- data.frame(concaveman(as.matrix(philip_lee_manholes[selected_sites_ID_nodes[[6]],c("long","lat")]),1))
names(pl_catch_area6) <- c("long","lat")

pl_polygon6 <- pl_catch_area6 %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
pl_polygon6 <- st_buffer(pl_polygon6, dist = 0)
pl_cases6 <- st_intersection(philip_lee_cases, pl_polygon6)

pl_sites <- c("Peyton Woods Trail","Plainville Trail","Benjamin E Mays High School","Chatham Ave","Walmart","Larchwood")
map_PhiLee <- ggplot() + geom_sf(data = philip_lee_polygon, fill="gray") +
  geom_sf(data = pl_polygon1, aes(fill=pl_sites[1])) +
  geom_sf(data = pl_polygon2, aes(fill=pl_sites[2])) +
  geom_sf(data = pl_polygon3, aes(fill=pl_sites[3])) +
  geom_sf(data = pl_polygon4, aes(fill=pl_sites[4])) +
  geom_sf(data = pl_polygon5, aes(fill=pl_sites[5])) +
  geom_sf(data = pl_polygon6, aes(fill=pl_sites[6])) +
  geom_point(data=philip_lee_manholes[which(sub.net.manholes$UNITID %in% selected_sites_ID),],aes(x=long,y=lat),size=4,col="black")+
  geom_path(philip_lee_sewage,mapping = aes(x=long, y=lat,group=group)) +
  scale_fill_manual(values = c("Peyton Woods Trail"="tomato","Plainville Trail"="cyan","Benjamin E Mays High School"="palegreen","Chatham Ave"="hotpink","Walmart"="red2","Larchwood"="gold"),
                    breaks = c("Peyton Woods Trail","Plainville Trail","Benjamin E Mays High School","Chatham Ave","Walmart","Larchwood"),
                    name = "Catchment") +
  guides(fill = guide_legend(override.aes = list(color = "black"))) +
  scale_color_identity(guide = "legend") +
  theme(legend.position="bottom") +
  ggtitle("Phillip Lee")

pl_sites_s <- c("PWT","PT","BEM","Cha","Wal","Lar")

philip_lee_cases$catchment <- NA
philip_lee_cases$catchment[which(philip_lee_cases$QARESPONSEID %in% pl_cases1$QARESPONSEID)] <- pl_sites_s[1]
philip_lee_cases$catchment[which(philip_lee_cases$QARESPONSEID %in% pl_cases2$QARESPONSEID)] <- pl_sites_s[2]
philip_lee_cases$catchment[which(philip_lee_cases$QARESPONSEID %in% pl_cases3$QARESPONSEID)] <- pl_sites_s[3]
philip_lee_cases$catchment[which(philip_lee_cases$QARESPONSEID %in% pl_cases4$QARESPONSEID)] <- pl_sites_s[4]
philip_lee_cases$catchment[which(philip_lee_cases$QARESPONSEID %in% pl_cases5$QARESPONSEID)] <- pl_sites_s[5]
philip_lee_cases$catchment[which(philip_lee_cases$QARESPONSEID %in% pl_cases6$QARESPONSEID)] <- pl_sites_s[6]
philip_lee_cases$catchment <- factor(philip_lee_cases$catchment, levels=pl_sites_s)

label_epi_sites <- c(paste0(pl_sites_s,",n=",table(philip_lee_cases$catchment[which(philip_lee_cases$report_date>=as.Date("09/01/2021",format = "%m/%d/%Y"))])),"Other")

philip_lee_cases$catchment <- NA
philip_lee_cases$catchment[which(philip_lee_cases$QARESPONSEID %in% pl_cases1$QARESPONSEID)] <- label_epi_sites[1]
philip_lee_cases$catchment[which(philip_lee_cases$QARESPONSEID %in% pl_cases2$QARESPONSEID)] <- label_epi_sites[2]
philip_lee_cases$catchment[which(philip_lee_cases$QARESPONSEID %in% pl_cases3$QARESPONSEID)] <- label_epi_sites[3]
philip_lee_cases$catchment[which(philip_lee_cases$QARESPONSEID %in% pl_cases4$QARESPONSEID)] <- label_epi_sites[4]
philip_lee_cases$catchment[which(philip_lee_cases$QARESPONSEID %in% pl_cases5$QARESPONSEID)] <- label_epi_sites[5]
philip_lee_cases$catchment[which(philip_lee_cases$QARESPONSEID %in% pl_cases6$QARESPONSEID)] <- label_epi_sites[6]
philip_lee_cases$catchment <- factor(philip_lee_cases$catchment, levels=label_epi_sites[6:1])

p2_PhiLee <- ggplot(data=dat.3 %>% filter(Collect_date >= '2021-09-01' & Collect_date <= "2022-04-15") 
                    %>% filter(!is.na(location_type)) 
                    %>% filter(location_type %in% c("Community","Influent Line"))
                    %>% filter(location %in% c("Phillip Lee","Peyton Woods Trail","Plainville Trail","Benjamin E Mays High School","Chatham Ave","Walmart","Larchwood"))
                    %>% mutate(location=factor(location,
                                               levels=c("Phillip Lee","Peyton Woods Trail","Plainville Trail","Benjamin E Mays High School","Chatham Ave","Walmart","Larchwood"),
                                               labels=c("Phillip Lee","PWT","PT","BEM","Cha","Wal","Lar"))))+
  geom_point(aes(x=Collect_date, y=location, col=positive),size=3) +
  scale_colour_manual(values=c("Sky Blue","Orange","Red")) + 
  xlim(as.Date("09/01/2021",format = "%m/%d/%Y"),as.Date("05/08/2022",format = "%m/%d/%Y")) +
  theme(legend.position="bottom",axis.text.y=element_text(angle=90,hjust=0.5)) +
  facet_grid(location_type ~ ., scales = "free_y", space='free_y')

epi_curve_PhiLee <- philip_lee_cases %>% filter(report_date>=as.Date("09/01/2021",format = "%m/%d/%Y")) %>%
  ggplot() + geom_bar(aes(report_date), stat="count") + xlim(as.Date("09/01/2021",format = "%m/%d/%Y"),as.Date("05/08/2022",format = "%m/%d/%Y")) +
  facet_grid(catchment ~ ., scales = "free_y")

#grid.arrange(p2_PhiLee,epi_curve_PhiLee,nrow = 2,heights = c(0.25,0.75))
#PhiLee <- grid.arrange(map_PhiLee,p2_PhiLee,epi_curve_PhiLee,nrow = 3,heights = c(0.4,0.15,0.45))
#heatmap
PhiLee.cases <- geocode.cases[which(geocode.cases$QARESPONSEID %in% philip_lee_cases$QARESPONSEID[philip_lee_cases$report_date>=as.Date("09/01/2021",format = "%m/%d/%Y")]),]

heat_PhiLee <- ggplot() + geom_polygon(data = philip_lee_catch_area, aes(x=long, y=lat), colour="black", fill=NA) +
  geom_polygon(data = pl_catch_area1, aes(x=long, y=lat), colour="red2", fill=NA) +
  geom_polygon(data = pl_catch_area2, aes(x=long, y=lat), colour="red2", fill=NA) +
  geom_polygon(data = pl_catch_area3, aes(x=long, y=lat), colour="red2", fill=NA) +
  geom_polygon(data = pl_catch_area4, aes(x=long, y=lat), colour="red2", fill=NA) +
  geom_polygon(data = pl_catch_area5, aes(x=long, y=lat), colour="red2", fill=NA) +
  geom_polygon(data = pl_catch_area6, aes(x=long, y=lat), colour="red2", fill=NA) +
  geom_point(data  = PhiLee.cases, aes(x = RES_LON, y = RES_LAT), color="gold") +
  # stat_density2d(data = PhiLee.cases, aes(x = RES_LON, y = RES_LAT, fill = ..density..), 
  #                geom = 'tile', contour = F, alpha=0.5) + scale_fill_viridis() +
  geom_density_2d_filled(data = PhiLee.cases, aes(x = RES_LON, y = RES_LAT),alpha = 0.5) +
  #geom_density_2d(data = PhiLee.cases, aes(x = RES_LON, y = RES_LAT),size = 0.25, colour = "black") +
  theme(legend.position="bottom") +
  ggtitle("COVID-19 Cases (Sep 1st 2021 to May 8th 2022)")

PhiLee <- grid.arrange(map_PhiLee,p2_PhiLee,heat_PhiLee,epi_curve_PhiLee,nrow = 2,heights = c(0.5,0.5))
ggsave(plot=PhiLee,file="./plots/res_PhiLee_051122.pdf",height = 12,width = 18)

###################################################

###################################################
#South Fulton;
i.site=3

influ.net.node.ind <- which(col.select.sites==temp.col[i.site])
influ.net <- induced_subgraph(net,influ.net.node.ind)
#count how many nodes upstream
influ.cnt.up.nodes <- c()
for (i in 1:length(V(influ.net))){
  print(i)
  influ.cnt.up.nodes[i] <- length(get_upstream_nodes(influ.net,i))
  cat("\014")
}
influ.net.manholes <- data.frame(cbind(V(net)$name[influ.net.node.ind],influ.cnt.up.nodes))
colnames(influ.net.manholes) <- c("ID","cnt")
influ.net.manholes$cnt <- as.numeric(influ.net.manholes$cnt)

south_fulton_manholes <- dat.manhole[which(col.select.sites==temp.col[i.site]),]
sub.net.manholes <- south_fulton_manholes
south_fulton_catch_area <- data.frame(concaveman(as.matrix(south_fulton_manholes[,c("long","lat")]),1))
names(south_fulton_catch_area) <- c("long","lat")

south_fulton_polygon <- south_fulton_catch_area %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
south_fulton_polygon <- st_buffer(south_fulton_polygon, dist = 0)

sp.cases <- st_as_sf(geocode.cases, coords = c("RES_LON","RES_LAT"), crs = 4326)
south_fulton_cases <- st_intersection(sp.cases, south_fulton_polygon)

south_fulton_cases <- merge(south_fulton_cases,pui.data[,c("QARESPONSEID","Q174631","Q174656")],by="QARESPONSEID",all.x=T)
names(south_fulton_cases)[which(names(south_fulton_cases)=="Q174631")] <- "report_date"
names(south_fulton_cases)[which(names(south_fulton_cases)=="Q174656")] <- "symptom_onset"
south_fulton_cases$report_date <- as.Date(south_fulton_cases$report_date,format = "%m/%d/%Y")
south_fulton_cases$symptom_onset <- as.Date(south_fulton_cases$symptom_onset,format = "%m/%d/%Y")

k.site.ID <- V(net)$name[select.sites[i.site]]

sub.net.node.ind <- sort(c(which(V(influ.net)$name==k.site.ID), get_upstream_nodes(influ.net,which(V(influ.net)$name==k.site.ID))))
sub.net <- induced_subgraph(influ.net,sub.net.node.ind)

selected_sites_ID <- c("13940200401","13850103501","13850100701","13950301801","23040112201","23040107701")

selected_sites_ID_nodes <- list()
selected_sites_ID_n_nodes <- c()
for (i in 1:length(selected_sites_ID)){
  selected_sites_ID_nodes[[i]] <- c(which(V(sub.net)$name==selected_sites_ID[i]),get_upstream_nodes(sub.net,which(V(sub.net)$name==selected_sites_ID[i])))
  selected_sites_ID_n_nodes[i] <- length(selected_sites_ID_nodes[[i]])
}

south_fulton_sewage <- sewage_1[which(sewage_1$UNITID %in% south_fulton_manholes$UNITID & sewage_1$UNITID2 %in% south_fulton_manholes$UNITID),]

sf_catch_area1 <- data.frame(concaveman(as.matrix(south_fulton_manholes[selected_sites_ID_nodes[[1]],c("long","lat")]),1))
names(sf_catch_area1) <- c("long","lat")

sf_polygon1 <- sf_catch_area1 %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
sf_polygon1 <- st_buffer(sf_polygon1, dist = 0)
sf_cases1 <- st_intersection(south_fulton_cases, sf_polygon1)

sf_catch_area2 <- data.frame(concaveman(as.matrix(south_fulton_manholes[selected_sites_ID_nodes[[2]],c("long","lat")]),1))
names(sf_catch_area2) <- c("long","lat")

sf_polygon2 <- sf_catch_area2 %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
sf_polygon2 <- st_buffer(sf_polygon2, dist = 0)
sf_cases2 <- st_intersection(south_fulton_cases, sf_polygon2)

sf_catch_area3 <- data.frame(concaveman(as.matrix(south_fulton_manholes[selected_sites_ID_nodes[[3]],c("long","lat")]),1))
names(sf_catch_area3) <- c("long","lat")

sf_polygon3 <- sf_catch_area3 %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
sf_polygon3 <- st_buffer(sf_polygon3, dist = 0)
sf_cases3 <- st_intersection(south_fulton_cases, sf_polygon3)

sf_catch_area4 <- data.frame(concaveman(as.matrix(south_fulton_manholes[selected_sites_ID_nodes[[4]],c("long","lat")]),1))
names(sf_catch_area4) <- c("long","lat")

sf_polygon4 <- sf_catch_area4 %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
sf_polygon4 <- st_buffer(sf_polygon4, dist = 0)
sf_cases4 <- st_intersection(south_fulton_cases, sf_polygon4)

sf_catch_area5 <- data.frame(concaveman(as.matrix(south_fulton_manholes[selected_sites_ID_nodes[[5]],c("long","lat")]),1))
names(sf_catch_area5) <- c("long","lat")

sf_polygon5 <- sf_catch_area5 %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
sf_polygon5 <- st_buffer(sf_polygon5, dist = 0)
sf_cases5 <- st_intersection(south_fulton_cases, sf_polygon5)

sf_catch_area6 <- data.frame(concaveman(as.matrix(south_fulton_manholes[selected_sites_ID_nodes[[6]],c("long","lat")]),1))
names(sf_catch_area6) <- c("long","lat")

sf_polygon6 <- sf_catch_area6 %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
sf_polygon6 <- st_buffer(sf_polygon6, dist = 0)
sf_cases6 <- st_intersection(south_fulton_cases, sf_polygon6)

sf_sites <- c("Mt. Gilead Rd","Guilford Forest Dr","Cascade Rd","Village Dr","Dodson Dr","Ivydale")
map_SouFul <- ggplot() + geom_sf(data = south_fulton_polygon, fill="gray") +
  geom_sf(data = sf_polygon1, aes(fill=sf_sites[1])) +
  geom_sf(data = sf_polygon2, aes(fill=sf_sites[2])) +
  geom_sf(data = sf_polygon3, aes(fill=sf_sites[3])) +
  geom_sf(data = sf_polygon4, aes(fill=sf_sites[4])) +
  geom_sf(data = sf_polygon5, aes(fill=sf_sites[5])) +
  geom_sf(data = sf_polygon6, aes(fill=sf_sites[6])) +
  geom_point(data=south_fulton_manholes[which(sub.net.manholes$UNITID %in% selected_sites_ID),],aes(x=long,y=lat),size=4,col="black")+
  geom_path(south_fulton_sewage,mapping = aes(x=long, y=lat,group=group)) +
  scale_fill_manual(values = c("Mt. Gilead Rd"="tomato","Guilford Forest Dr"="cyan","Cascade Rd"="palegreen","Village Dr"="hotpink","Dodson Dr"="red2","Ivydale"="gold"),
                    breaks = c("Mt. Gilead Rd","Guilford Forest Dr","Cascade Rd","Village Dr","Dodson Dr","Ivydale"),
                    name = "Catchment") +
  guides(fill = guide_legend(override.aes = list(color = "black"))) +
  scale_color_identity(guide = "legend") +
  theme(legend.position="bottom") +
  ggtitle("South Fulton")

sf_sites_s <- c("MtGilR","GuiForD","CasR","VilD","DodD","Ivy")

south_fulton_cases$catchment <- NA
south_fulton_cases$catchment[which(south_fulton_cases$QARESPONSEID %in% sf_cases1$QARESPONSEID)] <- sf_sites_s[1]
south_fulton_cases$catchment[which(south_fulton_cases$QARESPONSEID %in% sf_cases2$QARESPONSEID)] <- sf_sites_s[2]
south_fulton_cases$catchment[which(south_fulton_cases$QARESPONSEID %in% sf_cases3$QARESPONSEID)] <- sf_sites_s[3]
south_fulton_cases$catchment[which(south_fulton_cases$QARESPONSEID %in% sf_cases4$QARESPONSEID)] <- sf_sites_s[4]
south_fulton_cases$catchment[which(south_fulton_cases$QARESPONSEID %in% sf_cases5$QARESPONSEID)] <- sf_sites_s[5]
south_fulton_cases$catchment[which(south_fulton_cases$QARESPONSEID %in% sf_cases6$QARESPONSEID)] <- sf_sites_s[6]
south_fulton_cases$catchment <- factor(south_fulton_cases$catchment, levels=sf_sites_s)

label_epi_sites <- paste0(sf_sites_s,",n=",table(south_fulton_cases$catchment[which(south_fulton_cases$report_date>=as.Date("09/01/2021",format = "%m/%d/%Y"))]))

south_fulton_cases$catchment <- NA
south_fulton_cases$catchment[which(south_fulton_cases$QARESPONSEID %in% sf_cases1$QARESPONSEID)] <- label_epi_sites[1]
south_fulton_cases$catchment[which(south_fulton_cases$QARESPONSEID %in% sf_cases2$QARESPONSEID)] <- label_epi_sites[2]
south_fulton_cases$catchment[which(south_fulton_cases$QARESPONSEID %in% sf_cases3$QARESPONSEID)] <- label_epi_sites[3]
south_fulton_cases$catchment[which(south_fulton_cases$QARESPONSEID %in% sf_cases4$QARESPONSEID)] <- label_epi_sites[4]
south_fulton_cases$catchment[which(south_fulton_cases$QARESPONSEID %in% sf_cases5$QARESPONSEID)] <- label_epi_sites[5]
south_fulton_cases$catchment[which(south_fulton_cases$QARESPONSEID %in% sf_cases6$QARESPONSEID)] <- label_epi_sites[6]
south_fulton_cases$catchment <- factor(south_fulton_cases$catchment, levels=label_epi_sites[6:1])

p2_SouFul <- ggplot(data=dat.3 %>% filter(Collect_date >= '2021-09-01' & Collect_date <= "2022-04-15") 
                    %>% filter(!is.na(location_type)) 
                    %>% filter(location %in% c("South Fulton","Mt. Gilead Rd","Guilford Forest Dr","Cascade Rd","Village Dr","Dodson Dr","Ivydale"))
                    %>% mutate(location=factor(location,
                                               levels=c("South Fulton","Mt. Gilead Rd","Guilford Forest Dr","Cascade Rd","Village Dr","Dodson Dr","Ivydale"),
                                               labels=c("South Fulton","MtGilR","GuiForD","CasR","VilD","DodD","Ivy"))))+
  geom_point(aes(x=Collect_date, y=location, col=positive),size=3) +
  scale_colour_manual(values=c("Sky Blue","Orange","Red")) +
  xlim(as.Date("09/01/2021",format = "%m/%d/%Y"),as.Date("05/08/2022",format = "%m/%d/%Y")) +
  theme(legend.position="bottom",axis.text.y=element_text(angle=90,hjust=0.5)) +
  facet_grid(location_type ~ ., scales = "free_y", space='free_y')

epi_curve_SouFul <- south_fulton_cases %>% filter(report_date>=as.Date("09/01/2021",format = "%m/%d/%Y")) %>%
  ggplot() + geom_bar(aes(report_date), stat="count") + xlim(as.Date("09/01/2021",format = "%m/%d/%Y"),as.Date("05/08/2022",format = "%m/%d/%Y")) +
  facet_grid(catchment ~ ., scales = "free_y")

#grid.arrange(p2_SouFul,epi_curve_SouFul,nrow = 2,heights = c(0.25,0.75))
#SouFul <- grid.arrange(map_SouFul,p2_SouFul,epi_curve_SouFul,nrow = 3,heights = c(0.4,0.15,0.45))
#heatmap
SouFul.cases <- geocode.cases[which(geocode.cases$QARESPONSEID %in% south_fulton_cases$QARESPONSEID[south_fulton_cases$report_date>=as.Date("09/01/2021",format = "%m/%d/%Y")]),]

heat_SouFul <- ggplot() + geom_polygon(data = south_fulton_catch_area, aes(x=long, y=lat), colour="black", fill=NA) +
  geom_polygon(data = sf_catch_area1, aes(x=long, y=lat), colour="red2", fill=NA) +
  geom_polygon(data = sf_catch_area2, aes(x=long, y=lat), colour="red2", fill=NA) +
  geom_polygon(data = sf_catch_area3, aes(x=long, y=lat), colour="red2", fill=NA) +
  geom_polygon(data = sf_catch_area4, aes(x=long, y=lat), colour="red2", fill=NA) +
  geom_polygon(data = sf_catch_area5, aes(x=long, y=lat), colour="red2", fill=NA) +
  geom_polygon(data = sf_catch_area6, aes(x=long, y=lat), colour="red2", fill=NA) +
  geom_point(data  = SouFul.cases, aes(x = RES_LON, y = RES_LAT), color="gold") +
  # stat_density2d(data = SouFul.cases, aes(x = RES_LON, y = RES_LAT, fill = ..density..), 
  #                geom = 'tile', contour = F, alpha=0.5) + scale_fill_viridis() +
  geom_density_2d_filled(data = SouFul.cases, aes(x = RES_LON, y = RES_LAT),alpha = 0.5) +
  #geom_density_2d(data = SouFul.cases, aes(x = RES_LON, y = RES_LAT),size = 0.25, colour = "black") +
  theme(legend.position="bottom") +
  ggtitle("COVID-19 Cases (Sep 1st 2021 to May 8th 2022)")

SouFul <- grid.arrange(map_SouFul,p2_SouFul,heat_SouFul,epi_curve_SouFul,nrow = 2,heights = c(0.5,0.5))
ggsave(plot=SouFul,file="./plots/res_SouFul_051122.pdf",height = 14,width = 18)

###################################################

###################################################
#Jonesboro;
i.site=4

influ.net.node.ind <- which(col.select.sites==temp.col[i.site])
influ.net <- induced_subgraph(net,influ.net.node.ind)
#count how many nodes upstream
influ.cnt.up.nodes <- c()
for (i in 1:length(V(influ.net))){
  print(i)
  influ.cnt.up.nodes[i] <- length(get_upstream_nodes(influ.net,i))
  cat("\014")
}
influ.net.manholes <- data.frame(cbind(V(net)$name[influ.net.node.ind],influ.cnt.up.nodes))
colnames(influ.net.manholes) <- c("ID","cnt")
influ.net.manholes$cnt <- as.numeric(influ.net.manholes$cnt)

jonesboro_manholes <- dat.manhole[which(col.select.sites==temp.col[i.site]),]
sub.net.manholes <- jonesboro_manholes
jonesboro_catch_area <- data.frame(concaveman(as.matrix(jonesboro_manholes[,c("long","lat")]),1))
names(jonesboro_catch_area) <- c("long","lat")

jonesboro_polygon <- jonesboro_catch_area %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
jonesboro_polygon <- st_buffer(jonesboro_polygon, dist = 0)

sp.cases <- st_as_sf(geocode.cases, coords = c("RES_LON","RES_LAT"), crs = 4326)
jonesboro_cases <- st_intersection(sp.cases, jonesboro_polygon)

jonesboro_cases <- merge(jonesboro_cases,pui.data[,c("QARESPONSEID","Q174631","Q174656")],by="QARESPONSEID",all.x=T)
names(jonesboro_cases)[which(names(jonesboro_cases)=="Q174631")] <- "report_date"
names(jonesboro_cases)[which(names(jonesboro_cases)=="Q174656")] <- "symptom_onset"
jonesboro_cases$report_date <- as.Date(jonesboro_cases$report_date,format = "%m/%d/%Y")
jonesboro_cases$symptom_onset <- as.Date(jonesboro_cases$symptom_onset,format = "%m/%d/%Y")

k.site.ID <- V(net)$name[select.sites[i.site]]

sub.net.node.ind <- sort(c(which(V(influ.net)$name==k.site.ID), get_upstream_nodes(influ.net,which(V(influ.net)$name==k.site.ID))))
sub.net <- induced_subgraph(influ.net,sub.net.node.ind)

selected_sites_ID <- c("23330214201","23320205701","23330200801","23330305601","23320107101")

selected_sites_ID_nodes <- list()
selected_sites_ID_n_nodes <- c()
for (i in 1:length(selected_sites_ID)){
  selected_sites_ID_nodes[[i]] <- c(which(V(sub.net)$name==selected_sites_ID[i]),get_upstream_nodes(sub.net,which(V(sub.net)$name==selected_sites_ID[i])))
  selected_sites_ID_n_nodes[i] <- length(selected_sites_ID_nodes[[i]])
}

jonesboro_sewage <- sewage_1[which(sewage_1$UNITID %in% jonesboro_manholes$UNITID & sewage_1$UNITID2 %in% jonesboro_manholes$UNITID),]

jb_catch_area1 <- data.frame(concaveman(as.matrix(jonesboro_manholes[selected_sites_ID_nodes[[1]],c("long","lat")]),1))
names(jb_catch_area1) <- c("long","lat")

jb_polygon1 <- jb_catch_area1 %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
jb_polygon1 <- st_buffer(jb_polygon1, dist = 0)
jb_cases1 <- st_intersection(jonesboro_cases, jb_polygon1)

jb_catch_area2 <- data.frame(concaveman(as.matrix(jonesboro_manholes[selected_sites_ID_nodes[[2]],c("long","lat")]),1))
names(jb_catch_area2) <- c("long","lat")

jb_polygon2 <- jb_catch_area2 %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
jb_polygon2 <- st_buffer(jb_polygon2, dist = 0)
jb_cases2 <- st_intersection(jonesboro_cases, jb_polygon2)

jb_catch_area3 <- data.frame(concaveman(as.matrix(jonesboro_manholes[selected_sites_ID_nodes[[3]],c("long","lat")]),1))
names(jb_catch_area3) <- c("long","lat")

jb_polygon3 <- jb_catch_area3 %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
jb_polygon3 <- st_buffer(jb_polygon3, dist = 0)
jb_cases3 <- st_intersection(jonesboro_cases, jb_polygon3)

jb_catch_area4 <- data.frame(concaveman(as.matrix(jonesboro_manholes[selected_sites_ID_nodes[[4]],c("long","lat")]),1))
names(jb_catch_area4) <- c("long","lat")

jb_polygon4 <- jb_catch_area4 %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
jb_polygon4 <- st_buffer(jb_polygon4, dist = 0)
jb_cases4 <- st_intersection(jonesboro_cases, jb_polygon4)

jb_catch_area5 <- data.frame(concaveman(as.matrix(jonesboro_manholes[selected_sites_ID_nodes[[5]],c("long","lat")]),1))
names(jb_catch_area5) <- c("long","lat")

jb_polygon5 <- jb_catch_area5 %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
jb_polygon5 <- st_buffer(jb_polygon5, dist = 0)
jb_cases5 <- st_intersection(jonesboro_cases, jb_polygon5)

jb_sites <- c("Forest Park Rd -- North Site","Forest Park Rd -- South Site","South Atlanta High School 2","Southside Industrial Parkway","Ruby Harper Blvd")
map_Jone <- ggplot() + geom_sf(data = jonesboro_polygon, fill="gray") +
  geom_sf(data = jb_polygon1, aes(fill=jb_sites[1])) +
  geom_sf(data = jb_polygon2, aes(fill=jb_sites[2])) +
  geom_sf(data = jb_polygon3, aes(fill=jb_sites[3])) +
  geom_sf(data = jb_polygon4, aes(fill=jb_sites[4])) +
  geom_sf(data = jb_polygon5, aes(fill=jb_sites[5])) +
  geom_point(data=jonesboro_manholes[which(sub.net.manholes$UNITID %in% selected_sites_ID),],aes(x=long,y=lat),size=4,col="black")+
  geom_path(jonesboro_sewage,mapping = aes(x=long, y=lat,group=group)) +
  scale_fill_manual(values = c("Forest Park Rd -- North Site"="tomato","Forest Park Rd -- South Site"="cyan","South Atlanta High School 2"="palegreen","Southside Industrial Parkway"="hotpink","Ruby Harper Blvd"="red2"),
                    breaks = c("Forest Park Rd -- North Site","Forest Park Rd -- South Site","South Atlanta High School 2","Southside Industrial Parkway","Ruby Harper Blvd"),
                    name = "Catchment") +
  guides(fill = guide_legend(override.aes = list(color = "black"))) +
  scale_color_identity(guide = "legend") +
  theme(legend.position="bottom") +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+
  ggtitle("Jonesboro")

jb_sites_s <- c("FPR-N","FPR-S","SA-HS","SouIndP","RubHarB")

jonesboro_cases$catchment <- NA
jonesboro_cases$catchment[which(jonesboro_cases$QARESPONSEID %in% jb_cases1$QARESPONSEID)] <- jb_sites_s[1]
jonesboro_cases$catchment[which(jonesboro_cases$QARESPONSEID %in% jb_cases2$QARESPONSEID)] <- jb_sites_s[2]
jonesboro_cases$catchment[which(jonesboro_cases$QARESPONSEID %in% jb_cases3$QARESPONSEID)] <- jb_sites_s[3]
jonesboro_cases$catchment[which(jonesboro_cases$QARESPONSEID %in% jb_cases4$QARESPONSEID)] <- jb_sites_s[4]
jonesboro_cases$catchment[which(jonesboro_cases$QARESPONSEID %in% jb_cases5$QARESPONSEID)] <- jb_sites_s[5]
jonesboro_cases$catchment <- factor(jonesboro_cases$catchment, levels=jb_sites_s)

label_epi_sites <- paste0(jb_sites_s,",n=",table(jonesboro_cases$catchment[which(jonesboro_cases$report_date>=as.Date("09/01/2021",format = "%m/%d/%Y"))]))

jonesboro_cases$catchment <- NA
jonesboro_cases$catchment[which(jonesboro_cases$QARESPONSEID %in% jb_cases1$QARESPONSEID)] <- label_epi_sites[1]
jonesboro_cases$catchment[which(jonesboro_cases$QARESPONSEID %in% jb_cases2$QARESPONSEID)] <- label_epi_sites[2]
jonesboro_cases$catchment[which(jonesboro_cases$QARESPONSEID %in% jb_cases3$QARESPONSEID)] <- label_epi_sites[3]
jonesboro_cases$catchment[which(jonesboro_cases$QARESPONSEID %in% jb_cases4$QARESPONSEID)] <- label_epi_sites[4]
jonesboro_cases$catchment[which(jonesboro_cases$QARESPONSEID %in% jb_cases5$QARESPONSEID)] <- label_epi_sites[5]
jonesboro_cases$catchment <- factor(jonesboro_cases$catchment, levels=label_epi_sites[5:1])

p2_Jone <- ggplot(data=dat.3 %>% filter(Collect_date >= '2021-09-01' & Collect_date <= "2022-04-15") 
                    %>% filter(!is.na(location_type)) %>% filter(location_type!="School")
                    %>% filter(location %in% c("Jonesboro","Forest Park Rd -- North Site","Forest Park Rd -- South Site","South Atlanta High School 2","Southside Industrial Parkway","Ruby Harper Blvd"))
                    %>% mutate(location=factor(location,
                                               levels=c("Jonesboro","Forest Park Rd -- North Site","Forest Park Rd -- South Site","South Atlanta High School 2","Southside Industrial Parkway","Ruby Harper Blvd"),
                                               labels=c("Jonesboro","FPR-N","FPR-S","SA-HS","SouIndP","RubHarB"))))+
  geom_point(aes(x=Collect_date, y=location, col=positive),size=3) +
  scale_colour_manual(values=c("Sky Blue","Orange","Red")) +
  xlim(as.Date("09/01/2021",format = "%m/%d/%Y"),as.Date("05/08/2022",format = "%m/%d/%Y")) +
  theme(legend.position="bottom",axis.text.y=element_text(angle=90,hjust=0.5)) +
  facet_grid(location_type ~ ., scales = "free_y", space='free_y')

epi_curve_Jone <- jonesboro_cases %>% filter(report_date>=as.Date("09/01/2021",format = "%m/%d/%Y")) %>%
  ggplot() + geom_bar(aes(report_date), stat="count",binwidth=1) + xlim(as.Date("09/01/2021",format = "%m/%d/%Y"),as.Date("05/08/2022",format = "%m/%d/%Y")) +
  facet_grid(catchment ~ ., scales = "free_y")

#grid.arrange(p2_Jone,epi_curve_Jone,nrow = 2,heights = c(0.25,0.75))
#Jone <- grid.arrange(map_Jone,p2_Jone,epi_curve_Jone,nrow = 3,heights = c(0.4,0.15,0.45))
#heatmap
Jone.cases <- geocode.cases[which(geocode.cases$QARESPONSEID %in% jonesboro_cases$QARESPONSEID[jonesboro_cases$report_date>=as.Date("09/01/2021",format = "%m/%d/%Y")]),]

heat_Jone <- ggplot() + geom_polygon(data = jonesboro_catch_area, aes(x=long, y=lat), colour="black", fill=NA) +
  geom_polygon(data = jb_catch_area1, aes(x=long, y=lat), colour="red2", fill=NA) +
  geom_polygon(data = jb_catch_area2, aes(x=long, y=lat), colour="red2", fill=NA) +
  geom_polygon(data = jb_catch_area3, aes(x=long, y=lat), colour="red2", fill=NA) +
  geom_polygon(data = jb_catch_area4, aes(x=long, y=lat), colour="red2", fill=NA) +
  geom_polygon(data = jb_catch_area5, aes(x=long, y=lat), colour="red2", fill=NA) +
  geom_point(data  = Jone.cases, aes(x = RES_LON, y = RES_LAT), color="gold") +
  # stat_density2d(data = Jone.cases, aes(x = RES_LON, y = RES_LAT, fill = ..density..), 
  #                geom = 'tile', contour = F, alpha=0.5) + scale_fill_viridis() +
  geom_density_2d_filled(data = Jone.cases, aes(x = RES_LON, y = RES_LAT),alpha = 0.5) +
  #geom_density_2d(data = Jone.cases, aes(x = RES_LON, y = RES_LAT),size = 0.25, colour = "black") +
  theme(legend.position="bottom") +
  ggtitle("COVID-19 Cases (Sep 1st 2021 to May 8th 2022)")

Jone <- grid.arrange(map_Jone,p2_Jone,heat_Jone,epi_curve_Jone,nrow = 2,heights = c(0.5,0.5))
ggsave(plot=Jone,file="./plots/res_Jone_051122.pdf",height = 12,width = 18)

###################################################

###################################################
#Intrenchment;
i.site=6

influ.net.node.ind <- which(col.select.sites==temp.col[i.site])
influ.net <- induced_subgraph(net,influ.net.node.ind)
#count how many nodes upstream
influ.cnt.up.nodes <- c()
for (i in 1:length(V(influ.net))){
  print(i)
  influ.cnt.up.nodes[i] <- length(get_upstream_nodes(influ.net,i))
  cat("\014")
}
influ.net.manholes <- data.frame(cbind(V(net)$name[influ.net.node.ind],influ.cnt.up.nodes))
colnames(influ.net.manholes) <- c("ID","cnt")
influ.net.manholes$cnt <- as.numeric(influ.net.manholes$cnt)

intrenchment_manholes <- dat.manhole[which(col.select.sites==temp.col[i.site]),]
sub.net.manholes <- intrenchment_manholes
intrenchment_catch_area <- data.frame(concaveman(as.matrix(intrenchment_manholes[,c("long","lat")]),1))
names(intrenchment_catch_area) <- c("long","lat")

intrenchment_polygon <- intrenchment_catch_area %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
intrenchment_polygon <- st_buffer(intrenchment_polygon, dist = 0)

sp.cases <- st_as_sf(geocode.cases, coords = c("RES_LON","RES_LAT"), crs = 4326)
intrenchment_cases <- st_intersection(sp.cases, intrenchment_polygon)

intrenchment_cases <- merge(intrenchment_cases,pui.data[,c("QARESPONSEID","Q174631","Q174656")],by="QARESPONSEID",all.x=T)
names(intrenchment_cases)[which(names(intrenchment_cases)=="Q174631")] <- "report_date"
names(intrenchment_cases)[which(names(intrenchment_cases)=="Q174656")] <- "symptom_onset"
intrenchment_cases$report_date <- as.Date(intrenchment_cases$report_date,format = "%m/%d/%Y")
intrenchment_cases$symptom_onset <- as.Date(intrenchment_cases$symptom_onset,format = "%m/%d/%Y")

k.site.ID <- V(net)$name[select.sites[i.site]]

sub.net.node.ind <- sort(c(which(V(influ.net)$name==k.site.ID), get_upstream_nodes(influ.net,which(V(influ.net)$name==k.site.ID))))
sub.net <- induced_subgraph(influ.net,sub.net.node.ind)

selected_sites_ID <- c("23550100401","23560310401","23460337501")

selected_sites_ID_nodes <- list()
selected_sites_ID_n_nodes <- c()
for (i in 1:length(selected_sites_ID)){
  selected_sites_ID_nodes[[i]] <- c(which(V(sub.net)$name==selected_sites_ID[i]),get_upstream_nodes(sub.net,which(V(sub.net)$name==selected_sites_ID[i])))
  selected_sites_ID_n_nodes[i] <- length(selected_sites_ID_nodes[[i]])
}

intrenchment_sewage <- sewage_1[which(sewage_1$UNITID %in% intrenchment_manholes$UNITID & sewage_1$UNITID2 %in% intrenchment_manholes$UNITID),]

intr_catch_area1 <- data.frame(concaveman(as.matrix(intrenchment_manholes[selected_sites_ID_nodes[[1]],c("long","lat")]),1))
names(intr_catch_area1) <- c("long","lat")

intr_polygon1 <- intr_catch_area1 %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
intr_polygon1 <- st_buffer(intr_polygon1, dist = 0)
intr_cases1 <- st_intersection(intrenchment_cases, intr_polygon1)

intr_catch_area2 <- data.frame(concaveman(as.matrix(intrenchment_manholes[selected_sites_ID_nodes[[2]],c("long","lat")]),1))
names(intr_catch_area2) <- c("long","lat")

intr_polygon2 <- intr_catch_area2 %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
intr_polygon2 <- st_buffer(intr_polygon2, dist = 0)
intr_cases2 <- st_intersection(intrenchment_cases, intr_polygon2)

intr_catch_area3 <- data.frame(concaveman(as.matrix(intrenchment_manholes[selected_sites_ID_nodes[[3]],c("long","lat")]),1))
names(intr_catch_area3) <- c("long","lat")

intr_polygon3 <- intr_catch_area3 %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
intr_polygon3 <- st_buffer(intr_polygon3, dist = 0)
intr_cases3 <- st_intersection(intrenchment_cases, intr_polygon3)

# intr_catch_area2 <- data.frame(concaveman(as.matrix(intrenchment_manholes[selected_sites_ID_nodes[[2]],c("long","lat")]),1))
# names(intr_catch_area2) <- c("long","lat")
# 
# intr_polygon2 <- intr_catch_area2 %>%
#   st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
#   summarise(geometry = st_combine(geometry)) %>%
#   st_cast("POLYGON")
# intr_polygon2 <- st_buffer(intr_polygon2, dist = 0)
# intr_cases2 <- st_intersection(intrenchment_cases, intr_polygon2)
# 
# intr_catch_area3 <- data.frame(concaveman(as.matrix(intrenchment_manholes[selected_sites_ID_nodes[[3]],c("long","lat")]),1))
# names(intr_catch_area3) <- c("long","lat")
# 
# intr_polygon3 <- intr_catch_area3 %>%
#   st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
#   summarise(geometry = st_combine(geometry)) %>%
#   st_cast("POLYGON")
# intr_polygon3 <- st_buffer(intr_polygon3, dist = 0)
# intr_cases3 <- st_intersection(intrenchment_cases, intr_polygon3)
# 
# intr_catch_area4 <- data.frame(concaveman(as.matrix(intrenchment_manholes[selected_sites_ID_nodes[[4]],c("long","lat")]),1))
# names(intr_catch_area4) <- c("long","lat")
# 
# intr_polygon4 <- intr_catch_area4 %>%
#   st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
#   summarise(geometry = st_combine(geometry)) %>%
#   st_cast("POLYGON")
# intr_polygon4 <- st_buffer(intr_polygon4, dist = 0)
# intr_cases4 <- st_intersection(intrenchment_cases, intr_polygon4)
# 
# intr_catch_area5 <- data.frame(concaveman(as.matrix(intrenchment_manholes[selected_sites_ID_nodes[[5]],c("long","lat")]),1))
# names(intr_catch_area5) <- c("long","lat")
# 
# intr_polygon5 <- intr_catch_area5 %>%
#   st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
#   summarise(geometry = st_combine(geometry)) %>%
#   st_cast("POLYGON")
# intr_polygon5 <- st_buffer(intr_polygon5, dist = 0)
# intr_cases5 <- st_intersection(intrenchment_cases, intr_polygon5)

intr_sites <- c("Lilac Lane","Oakview Rd","Montgomery St and Woodbine Ave")
map_Intr <- ggplot() + geom_sf(data = intrenchment_polygon, fill="gray") +
  geom_sf(data = intr_polygon1, aes(fill=intr_sites[1])) +
  geom_sf(data = intr_polygon2, aes(fill=intr_sites[2])) +
  geom_sf(data = intr_polygon3, aes(fill=intr_sites[3])) +
  # geom_sf(data = intr_polygon4, aes(fill=intr_sites[4])) +
  # geom_sf(data = intr_polygon5, aes(fill=intr_sites[5])) +
  geom_point(data=intrenchment_manholes[which(sub.net.manholes$UNITID %in% selected_sites_ID),],aes(x=long,y=lat),size=4,col="black")+
  geom_path(intrenchment_sewage,mapping = aes(x=long, y=lat,group=group)) +
  scale_fill_manual(values = c("Lilac Lane"="tomato","Oakview Rd"="cyan","Montgomery St and Woodbine Ave"="palegreen"),
                    breaks = c("Lilac Lane","Oakview Rd","Montgomery St and Woodbine Ave"),
                    name = "Catchment") +
  guides(fill = guide_legend(override.aes = list(color = "black"))) +
  scale_color_identity(guide = "legend") +
  theme(legend.position="bottom") +
  ggtitle("Intrenchment")

intr_sites_s <- c("LilL","OakR","MSWA")

intrenchment_cases$catchment <- NA
intrenchment_cases$catchment[which(intrenchment_cases$QARESPONSEID %in% intr_cases1$QARESPONSEID)] <- intr_sites_s[1]
intrenchment_cases$catchment[which(intrenchment_cases$QARESPONSEID %in% intr_cases2$QARESPONSEID)] <- intr_sites_s[2]
intrenchment_cases$catchment[which(intrenchment_cases$QARESPONSEID %in% intr_cases3$QARESPONSEID)] <- intr_sites_s[3]
# intrenchment_cases$catchment[which(intrenchment_cases$QARESPONSEID %in% intr_cases4$QARESPONSEID)] <- intr_sites_s[4]
# intrenchment_cases$catchment[which(intrenchment_cases$QARESPONSEID %in% intr_cases5$QARESPONSEID)] <- intr_sites_s[5]
intrenchment_cases$catchment <- factor(intrenchment_cases$catchment, levels=intr_sites_s)

label_epi_sites <- paste0(intr_sites_s,",n=",table(intrenchment_cases$catchment[which(intrenchment_cases$report_date>=as.Date("09/01/2021",format = "%m/%d/%Y"))]))

intrenchment_cases$catchment <- NA
intrenchment_cases$catchment[which(intrenchment_cases$QARESPONSEID %in% intr_cases1$QARESPONSEID)] <- label_epi_sites[1]
intrenchment_cases$catchment[which(intrenchment_cases$QARESPONSEID %in% intr_cases2$QARESPONSEID)] <- label_epi_sites[2]
intrenchment_cases$catchment[which(intrenchment_cases$QARESPONSEID %in% intr_cases3$QARESPONSEID)] <- label_epi_sites[3]
# intrenchment_cases$catchment[which(intrenchment_cases$QARESPONSEID %in% intr_cases4$QARESPONSEID)] <- label_epi_sites[4]
# intrenchment_cases$catchment[which(intrenchment_cases$QARESPONSEID %in% intr_cases5$QARESPONSEID)] <- label_epi_sites[5]
# intrenchment_cases$catchment <- factor(intrenchment_cases$catchment, levels=label_epi_sites[5:1])
intrenchment_cases$catchment <- factor(intrenchment_cases$catchment, levels=label_epi_sites[3:1])

p2_Intr <- ggplot(data=dat.3 %>% filter(Collect_date >= '2021-09-01' & Collect_date <= "2022-04-15") 
                    %>% filter(!is.na(location_type)) 
                    %>% filter(location %in% c("Intrenchment","Lilac Lane","Oakview Rd","Montgomery St and Woodbine Ave"))
                    %>% mutate(location=factor(location,
                                               levels=c("Intrenchment","Lilac Lane","Oakview Rd","Montgomery St and Woodbine Ave"),
                                               labels=c("Intrenchment","LilL","OakR","MSWA"))))+
  geom_point(aes(x=Collect_date, y=location, col=positive),size=3) +
  scale_colour_manual(values=c("Sky Blue","Orange","Red")) +
  xlim(as.Date("09/01/2021",format = "%m/%d/%Y"),as.Date("05/08/2022",format = "%m/%d/%Y")) +
  theme(legend.position="bottom",axis.text.y=element_text(angle=90,hjust=0.5)) +
  facet_grid(location_type ~ ., scales = "free_y", space='free_y')

epi_curve_Intr <- intrenchment_cases %>% filter(report_date>=as.Date("09/01/2021",format = "%m/%d/%Y")) %>%
  ggplot() + geom_bar(aes(report_date), stat="count") + xlim(as.Date("09/01/2021",format = "%m/%d/%Y"),as.Date("05/08/2022",format = "%m/%d/%Y")) +
  facet_grid(catchment ~ ., scales = "free_y")

#grid.arrange(p2_Intr,epi_curve_Intr,nrow = 2,heights = c(0.25,0.75))
#Intr <- grid.arrange(map_Intr,p2_Intr,epi_curve_Intr,nrow = 3,heights = c(0.4,0.15,0.45))
#heatmap
Intr.cases <- geocode.cases[which(geocode.cases$QARESPONSEID %in% intrenchment_cases$QARESPONSEID[intrenchment_cases$report_date>=as.Date("09/01/2021",format = "%m/%d/%Y")]),]

heat_Intr <- ggplot() + geom_polygon(data = intrenchment_catch_area, aes(x=long, y=lat), colour="black", fill=NA) +
  geom_polygon(data = intr_catch_area1, aes(x=long, y=lat), colour="red2", fill=NA) +
  geom_polygon(data = intr_catch_area2, aes(x=long, y=lat), colour="red2", fill=NA) +
  geom_polygon(data = intr_catch_area3, aes(x=long, y=lat), colour="red2", fill=NA) +
  # geom_polygon(data = intr_catch_area4, aes(x=long, y=lat), colour="red2", fill=NA) +
  # geom_polygon(data = intr_catch_area5, aes(x=long, y=lat), colour="red2", fill=NA) +
  geom_point(data  = Intr.cases, aes(x = RES_LON, y = RES_LAT), color="gold") +
  # stat_density2d(data = Intr.cases, aes(x = RES_LON, y = RES_LAT, fill = ..density..), 
  #                geom = 'tile', contour = F, alpha=0.5) + scale_fill_viridis() +
  geom_density_2d_filled(data = Intr.cases, aes(x = RES_LON, y = RES_LAT),alpha = 0.5) +
  #geom_density_2d(data = Intr.cases, aes(x = RES_LON, y = RES_LAT),size = 0.25, colour = "black") +
  theme(legend.position="bottom") +
  ggtitle("COVID-19 Cases (Sep 1st 2021 to May 8th 2022)")

Intr <- grid.arrange(map_Intr,p2_Intr,heat_Intr,epi_curve_Intr,nrow = 2,heights = c(0.5,0.5))
ggsave(plot=Intr,file="./plots/res_Intr_051122.pdf",height = 12,width = 18)

###################################################
