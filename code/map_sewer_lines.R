library(raster)
library(rgdal)
library(rgeos)
library(ggplot2)
library(ggmap)
library(geosphere)
library(sf)

#sewage
sewage1 <- readOGR("./Shapefiles/","CampCreek_SewerMain")
sewage2 <- readOGR("./Shapefiles/","IntrenchmentCreek_SewerMain")
sewage3 <- readOGR("./Shapefiles/","LongIslandCreek_SewerMain")
sewage4 <- readOGR("./Shapefiles/","NancyCreek_SewerMain")
sewage5 <- readOGR("./Shapefiles/","PeachtreeCreek_SewerMain")
sewage6 <- readOGR("./Shapefiles/","ProctorCreek_SewerMain")
sewage7 <- readOGR("./Shapefiles/","SandyCreek_SewerMain")
sewage8 <- readOGR("./Shapefiles/","SouthRiver_SewerMain")
sewage9 <- readOGR("./Shapefiles/","SugarCreek_SewerMain")
sewage10 <- readOGR("./Shapefiles/","Utoy_Creek_Sewer_Mains")

sewage0 <- gUnion(gUnion(gUnion(gUnion(gUnion(gUnion(gUnion(gUnion(gUnion(gUnion(sewage1,sewage2),sewage3),sewage4),sewage5),sewage5),sewage6),sewage7),sewage8),sewage9),sewage10)

sewage_0 <- spTransform(sewage0, CRS("+proj=longlat +datum=WGS84"))
sewage_1 <- spTransform(sewage1, CRS("+proj=longlat +datum=WGS84"))
sewage_2 <- spTransform(sewage2, CRS("+proj=longlat +datum=WGS84"))
sewage_3 <- spTransform(sewage3, CRS("+proj=longlat +datum=WGS84"))
sewage_4 <- spTransform(sewage4, CRS("+proj=longlat +datum=WGS84"))
sewage_5 <- spTransform(sewage5, CRS("+proj=longlat +datum=WGS84"))
sewage_6 <- spTransform(sewage6, CRS("+proj=longlat +datum=WGS84"))
sewage_7 <- spTransform(sewage7, CRS("+proj=longlat +datum=WGS84"))
sewage_8 <- spTransform(sewage8, CRS("+proj=longlat +datum=WGS84"))
sewage_9 <- spTransform(sewage9, CRS("+proj=longlat +datum=WGS84"))
sewage_10 <- spTransform(sewage10, CRS("+proj=longlat +datum=WGS84"))

sewage <- list(sewage_1,sewage_2,sewage_3,sewage_4,sewage_5,sewage_6,sewage_7,sewage_8,sewage_9,sewage_10)

lat.range <- c()
long.range <- c()

for (i in 1:10){
  long.range <- c(long.range,sewage[[i]]@bbox[1,])
  lat.range <- c(lat.range,sewage[[i]]@bbox[2,])
}
range(lat.range)
range(long.range)

plot(sewage_1,col=1,xlim=range(long.range),ylim=range(lat.range))
for (i in 1:10){
  if (i==1){
    plot(sewage[[i]],col=i,xlim=range(long.range),ylim=range(lat.range))
  } else {
    lines(sewage[[i]],col=i)
  }
}

#manipulate the lines;
for (i in 1:10){
  if (i==1){
    tmp <- sewage[[i]]@data[,c("UNITID","FROMMH","UPSXCOORD","UPSYCOORD","UPSZCOORD")]
  } else {
    tmp <- rbind(tmp,sewage[[i]]@data[,c("UNITID","FROMMH","UPSXCOORD","UPSYCOORD","UPSZCOORD")])
  }
}
dat.manhole <- tmp[-which(duplicated(tmp)),]
manhole.gps <- dat.manhole[,c("UPSXCOORD","UPSYCOORD")]
coordinates(manhole.gps) <- 1:2
crs(manhole.gps) <- CRS("+proj=tmerc +lat_0=30 +lon_0=-84.1666666666667 +k=0.9999 +x_0=700000 +y_0=0
                        +datum=NAD83 +units=us-ft +no_defs")
manhole.gps<-spTransform(manhole.gps, crs(sewage_1))
dat.manhole$long <- manhole.gps@coords[,1]
dat.manhole$lat <- manhole.gps@coords[,2]
dat.manhole <- dat.manhole[-which(dat.manhole$long < -84.56 | dat.manhole$long > -84.28 | dat.manhole$lat < 33.60 | dat.manhole$lat > 33.90),]

sewage_sld <- SpatialLinesDataFrame(sewage_0, data = data.frame(ID = 1))

SewerMap <- ggplot(sewage_ft, aes(x=long,y=lat, group=group)) + geom_line() +
  xlab("Longitude (Degrees)") + ylab("Latitude (Degrees)") +
  geom_point(data = dat.manhole,
             aes(x = long, y = lat, group=NULL), colour = "springgreen", size = 0.1)
# SewerMap

#network
library(igraph)
for (i in 1:10){
  if (i==1){
    tmp <- sewage[[i]]@data[,c("FROMMH","TOMH")]
  } else {
    tmp <- rbind(tmp,sewage[[i]]@data[,c("FROMMH","TOMH")])
  }
}
names(tmp) <- c("from","to")
links <- tmp
links <- links[-which(is.na(links$from) | is.na(links$to) | (links$from==links$to)),]
unique.nodes <- data.frame(FROMMH=unique(c(links$from,links$to)))
nodes <- merge(unique.nodes,dat.manhole[,c("FROMMH","UPSXCOORD","UPSYCOORD")],by="FROMMH",all.x=T)
names(nodes) <- c("UID","x","y")
nodes <- nodes[-which(is.na(nodes$x) | is.na(nodes$y)),]
nodes <- nodes[-which(duplicated(nodes[,1])),]
#nodes <- nodes[complete.cases(nodes),]
links <- links[which(links$from %in% nodes$UID & links$to %in% nodes$UID),]
#rev.links <- links
#rev.links$from <- links$to
#rev.links$to <- links$from
net <- graph_from_data_frame(links,vertices = nodes[,1],directed = T)
net <- simplify(net)
#rev.net <- graph_from_data_frame(rev.links,vertices = nodes[,1],directed = T)
# pdf(file="./plots/SewageMap.pdf",height=40,width=40)
# plot(net,vertex.size=0.2, edge.arrow.size=0.2, vertex.label=NA,layout=as.matrix(nodes[,2:3]))
# dev.off()

source("./code/lucy-master/R/misc-utilities.R")
source("./code/lucy-master/R/graph-plots.R")
source("./code/lucy-master/R/graph-sim.R")
source("./code/lucy-master/R/lucy.R")
source("./code/lucy-master/R/propagation.R")

# col.nodes <- rep("black",length(V(net)))
# col.nodes[c(2967, get_upstream_nodes(net,2967))] <- "red"
# size.nodes <- rep(0.2,length(V(net)))
# size.nodes[2967] <- 1
# 
# pdf(file="./plots/SewageMap_color.pdf",height=40,width=40)
# plot(net,vertex.size=size.nodes, edge.arrow.size=0.2, vertex.label=NA,vertex.color=col.nodes,vertex.frame.color=col.nodes,layout=as.matrix(nodes[,2:3]))
# dev.off()

library("viridis")
sites.dat <- read.csv("./data/AtlantaSamples.csv")
sites.dat.ID <- sites.dat[-which(sites.dat$Manhole.Number==""),]
temp.col <- viridis(length(sites.dat.ID$NumID))

id.nodes <- rep(NA,length(sites.dat.ID$Manhole.Number))
for (i in 1:length(sites.dat.ID$Manhole.Number)){
  if (length(which(V(net)$name==sites.dat.ID$Manhole.Number[i])>0)){
    id.nodes[i] <- which(V(net)$name==sites.dat.ID$Manhole.Number[i])
  }
}
label.nodes <- rep(NA,length(V(net)))
label.nodes[id.nodes[which(!is.na(id.nodes))]] <- sites.dat.ID$Full.Name[which(!is.na(id.nodes))]

col.nodes <- rep("black",length(V(net)))

for (i in 1:length(sites.dat.ID$NumID)){
  col.nodes[c(which(V(net)$name==sites.dat.ID$Manhole.Number[i]), get_upstream_nodes(net,which(V(net)$name==sites.dat.ID$Manhole.Number[i])))] <- temp.col[i]
}

size.nodes <- rep(0.2,length(V(net)))
size.nodes[which(V(net)$name %in% sites.dat.ID$Manhole.Number)] <- 1

# pdf(file="./plots/SewageMap_sites.pdf",height=40,width=40)
# plot(net,vertex.size=size.nodes, edge.arrow.size=0.2, vertex.label=label.nodes,vertex.color=col.nodes,vertex.frame.color=col.nodes,layout=as.matrix(nodes[,2:3]))
# dev.off()

#count how many nodes upstream
cnt.up.nodes <- c()
for (i in 1:length(V(net))){
  print(i)
  cnt.up.nodes[i] <- length(get_upstream_nodes(net,i))
  cat("\014")
}
#save(net,nodes,cnt.up.nodes,file="./net.rda")
#load("./net.rda")

net.indegree <- degree(net, mode="in")
net.outdegree <- degree(net, mode="out")
net.between <- betweenness(net)
net.edge <- edge.betweenness(net)

net.layout <- layout.reingold.tilford(net)
net.layout[,1] <- 1:length(net.layout[,1])
net.layout[,2] <- log10(cnt.up.nodes+1)*100
plot(net,vertex.size=0.2, edge.arrow.size=0.2, vertex.label=NA,vertex.color="black",vertex.frame.color=col.nodes,layout=net.layout[,1:2])
axis(2, at=((0:4)*100/387.9841*2-1), c(1,10,100,1000,10000))

pdf(file="./plots/SewageNet.pdf",height=40,width=40)
plot(net,vertex.size=0.2, edge.arrow.size=0.2, vertex.label=NA,vertex.color="black",vertex.frame.color=col.nodes,layout=net.layout[,1:2])
axis(2, at=((0:4)*100/387.9841*2-1), c(1,10,100,1000,10000),cex.axis=5)
dev.off()

target.pop <- 100
select.sites <- which(cnt.up.nodes+1==target.pop)

#temp.col <- viridis(length(select.sites))
temp.col <- heat.colors(length(select.sites))
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
pdf(file=paste0("./plots/SewageSite_withCatchment_",target.pop,".pdf"),height=40,width=40)
plot(net,vertex.size=size.select.sites, edge.arrow.size=0.1, vertex.label=label.select.sites,vertex.color=col.select.sites,vertex.frame.color=col.select.sites,layout=as.matrix(nodes[,2:3]))
dev.off()

#subgraph
sub.net <- subgraph(net,which(col.select.sites!="black"))
plot(sub.net,vertex.size=size.select.sites[which(col.select.sites!="black")], edge.arrow.size=0.1, vertex.label=label.select.sites[which(col.select.sites!="black")],vertex.color=col.select.sites[which(col.select.sites!="black")],vertex.frame.color=col.select.sites[which(col.select.sites!="black")],layout=as.matrix(nodes[which(col.select.sites!="black"),2:3]))

plot(sub.net,vertex.size=1, edge.arrow.size=0.5, vertex.label=label.select.sites[which(col.select.sites!="black")],
     vertex.color=col.select.sites[which(col.select.sites!="black")],vertex.frame.color=col.select.sites[which(col.select.sites!="black")],
     layout=net.layout[which(col.select.sites!="black"),1:2])
axis(2, at=((0:3)*100/269.897*2-1), c(1,10,100,1000))


#catchment size 500;
#"13950400501" "23070425501" "23190322801" "23260351103" "23380108601"
sites.catch500 <- c("13950400501","23070425501","23190322801","23260351103","23380108601")

#catchment size 200;
sites.catch200 <- c("13940109101","13980300801","13980400901","23250142801","23260239101","23260408201",
                    "23350209001","23390334901")

#catchment size 100;
sites.catch100 <- c("13930110601","23060311401","23070101301","23160105901","23160442201","23160455101",
                    "23230207401","23270345901","23320109401","23330112301","23350421501","23360344706",
                    "23380327001","23390119901","23390206401","24300309201")

dat.manhole500 <- dat.manhole[which(dat.manhole$UNITID %in% sites.catch500),c("UNITID","long","lat")]
dat.manhole500 <- dat.manhole500[-5,] #remove 1 duplicated;
dat.manhole500 <- cbind(dat.manhole500,rep(500,length(sites.catch500)))
names(dat.manhole500) <- c("ID","longitude","latitude","catchment")

dat.manhole200 <- dat.manhole[which(dat.manhole$UNITID %in% sites.catch200),c("UNITID","long","lat")]
dat.manhole200 <- cbind(dat.manhole200,rep(200,length(sites.catch200)))
names(dat.manhole200) <- c("ID","longitude","latitude","catchment")

dat.manhole100 <- dat.manhole[which(dat.manhole$UNITID %in% sites.catch100),c("UNITID","long","lat")]
dat.manhole100 <- dat.manhole100[-c(4,11),] #remove 2 duplicated;
dat.manhole100 <- cbind(dat.manhole100,rep(100,length(sites.catch100)))
names(dat.manhole100) <- c("ID","longitude","latitude","catchment")

dat.manhole.catch <- rbind(dat.manhole100,dat.manhole200,dat.manhole500)
dat.manhole.catch$catchment <- as.factor(dat.manhole.catch$catchment)
write.csv(dat.manhole.catch,file="./data/selected_manholes_with_certain_catchment.csv")

## your api key goes here
api_key <- "ADD YOUR API KEY"
register_google(key = api_key)

base_map <- get_googlemap(center = c(-84.40383,33.75133), zoom=11, maptype = "roadmap")
p <- ggmap(base_map)
p + geom_point(data = dat.manhole.catch,aes(x=longitude,y=latitude,col=catchment),shape=16,size=2) + 
  ggtitle("Sampling sites with different catchment size")



