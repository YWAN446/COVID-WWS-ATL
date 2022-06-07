library(raster)
library(rgdal)
library(rgeos)
library(ggplot2)
library(ggmap)
library(geosphere)
library(sf)
library(igraph)

#load sewage shapefiles;
sewage1 <- readOGR("./data/shapefiles/","SanitarySewerMains")
sewage2 <- readOGR("./data/shapefiles/","SanitarySewerStructures")

sewage_1 <- spTransform(sewage1, CRS("+proj=longlat +datum=WGS84"))
sewage_2 <- spTransform(sewage2, CRS("+proj=longlat +datum=WGS84"))

#manipulate the lines;
dat.manhole1 <- sewage_1@data[,c("UNITID","UPSXCOORD","UPSYCOORD","UPSZCOORD")]

tmp <- sewage_2@data[,c("UNITID","XCOORD","YCOORD","ZCOORD")]
dat.manhole2 <- tmp[-which(duplicated(tmp$UNITID)),]

dat.manhole <- merge(dat.manhole1, dat.manhole2,by="UNITID",all=T)
dat.manhole$UPSXCOORD[which(!is.na(dat.manhole$XCOORD))] <- dat.manhole$XCOORD[which(!is.na(dat.manhole$XCOORD))]
dat.manhole$UPSYCOORD[which(!is.na(dat.manhole$YCOORD))] <- dat.manhole$YCOORD[which(!is.na(dat.manhole$YCOORD))]
dat.manhole$UPSZCOORD[which(!is.na(dat.manhole$ZCOORD))] <- dat.manhole$ZCOORD[which(!is.na(dat.manhole$ZCOORD))]

#clean four manholes;
dat.manhole$UPSXCOORD[which(dat.manhole$UNITID=="22370444509")] <- 2235951-4
dat.manhole$UPSYCOORD[which(dat.manhole$UNITID=="22370444509")] <- 1372117-4
dat.manhole$UPSXCOORD[which(dat.manhole$UNITID=="22370444609")] <- 2235951-2
dat.manhole$UPSYCOORD[which(dat.manhole$UNITID=="22370444609")] <- 1372117-2

dat.manhole$UPSXCOORD[which(dat.manhole$UNITID=="23260260015")] <- 2227345-2
dat.manhole$UPSYCOORD[which(dat.manhole$UNITID=="23260260015")] <- 1365546-2

dat.manhole$UPSXCOORD[which(dat.manhole$UNITID=="23360240809")] <- 2227159-2
dat.manhole$UPSYCOORD[which(dat.manhole$UNITID=="23360240809")] <- 1366230-2

#remove sites without
dat.manhole <- dat.manhole[-which(dat.manhole$UPSXCOORD==0 | dat.manhole$UPSYCOORD==0),c("UNITID","UPSXCOORD","UPSYCOORD","UPSZCOORD")]
dat.manhole$FROMMH <- dat.manhole$UNITID

#remove duplicated records;
dat.manhole <- dat.manhole[-which(duplicated(dat.manhole$UNITID)),]

manhole.gps <- dat.manhole[,c("UPSXCOORD","UPSYCOORD")]
coordinates(manhole.gps) <- 1:2
crs(manhole.gps) <- CRS("+proj=tmerc +lat_0=30 +lon_0=-84.1666666666667 +k=0.9999 +x_0=700000 +y_0=0
                        +datum=NAD83 +units=us-ft +no_defs")
manhole.gps<-spTransform(manhole.gps, crs(sewage_1))
dat.manhole$long <- manhole.gps@coords[,1]
dat.manhole$lat <- manhole.gps@coords[,2]
dat.manhole <- dat.manhole[-which(dat.manhole$long < -85 | dat.manhole$long > -84 | dat.manhole$lat < 33 | dat.manhole$lat > 34.5),]

#network
tmp <- sewage_1@data[,c("FROMMH","TOMH","PIPELEN")]
names(tmp) <- c("from","to","length")
links <- tmp
links <- links[-which(is.na(links$from) | is.na(links$to) | (links$from==links$to)),]
nodes <- dat.manhole[,c("FROMMH","UPSXCOORD","UPSYCOORD")]
names(nodes) <- c("UID","x","y")

links <- links[which(links$from %in% nodes$UID & links$to %in% nodes$UID),]
net <- graph_from_data_frame(links,vertices = nodes[,1],directed = T)
net <- simplify(net)

pdf(file="./v2/output/plots/SewageMap_color.pdf",height=40,width=40)
plot(net,vertex.size=0.2, edge.arrow.size=0.2, vertex.label=NA,layout=as.matrix(nodes[,2:3]))
dev.off()

source("./code/lucy-master/R/misc-utilities.R")
source("./code/lucy-master/R/graph-plots.R")
source("./code/lucy-master/R/graph-sim.R")
source("./code/lucy-master/R/lucy.R")
source("./code/lucy-master/R/propagation.R")

#count how many nodes upstream
# cnt.up.nodes <- c()
# for (i in 1:length(V(net))){
#   print(i)
#   cnt.up.nodes[i] <- length(get_upstream_nodes(net,i))
#   cat("\014")
# }
# save(net,dat.manhole,nodes,links,cnt.up.nodes,file="./net.rda")
load("./net.rda")

target.pop <- 500
#select one subnetwrok with catchment 500;
select.sites <- which(V(net)$name=="23070425401")
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
# pdf(file=paste0("./plots/SewageSite_withCatchment_",target.pop,".pdf"),height=40,width=40)
# plot(net,vertex.size=size.select.sites, edge.arrow.size=0.1, vertex.label=label.select.sites,vertex.color=col.select.sites,vertex.frame.color=col.select.sites,layout=as.matrix(nodes[,2:3]))
# dev.off()

#subgraph
sub.net <- subgraph(net,which(col.select.sites!="black"))
#plot(sub.net,vertex.size=size.select.sites[which(col.select.sites!="black")], edge.arrow.size=0.1, vertex.color=col.select.sites[which(col.select.sites!="black")],vertex.frame.color=col.select.sites[which(col.select.sites!="black")],layout=as.matrix(nodes[which(col.select.sites!="black"),2:3]))

sub.net.layout <- layout.reingold.tilford(sub.net)
sub.net.node.ID <- sort(c(which(V(net)$name==select.sites.ID[1]), get_upstream_nodes(net,which(V(net)$name==select.sites.ID[1]))))
sub.net.layout[,1] <- (1:length(sub.net.layout[,1]))*(sub.net.layout[,1])
sub.net.layout[,2] <- cnt.up.nodes[sub.net.node.ID]+1
sub.net.layout[,2] <- log10(cnt.up.nodes[sub.net.node.ID]+1)*100
#plot(sub.net,vertex.size=size.select.sites[sub.net.node.ID]*10, edge.arrow.size=0.5, vertex.color="black",vertex.label=NA)

l <- layout_with_sugiyama(sub.net)
plot(l$extd_graph, 
     vertex.size=1, edge.arrow.size=0.5, vertex.color="black",vertex.label=NA)

degree(sub.net)

#generate upstream matrix for points;
n.points <- length(V(sub.net))
mat.connect.points <- matrix(0,nrow=n.points,ncol=n.points)
for (i in 1:n.points){
  mat.connect.points[i,get_upstream_nodes(sub.net,i)] <- 1
}
mat.connect.points <- mat.connect.points + diag(n.points)
sub.net.sites <- V(sub.net)$name
rownames(mat.connect.points) <- sub.net.sites
colnames(mat.connect.points) <- sub.net.sites

#generate upstream distance matrix for points;

mat.upstream.dis.points <- matrix(NA,nrow=n.points,ncol=n.points)
for (i in 1:n.points){
  for (j in c(i,get_upstream_nodes(sub.net,i))){
    mat.upstream.dis.points[i,j] <- sum(links$length[which(links$from %in% sub.net.sites[which(mat.connect.points[,j]==1)])]) - sum(links$length[which(links$from %in% sub.net.sites[which(mat.connect.points[,i]==1)])])
  }
}
rownames(mat.upstream.dis.points) <- sub.net.sites
colnames(mat.upstream.dis.points) <- sub.net.sites




