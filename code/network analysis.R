#COVID-19 wastewater surveillance in Atlanta;
#set direction
setwd("~/stat/nCoV_SewageSurveillance/")
#packages
library(igraph)
library(concaveman)
library(tidyverse)
library(maps)
library(raster)
library(rgdal)
library(rgeos)
library(ggplot2)
library(ggmap)
library(geosphere)
library(sf)
library(readxl)
library(gridExtra)
library(grid)
library(patchwork)

#load sewage shapefiles;
sewage1 <- readOGR("./data/shapefiles/","SanitarySewerMains")
sewage2 <- readOGR("./data/shapefiles/","SanitarySewerStructures")
combinedsewer <- readOGR("./data/shapefiles/","Combined Sewer Areas")

sewage_1 <- spTransform(sewage1, CRS("+proj=longlat +datum=WGS84"))
sewage_2 <- spTransform(sewage2, CRS("+proj=longlat +datum=WGS84"))
combined_sewer <- spTransform(combinedsewer, CRS("+proj=longlat +datum=WGS84"))

#load some functions;
source("./code/lucy-master/R/misc-utilities.R")
source("./code/lucy-master/R/graph-plots.R")
source("./code/lucy-master/R/graph-sim.R")
source("./code/lucy-master/R/lucy.R")
source("./code/lucy-master/R/propagation.R")

#load sewerage network, manhole date, nodes, links and catchment manhole size;
load("./net.rda")

#network
tmp <- sewage_1@data[,c("FROMMH","TOMH","SEWERSHED","BASIN","PIPEDIAM","PIPELEN","UPSINV")]
names(tmp) <- c("from","to","sewershed","basin","diameter","length","ups_inv")
links <- tmp
links <- links[-which(is.na(links$from) | is.na(links$to) | (links$from==links$to)),]
nodes <- dat.manhole[,c("FROMMH","UPSXCOORD","UPSYCOORD")]
names(nodes) <- c("UID","x","y")

links <- links[which(links$from %in% nodes$UID & links$to %in% nodes$UID),]
net <- graph_from_data_frame(links,vertices = nodes[,1],directed = T)
net <- simplify(net)

pdf("./plots/sewer_map.pdf",width=6,height=6)
plot(sewage_1)
dev.off()

pdf("./plots/combined_sewer_map.pdf",width=6,height=6)
plot(sewage_1)
plot(combined_sewer,col=rgb(1, 99/255, 71/255, 0.5),add = TRUE)
legend(
  "topleft",
  legend = c("combined sewage","sewer line"),
  col = c(NA,"black"),
  fill = c(rgb(1, 99/255, 71/255, 0.5),NA),
  lty=c(NA,1),
  border = c(rgb(1, 99/255, 71/255, 0.5),NA), 
  lwd=1, 
  bty    = "n",
  cex = 1
)
dev.off()

###degree;
dat.manhole$indegree <- degree(net,mode = "in")
dat.manhole$outdegree <- degree(net,mode = "out")

indegree.size <- dat.manhole$indegree * 0.5
indegree.size[which(indegree.size<=1.5)] <- 0.1

outdegree.size <- dat.manhole$outdegree
outdegree.size[which(outdegree.size<=2)] <- 0.1

pdf("./plots/degree_map.pdf",width=12,height=6)
par(mfrow=c(1,2))
par(mar=c(1,1,1,1))
plot(net,vertex.size=indegree.size,vertex.color="tomato",edge.arrow.size=0.05, vertex.label=NA,layout=as.matrix(nodes[,2:3]))
legend(
  "topleft",
  legend = c("<=3","4","5","6","7","8","9","10"),
  pt.bg  = "tomato",
  pch    = 21,
  pt.cex = sort(unique(indegree.size))/3,
  bty    = "n",
  cex = 1.5,
  title="Indegree"
)

plot(net,vertex.size=outdegree.size,vertex.color="cyan",edge.arrow.size=0.05, vertex.label=NA,layout=as.matrix(nodes[,2:3]))
legend(
  "topleft",
  legend = c("<=2","3","4","5"),
  pt.bg  = "cyan",
  pch    = 21,
  pt.cex = sort(unique(outdegree.size))/3,
  bty    = "n",
  cex = 1.5,
  title="Outdegree"
)
dev.off()

pdf("./plots/degree_dist.pdf",width=6,height=6)
par(mfrow=c(2,1))
par(mar=c(4.1,4.1,1,1))
barplot(table(dat.manhole$indegree),xlab="Indegree",ylab="Number of Manholes",xlim=c(0,10),ylim=c(0,50000))
barplot(table(dat.manhole$outdegree),xlab="Outdegree",ylab="Number of Manholes",xlim=c(0,10),ylim=c(0,50000))
dev.off()

#authority;
dat.manhole$authority <- authority.score(net)$vector

authority <- (dat.manhole$authority*10^16)/2
authority[which(authority<3)] <- 0.1
authority[which(authority>8)] <- 8

pdf("./plots/authority_map.pdf",width=10,height=10)
par(mfrow=c(1,1))
plot(net,vertex.size=authority,vertex.color="tomato",edge.arrow.size=0.05, vertex.label=NA,layout=as.matrix(nodes[,2:3]))
legend(
  "topleft",
  legend = c("<=6e-16","8e-16","1e-15","1.2e-15","1.4e-15",">1.6e-16"),
  pt.bg  = "tomato",
  pch    = 21,
  pt.cex = c(0.1,4,5,6,7,8)/2.5,
  bty    = "n",
  cex = 1.5,
  title="Authority"
)
dev.off()

###pipe diameter;
links$diameter[which(links$diameter==0)] <- 2
pipe.size <- rep(0.1,length(links$diameter))
pipe.size[which(links$diameter>8 & links$diameter<=24)] <- 0.2
pipe.size[which(links$diameter>24 & links$diameter<=48)] <- 1
pipe.size[which(links$diameter>48 & links$diameter<=96)] <- 3
pipe.size[which(links$diameter>96)] <- 5

pdf("./plots/diameter.pdf",width=10,height=10)
par(mfrow=c(1,1))
plot(net,vertex.size=0.01,vertex.color="black",vertex.frame.color="white",edge.arrow.size=0.01, edge.color="black", edge.width=pipe.size, vertex.label=NA,layout=as.matrix(nodes[,2:3]))
legend(
  "topleft",
  legend = c("<=8","8-24","24-48","48-96",">96"),
  lwd = c(0.1,0.2,1,3,5),
  bty    = "n",
  cex = 1.5,
  title="Diameter"
)
dev.off()

###sewer shed;
pdf("./plots/sewershed.pdf",width=6,height=6)
par(mfrow=c(1,1))
E(net)$color <- factor(links$sewershed)
plot(net,vertex.size=0.1,vertex.color="black",edge.arrow.size=0.05, vertex.label=NA,layout=as.matrix(nodes[,2:3]))
dev.off()

###basin;
pdf("./plots/basin.pdf",width=6,height=6)
par(mfrow=c(1,1))
E(net)$color <- factor(links$basin)
plot(net,vertex.size=0.1,vertex.color="black",edge.arrow.size=0.05, vertex.label=NA,layout=as.matrix(nodes[,2:3]))
dev.off()

###elevation map;
elevation <- links$ups_inv
elevation[which(elevation>2000)] <- NA
elevation[which(elevation<500)] <- NA

fine = 500 # this will adjust the resolving power.
pal = colorRampPalette(c('blue','red'))
#this gives you the colors you want for every point
graphCol = pal(fine)[as.numeric(cut(elevation,breaks = fine))]

pdf("./plots/elevation.pdf",width=6,height=6)
par(mfrow=c(1,1))
plot(net,vertex.size=0.5,vertex.color=graphCol,vertex.frame.color=graphCol,edge.arrow.size=0.05, vertex.label=NA,layout=as.matrix(nodes[,2:3]))
legend(
  "topleft",
  legend=seq(min(elevation,na.rm=T),max(elevation,na.rm=T),by=41.383),
  fill=pal(fine)[c(seq(1,500,by=50),500)],
  bty    = "n",
  cex = 1.5,
  title="Elevation"
)
dev.off()


#influent sites catchment;
select.sites <- c(which(V(net)$name=="13760400901"),which(V(net)$name=="13760401701"),which(V(net)$name=="13760400801"),
                  which(V(net)$name=="23330211001"),which(V(net)$name=="23340422106"),which(V(net)$name=="23330209701"))

temp.col <- c("tomato","palegreen","gold","hotpink","cyan","red2")
name.sites <- c("Phillip Lee","Old Winn Dixie","South Fulton","Jonesboro","Flint","Intrenchment")
select.sites.ID <- V(net)$name[select.sites]
col.select.sites <- rep("black",length(V(net)))
for (i in 1:length(select.sites)){
  col.select.sites[c(which(V(net)$name==select.sites.ID[i]), get_upstream_nodes(net,which(V(net)$name==select.sites.ID[i])))] <- temp.col[i]
}
pdf("./plots/influ_sites_degree.pdf",width=8,height=8)
for (i in 1:6){
  sub.net <- induced_subgraph(net,which(col.select.sites==temp.col[i]))
  plot(sub.net,vertex.size=1.5,vertex.color=temp.col[i],edge.arrow.size=0.2, vertex.label=NA,
       layout=as.matrix(nodes[which(col.select.sites==temp.col[i]),2:3]),
       main=paste0(name.sites[i],", avg indegree=",round(mean(degree(sub.net,mode = "in")[which(degree(sub.net,mode = "in")>0)]),2),
                   ", avg outdegree=",round(mean(degree(sub.net,mode = "out")[which(degree(sub.net,mode = "out")>0)]),2)))
}
dev.off()

pdf("./plots/net_sites_example.pdf",width=8,height=8)
k.site.ID="23370330601"
sub.net.node.ind <- sort(c(which(V(net)$name==k.site.ID), get_upstream_nodes(net,which(V(net)$name==k.site.ID))))
sub.net <- induced_subgraph(net,sub.net.node.ind)
plot(sub.net,vertex.size=1,vertex.color="tomato",edge.arrow.size=0.2, vertex.label=NA,
     layout=as.matrix(nodes[sub.net.node.ind,2:3]),
     main=paste0(k.site.ID,", avg indegree=",round(mean(degree(sub.net,mode = "in")[which(degree(sub.net,mode = "in")>0)]),2),
     ", avg outdegree=",round(mean(degree(sub.net,mode = "out")[which(degree(sub.net,mode = "out")>0)]),2)))
dev.off()
# sub.net <- induced_subgraph(net,c(which(V(net)$name=="23330211001"), get_upstream_nodes(net,which(V(net)$name=="23330211001"))))
# plot(sub.net,vertex.size=1,vertex.color="tomato",edge.arrow.size=0.2, vertex.label=NA,
#      layout=as.matrix(nodes[c(which(V(net)$name=="23330211001"), get_upstream_nodes(net,which(V(net)$name=="23330211001"))),2:3]),
#      main=paste0(name.sites[i],", avg indegree=",round(mean(degree(sub.net,mode = "in")[which(degree(sub.net,mode = "in")>0)]),2),
#                  ", avg outdegree=",round(mean(degree(sub.net,mode = "out")[which(degree(sub.net,mode = "out")>0)]),2)))

all_simple_paths(sub.net,from=1,to=1977)
all_shortest_paths(sub.net,from=1,to=1977)

temp.size <- rep(0.01,length(nodes$UID))
temp.size[which(cnt.up.nodes>1000)] <- 1
temp.label <- dat.manhole$UNITID
temp.label[which(cnt.up.nodes<=1000)] <- NA
#temp.label[which(cnt.up.nodes>1100)] <- NA

pdf("./plots/test.pdf",height=100,width=100)
plot(net,vertex.size=temp.size,vertex.color="tomato",edge.arrow.size=0.05, vertex.label=temp.label,layout=as.matrix(nodes[,2:3]))
dev.off()

