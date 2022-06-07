#COVID-19 wastewater surveillance in Atlanta;
#set direction
setwd("~/stat/nCoV_SewageSurveillance/")
#packages
library(igraph)
library(rgdal)
library(rgeos)
library(ggmap)
library(geosphere)
library(sf)
library(ggplot2)
library(doParallel)
library(doRNG)

#load some functions;
source("./code/lucy-master/R/misc-utilities.R")
source("./code/lucy-master/R/graph-plots.R")
source("./code/lucy-master/R/graph-sim.R")
source("./code/lucy-master/R/lucy.R")
source("./code/lucy-master/R/propagation.R")

#load sewerage network, manhole date, nodes, links and catchment manhole size;
load("./net.rda")
#subset the network;
k.site.ID <- "23070425401"
sub.net.node.ind <- sort(c(which(V(net)$name==k.site.ID), get_upstream_nodes(net,which(V(net)$name==k.site.ID))))
sub.net <- induced_subgraph(net,sub.net.node.ind)
sub.net.dat.manhole <- dat.manhole[sub.net.node.ind,]
n.manhole <- length(V(sub.net))

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
mat.upstream.dis.points <- mat.upstream.dis.points/1000 #change from meter to km?

#parameters;
n.days <- 1000
#decay rate;
gamma.shape <- 1
gamma.rate <- 0.25

#infection pressure;
lambda <- 5
n.outbreak.area <- 2
outbreak.range <- 0.02
outbreak.factor <- 5

#parameters of transmission;
mu.shed <- 10^8
sigma.shed <- 1
n.days.shed <- 21

#sampling
target.n.sites <- 5
LLOD.test <- 0.1
total.vol <- 1e10 #1e10 liter per day;


n.infected <- rpois(n.days,lambda)
manhole_point <- sort(c(runif(n.manhole-1,0,n.manhole),0,n.manhole))

manhole_range <- data.frame(start=manhole_point[-length(manhole_point)], end=manhole_point[-1])
manhole_range$length <- manhole_range$end - manhole_range$start

gps.points.range.x <- range(sub.net.dat.manhole$long)
gps.points.range.y <- range(sub.net.dat.manhole$lat)
gps.points <- sub.net.dat.manhole[,c("long","lat")]

outbreak.x <- runif(n.outbreak.area,gps.points.range.x[1],gps.points.range.x[2])
outbreak.y <- runif(n.outbreak.area,gps.points.range.y[1],gps.points.range.y[2])
outbreak.points <- which(((gps.points[,1]-outbreak.x[1])/(gps.points.range.x[2]-gps.points.range.x[1]))^2+((gps.points[,2]-outbreak.y[1])/(gps.points.range.y[2]-gps.points.range.y[1]))^2<=outbreak.range)
for (j in 1:n.outbreak.area){
  outbreak.points <- unique(c(outbreak.points,which(((gps.points[,1]-outbreak.x[j])/(gps.points.range.x[2]-gps.points.range.x[1]))^2+((gps.points[,2]-outbreak.y[j])/(gps.points.range.y[2]-gps.points.range.y[1]))^2<=outbreak.range)))
}
manhole_range$outbreak.length <- manhole_range$length
manhole_range$outbreak.length[outbreak.points] <- manhole_range$length[outbreak.points]*outbreak.factor
manhole_range$outbreak.length <- manhole_range$outbreak.length/sum(manhole_range$outbreak.length)*n.manhole #standardize to n.manhole;
manhole_range$outbreak.end <- cumsum(manhole_range$outbreak.length)
manhole_range$outbreak.start <- c(0,manhole_range$outbreak.end[-n.manhole])  

#show the risk map;
my.size <- manhole_range$outbreak.length
plot(sub.net, 
     vertex.size=my.size, edge.arrow.size=0.5, vertex.color=my.col, vertex.label=NA, 
     layout=as.matrix(nodes[sub.net.node.ind,2:3]),main="risk area")


mat.manhole.pos <- matrix(0,nrow=n.days,ncol=n.manhole)
for (t in which(n.infected>0)){
  vec.loc.pos <- sample(x=1:n.manhole,size=n.infected[t],prob=manhole_range$outbreak.length,replace = TRUE)
  mat.manhole.pos[t,unique(sort(vec.loc.pos))] <- table(vec.loc.pos)
}

#shedding
mat.count.gen <- matrix(0,nrow=n.days,ncol=n.manhole)
array.count.gen <- array(0, dim=c(n.days,n.manhole,n.days.shed))

k.shed <- sort(unique(as.vector(mat.manhole.pos)))
k.shed <- k.shed[which(k.shed>0)]

Mu.shed <- log(mu.shed)-0.5*sigma.shed^2

for (k in 1:n.days.shed){
  mat.count.tmp <- matrix(0,nrow=n.days,ncol=n.manhole)
  for (i in k.shed){
    manhole.shed.i <- which(mat.manhole.pos==i)
    if (i==1){
      mat.count.tmp[manhole.shed.i] <- round(rlnorm(length(manhole.shed.i),Mu.shed,sigma.shed)*rbinom(length(manhole.shed.i),1,5*dnbinom(k,3,0.4)),0)
    } else {
      mat.count.tmp[manhole.shed.i] <- round(rowSums(matrix(rlnorm(length(manhole.shed.i)*i,Mu.shed,sigma.shed)*rbinom(length(manhole.shed.i)*i,1,5*dnbinom(k,3,0.4)),ncol=i)),0)
    }
  }
  array.count.gen[k:n.days,,k] <- mat.count.tmp[1:(n.days+1-k),]
  rm(mat.count.tmp)
}

mat.count.gen <- rowSums(array.count.gen,dims=2)

#assume decay and lost;
mat.upstream.dis.points.prop <- 1-pgamma(mat.upstream.dis.points,shape=gamma.shape,rate=gamma.rate)
mat.upstream.dis.points.prop[which(is.na(mat.upstream.dis.points.prop))] <- 0
mat.count <- matrix(NA,nrow=n.days,ncol=n.manhole)

mat.count <- round(mat.count.gen%*%t(mat.upstream.dis.points.prop),0)

#prob of detecting positive
vol.dilution <- total.vol * cnt.up.nodes[sub.net.node.ind]/n.manhole

mat.conc <- t(apply(mat.count,1,FUN = function(x) x/vol.dilution))


