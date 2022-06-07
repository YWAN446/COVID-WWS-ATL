# scenario for single line of sewerage;
#par(mfrow=c(4,4))
par(mar=c(3,2,1,1))
layout(matrix(1:16, 4, 4, byrow = F),
       widths=c(1,1,1,1), heights=c(1,3,1,3))
update=0
# parameters
n.manhole <- 100
n.week <- 4 #adapt every 4 weeks;
n.infection <- 1
target.n.sites <- 4
simul.net <- make_graph(c(1,rep(2:(n.manhole-1),each=2),n.manhole), directed = T)
# single manhole with infections;
risk.ind <- sample(1:n.manhole,n.infection,replace = F)
p.infection <- rep(0,n.manhole)
for (i in 1:length(risk.ind)){
  p.infection[risk.ind[i]:n.manhole] <- 1-(1-p.infection[risk.ind[i]:n.manhole])*pgamma(0:(n.manhole-risk.ind[i]),shape=5,scale=10)
}
p.infection = p.infection * 0.5
# initialization
# generate upstream matrix for manholes;
mat.connect.points <- matrix(0,nrow=n.manhole,ncol=n.manhole)
for (i in 1:n.manhole){
  mat.connect.points[i,get_upstream_nodes(simul.net,i)] <- 1
}
mat.connect.points <- mat.connect.points + diag(n.manhole)
# generate manhole dataset;
simul.net.manholes <- data.frame(cbind(paste("manhole",1:n.manhole),1:n.manhole))
colnames(simul.net.manholes) <- c("ID","cnt")
simul.net.manholes$weight <- rep(1,length(simul.net.manholes$cnt))
simul.net.manholes$cnt <- as.numeric(simul.net.manholes$cnt)
target.site.cnt <- length(simul.net.manholes[,1]) %/% target.n.sites
#simul.net.edge.list <- as_edgelist(simul.net)

#update
for (k.update in 1:16){
dis.path <- distances(simul.net,to=n.manhole)
main_start_node = which(dis.path==max(dis.path))
main_end_node = n.manhole
main_stream <- shortest_paths(simul.net,from=main_start_node,to=main_end_node)$vpath[[1]]
main_stream_ind <- as.numeric(main_stream)
main_stream_manholes <- simul.net.manholes[main_stream_ind,]
main_stream_manholes <- main_stream_manholes[order(main_stream_manholes$cnt,decreasing = T),]
main_stream_manholes$cnt.next <- c(main_stream_manholes$cnt[-1],0)
main_stream_manholes$cnt.diff <- main_stream_manholes$cnt - main_stream_manholes$cnt.next

if (max(main_stream_manholes$cnt.diff)<target.site.cnt){
  selected_sites <- c()
  for (i in 1:target.n.sites){
    selected_sites[i] <- which(abs(main_stream_manholes$cnt-target.site.cnt*i)==min(abs(main_stream_manholes$cnt-target.site.cnt*i)))[1]
  }
  selected_sites_ID <- main_stream_manholes$ID[selected_sites]
}

selected_sites_ind <- c()
for (i in 1:length(selected_sites_ID)){
  selected_sites_ind[i] <- which(simul.net.manholes$ID==selected_sites_ID[i])
}

simul.net.size <- simul.net.manholes$weight
simul.net.size[risk.ind] <- 4
simul.net.col <- rep("black",n.manhole)
simul.net.col[risk.ind] <- "red"
simul.net.shape <- rep("circle",n.manhole)
simul.net.shape[selected_sites_ind] <- "rectangle"

plot(simul.net,xlim=c(-0.95,0.95),ylim=c(-1.5,-0.5),asp = 0,
     vertex.size=simul.net.size, edge.arrow.size=0.1, vertex.color=simul.net.col, vertex.shape=simul.net.shape,vertex.label=NA, 
     layout=as.matrix(cbind(1:100,rep(0,100))))#vertex.label=paste0(V(net)$name[sub.net.node.ID],"\n",cnt.up.nodes[sub.net.node.ID]))
mtext(paste0("Update ",update),side = 2,adj = 0)
plot(1:n.manhole,simul.net.manholes$weight,type="l")
abline(v=risk.ind,col="red",lwd=2)

#adapt the weight of the sites based on results;
inf.mat <- matrix(c(rbinom(n.week,size=1,prob=p.infection[selected_sites_ind[1]]),
                    rbinom(n.week,size=1,prob=p.infection[selected_sites_ind[2]]),
                    rbinom(n.week,size=1,prob=p.infection[selected_sites_ind[3]]),
                    rbinom(n.week,size=1,prob=p.infection[selected_sites_ind[4]])),
                  nrow=4,byrow=T)

area_sites_control <- list()
for (i in 1:target.n.sites){
  if (i==1){
    area_sites_control[[i]] <- c(get_upstream_nodes(simul.net,selected_sites_ind[i]),selected_sites_ind[i])
  } else {
    area_sites_control[[i]] <- setdiff(c(get_upstream_nodes(simul.net,selected_sites_ind[i]),selected_sites_ind[i]),
                                       c(get_upstream_nodes(simul.net,selected_sites_ind[i-1]),selected_sites_ind[i-1]))
  }
}

weight.increase <- rowMeans(inf.mat)
if (all(weight.increase>0)){
  weight.increase <- (length(weight.increase):1/length(weight.increase))*weight.increase
}

for (i in 1:length(weight.increase)){
  simul.net.manholes$weight[area_sites_control[[i]]] <- simul.net.manholes$weight[area_sites_control[[i]]] * (1+weight.increase[i])
}
simul.net.manholes$weight <- simul.net.manholes$weight/sum(simul.net.manholes$weight)*length(simul.net.manholes$weight)
simul.net.manholes$cnt <- mat.connect.points %*% simul.net.manholes$weight
update = update+1
}

# assuming no decay and lab method is perfect; All the sites downstream of manholes with infections will be positive;



