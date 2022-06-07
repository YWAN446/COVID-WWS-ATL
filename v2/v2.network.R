par(mfrow=c(2,1))
l <- layout_with_sugiyama(sub.net)
plot(l$extd_graph, 
     vertex.size=5, edge.arrow.size=0.5, vertex.color="black",vertex.label=NA)

clus <- cluster_edge_betweenness(sub.net)
plot(clus, sub.net, 
     vertex.size=1, edge.arrow.size=0.5, vertex.color="black",vertex.label=NA, layout=l$layout)

par(mfrow=c(2,1))
sg <- cluster_spinglass(sub.net, spin=4)
plot(sg, sub.net, 
     vertex.size=3, edge.arrow.size=0.2, vertex.color="black",vertex.label=NA, layout=l$layout)

sg <- cluster_spinglass(sub.net, spin=3)
plot(sg, sub.net, 
     vertex.size=3, edge.arrow.size=0.2, vertex.color="black",vertex.label=NA, layout=l$layout)



#working on the initialization algorithm on a known sewerage network.
#subset the network
k.site.ID <- "23070425401"

target.n.sites <- 5
sub.net.node.ind <- sort(c(which(V(net)$name==k.site.ID), get_upstream_nodes(net,which(V(net)$name==k.site.ID))))
sub.net <- induced_subgraph(net,sub.net.node.ind)
sub.net.manholes <- data.frame(cbind(V(net)$name[sub.net.node.ind],cnt.up.nodes[sub.net.node.ind]))
colnames(sub.net.manholes) <- c("ID","cnt")
sub.net.manholes$cnt <- as.numeric(sub.net.manholes$cnt)+1
sub.net.manholes$weight <- rep(1,length(sub.net.manholes$cnt))
sub.net.manholes$indegree <- degree(sub.net,mode="in")
sub.net.edge.list <- as_edgelist(sub.net)

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

#Find all the merging points and points before merging, which will be potential sampling sites for branches;
merging.points.ID <- sub.net.manholes$ID[which(sub.net.manholes$indegree>1)]
b4merging.points.ID <- sub.net.edge.list[which(sub.net.edge.list[,2] %in% merging.points.ID),1]
b4merging.manholes <- sub.net.manholes[which(sub.net.manholes$ID %in% b4merging.points.ID),]
#find the connection between all b4merging points;
mat.connect.b4merging.points <- mat.connect.points[which(sub.net.manholes$ID %in% b4merging.points.ID),which(sub.net.manholes$ID %in% b4merging.points.ID)]

#identify the longest path to the endpoint of the network in the sub network as the main stream;
dis.path <- distances(sub.net,to=which(V(sub.net)$name==k.site.ID))
main_start_node = rownames(dis.path)[which(dis.path==max(dis.path))[1]]
main_end_node = k.site.ID
main_stream <- shortest_paths(sub.net,from=which(V(sub.net)$name==main_start_node),to=which(V(sub.net)$name==main_end_node))$vpath[[1]]
main_stream_ind <- as.numeric(main_stream)
main_stream_ID <- attributes(main_stream)$names
main_stream_manholes <- sub.net.manholes[which(sub.net.manholes$ID %in% main_stream_ID),]
main_stream_manholes <- main_stream_manholes[order(main_stream_manholes$cnt,decreasing = T),]
# #only keep the points before merging;
# main_stream_manholes <- main_stream_manholes[which(main_stream_manholes$ID %in% b4merging.points.ID),]
branch_manholes <- b4merging.manholes[which(!b4merging.manholes$ID %in% main_stream_manholes$ID),]

#Initalization;
target.n.branch.sites <- target.n.sites - 2 #one site at the downstream end point and one site on the main stream;
branch_selected_sites_ID <- c()
for (i in 1:target.n.branch.sites){
  if (i==1){
    branch_selected_sites_ID[i] <- branch_manholes$ID[order(branch_manholes$cnt,decreasing = T)[1]]
  }
  if (i>1){
    temp.included <- mat.connect.b4merging.points[which(b4merging.manholes$ID %in% branch_selected_sites_ID),]
    temp.included.manholes.ID <- b4merging.manholes$ID[which(colSums(rbind(rep(0,length(mat.connect.b4merging.points[1,])),temp.included))>0)]
    temp.manholes <- branch_manholes[-which(branch_manholes$ID %in% temp.included.manholes.ID),]
    branch_selected_sites_ID[i] <- temp.manholes$ID[order(temp.manholes$cnt,decreasing = T)[1]]
  }
}

main_selected_sites_ID <- main_stream_manholes$ID[which(abs(main_stream_manholes$cnt-0.5*n.points)==min(abs(main_stream_manholes$cnt-0.5*n.points)))[1]]
selected_sites_ID <- c(k.site.ID,main_selected_sites_ID,branch_selected_sites_ID)

#plot
l <- layout_with_sugiyama(sub.net)
sub.net.size <- rep(2,length(V(sub.net)))
sub.net.size[which(sub.net.manholes$ID %in% selected_sites_ID)] <- 5
sub.net.col <- rep("black",length(V(sub.net)))
sub.net.col[which(sub.net.manholes$ID %in% selected_sites_ID)] <- "red"
sub.net.label <- rep(NA,length(V(sub.net)))
sub.net.label[which(sub.net.manholes$ID %in% selected_sites_ID)] <- sub.net.manholes$cnt[which(sub.net.manholes$ID %in% selected_sites_ID)]

sub.net.col <- rep("black",length(V(sub.net)))
selected_sites_ID_nodes <- list()
selected_sites_ID_n_nodes <- c()
for (i in 1:length(selected_sites_ID)){
  selected_sites_ID_nodes[[i]] <- get_upstream_nodes(sub.net,which(V(sub.net)$name==selected_sites_ID[i]))
  selected_sites_ID_n_nodes[i] <- length(selected_sites_ID_nodes[[i]])
}

for (i in order(selected_sites_ID_n_nodes,decreasing = T)){
  sub.net.col[selected_sites_ID_nodes[[i]]] <- i
}
sub.net.col[which(sub.net.manholes$ID %in% selected_sites_ID)] <- length(selected_sites_ID_n_nodes)+1

# pdf(paste0("./v1/output/plots/init_map_",k.site.ID,"_",target.n.sites,".pdf"),width=20,height=20)
plot(sub.net, 
     vertex.size=sub.net.size, edge.arrow.size=0.5, vertex.color=sub.net.col, vertex.label=sub.net.label, 
     layout=as.matrix(nodes[sub.net.node.ind,2:3]))#vertex.label=paste0(V(net)$name[sub.net.node.ID],"\n",cnt.up.nodes[sub.net.node.ID]))
# dev.off()
