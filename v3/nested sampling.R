cnt.up.nodes[which(V(net)$name=="13760400901")] #Phillip Lee Influent Line
cnt.up.nodes[which(V(net)$name=="13760401701")] #Old Winn Dixie Influent Line
cnt.up.nodes[which(V(net)$name=="13760400801")] #South Fulton Influent Line
cnt.up.nodes[which(V(net)$name=="23330211001")] # Jonesboro
cnt.up.nodes[which(V(net)$name=="23340422106")] # Flint
cnt.up.nodes[which(V(net)$name=="23330209701")] # Intrenchment

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
# pdf(file=paste0("./presentation/plots/Influent.pdf"),height=40,width=40)
# plot(net,vertex.size=size.select.sites, edge.arrow.size=0.1, vertex.label=label.select.sites,vertex.color=col.select.sites,vertex.frame.color=col.select.sites,layout=as.matrix(nodes[,2:3]))
# legend("topleft",c("South Fulton","Phillip Lee","Old Winn Dixie","Jonesboro","Intrenchment","Flint"),col=c("gold","tomato","palegreen","hotpink","red2","cyan"),pch=16,title="Influent Site",cex=8)
# #legend("topleft",c("Phillip Lee","Old Winn Dixie","South Fulton","Jonesboro","Flint","Intrenchment"),col=temp.col,pch=16,title="Influent Site",cex=8)
# dev.off()

#pdf(file=paste0("./plots/nested_sampling.pdf"),height=10,width=10)
for (i.site in c(1,3,4,5,6)){
pdf(file=paste0("./plots/nested_sampling_",site.names[i.site],".pdf"),height=10,width=10)
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

#k.site.ID <- influ.net.manholes$ID[which(influ.net.manholes$cnt==sort(influ.net.manholes$cnt[influ.net.manholes$cnt>=600])[1])[1]]
k.site.ID <- V(net)$name[select.sites[i.site]]

sub.net.node.ind <- sort(c(which(V(influ.net)$name==k.site.ID), get_upstream_nodes(influ.net,which(V(influ.net)$name==k.site.ID))))
sub.net <- induced_subgraph(influ.net,sub.net.node.ind)

#initialize;
sub.net.manholes <- data.frame(cbind(V(influ.net)$name[sub.net.node.ind],influ.cnt.up.nodes[sub.net.node.ind]))
colnames(sub.net.manholes) <- c("ID","cnt")
sub.net.manholes$cnt <- as.numeric(sub.net.manholes$cnt)+1
sub.net.manholes$weight <- rep(2,length(sub.net.manholes$cnt))
sub.net.manholes$indegree <- degree(sub.net,mode="in")
sub.net.manholes$outdegree <- degree(sub.net,mode="out")
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
#main_stream_manholes <- main_stream_manholes[order(main_stream_manholes$cnt,decreasing = T),]
main_branch_cut <- min(main_stream_manholes$cnt[which(main_stream_manholes$cnt>100 & main_stream_manholes$indegree>1)])
main_stream_ID <- main_stream_manholes$ID[which(main_stream_manholes$cnt>=main_branch_cut)]
main_stream_sub_ind <- which(V(sub.net)$name %in% main_stream_ID)
main_stream_manholes <- main_stream_manholes[which(main_stream_manholes$cnt>=main_branch_cut),]
# #only keep the points before merging;
# main_stream_manholes <- main_stream_manholes[which(main_stream_manholes$ID %in% b4merging.points.ID),]
branch_manholes <- b4merging.manholes[which(!b4merging.manholes$ID %in% main_stream_manholes$ID),]

branch_cover_main_stream <- function(ID, sub.net, main_stream_ind){
  return(any(get_upstream_nodes(sub.net,which(V(sub.net)$name==ID)) %in% main_stream_ind))
}
branch_remove <- which(sapply(branch_manholes$ID,FUN = branch_cover_main_stream, sub.net=sub.net, main_stream_ind=main_stream_sub_ind))
if (length(branch_remove)>0){
  branch_manholes <- branch_manholes[-branch_remove,]
}
# branch_manholes <- branch_manholes[-which(branch_manholes$cnt>max(main_stream_manholes$cnt-c(main_stream_manholes$cnt[-1],0))),]

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
    temp_remove <- which(sapply(temp.manholes$ID,FUN = branch_cover_main_stream, sub.net=sub.net, main_stream_ind=which(sub.net.manholes$ID %in% temp.included.manholes.ID)))
    if (length(temp_remove)>0){
      temp.manholes <- temp.manholes[-temp_remove,]
    }
    branch_selected_sites_ID[i] <- temp.manholes$ID[order(temp.manholes$cnt,decreasing = T)[1]]
  }
}

main_selected_sites_ID <- main_stream_manholes$ID[which(abs(main_stream_manholes$cnt-0.5*n.points)==min(abs(main_stream_manholes$cnt-0.5*n.points)))[1]]
#only use the branch points;
#selected_sites_ID <- c(k.site.ID,main_selected_sites_ID,branch_selected_sites_ID)
selected_sites_ID <- branch_selected_sites_ID

sub.net.size <- sub.net.manholes$weight
sub.net.size[which(sub.net.manholes$ID %in% selected_sites_ID)] <- 5
sub.net.col <- rep("gray",length(V(sub.net)))
sub.net.col[main_stream_sub_ind] <- "black"
sub.net.col[which(sub.net.manholes$ID %in% selected_sites_ID)] <- "red"
sub.net.label <- rep(NA,length(V(sub.net)))
for (i in 1:length(selected_sites_ID)){
  sub.net.label[which(sub.net.manholes$ID==selected_sites_ID[i])] <- sub.net.manholes$cnt[which(sub.net.manholes$ID==selected_sites_ID[i])]
}

selected_sites_ID_nodes <- list()
selected_sites_ID_n_nodes <- c()
for (i in 1:length(selected_sites_ID)){
  selected_sites_ID_nodes[[i]] <- c(which(V(sub.net)$name==selected_sites_ID[i]),get_upstream_nodes(sub.net,which(V(sub.net)$name==selected_sites_ID[i])))
  selected_sites_ID_n_nodes[i] <- length(selected_sites_ID_nodes[[i]])
}

for (i in order(selected_sites_ID_n_nodes,decreasing = T)){
  sub.net.col[selected_sites_ID_nodes[[i]]] <- i
}
sub.net.col[which(sub.net.manholes$ID %in% selected_sites_ID)] <- "red"

plot(sub.net, 
     vertex.size=sub.net.size, edge.arrow.size=0.5, vertex.color=sub.net.col, vertex.label=sub.net.label, 
     layout=as.matrix(nodes[influ.net.node.ind[sub.net.node.ind],2:3]))#vertex.label=paste0(V(net)$name[sub.net.node.ID],"\n",cnt.up.nodes[sub.net.node.ID]))
title(site.names[i.site],cex.main=3)

legend("bottomleft",legend=c("main stream","not covered","sampling site"),col=c("black","gray","red"),pch=16,cex=2)
text(1,1,paste0(c("Sites Selected:\n" ,paste0(selected_sites_ID,"\n")),collapse=""),cex=1.5)
dev.off()
}















# influ.net.size <- rep(1.5,length(influ.net.manholes$ID))
# influ.net.size[which(influ.net.manholes$ID %in% selected_sites_ID)] <- 5
# 
# influ.net.col <- rep("black",length(V(influ.net)))
# influ.net.col[sub.net.node.ind] <- sub.net.col
# 
# influ.net.label <- rep(NA,length(V(influ.net)))
# for (i in 1:length(selected_sites_ID)){
#   influ.net.label[which(influ.net.manholes$ID==selected_sites_ID[i])] <- influ.net.manholes$cnt[which(influ.net.manholes$ID==selected_sites_ID[i])]
# }
# 
# plot(influ.net,
#      vertex.size=influ.net.size, edge.arrow.size=0.5, vertex.color=influ.net.col, vertex.label=influ.net.label, 
#      layout=as.matrix(nodes[influ.net.node.ind,2:3]))
# title(site.names[i.site],cex.main=3)
# 
# 
# main_stream_size <- rep(1,length(V(influ.net)))
# main_stream_size[main_stream_ind] <- 2
# main_stream_col <- rep("black",length(V(influ.net)))
# main_stream_col[main_stream_ind] <- "red"
# plot(influ.net,
#      vertex.size=main_stream_size, edge.arrow.size=0.5, vertex.color=main_stream_col, vertex.label=NA, 
#      layout=as.matrix(nodes[influ.net.node.ind,2:3]))
# title(paste0("Main Stream (",site.names[i.site],")"),cex.main=3)

# test.col <- rep("gray",length(sub.net.manholes$ID))
# test.col[which(sub.net.manholes$outdegree>1)] <- "red"
# test.col[main_stream_ind] <- "black"
# test.label <- rep(NA,length(V(sub.net)))
# test.label[which(sub.net.manholes$outdegree>1)] <- sub.net.manholes$cnt[which(sub.net.manholes$outdegree>1)]
# 
# pdf(file=paste0("./plots/nested_sampling_test.pdf"),height=10,width=10)
# plot(sub.net,
#      vertex.size=1, edge.arrow.size=0.5, vertex.color=test.col, vertex.label=test.label,
#      layout=as.matrix(nodes[influ.net.node.ind[sub.net.node.ind],2:3]))
# dev.off()
