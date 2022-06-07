#Try the adapt steps;
k.update <- 0
k.day <- 0
n.week=2
target.n.sites <- 6
pdf(paste0("./v4/output/plots/update_map_",k.site.ID,"_",target.n.sites,".pdf"),width=30,height=30)
par(mfrow=c(3,3))

#show the risk map;
my.size <- manhole_range$outbreak.length
plot(sub.net, 
     vertex.size=my.size, edge.arrow.size=0.5, vertex.color="black", vertex.label=NA, 
     layout=as.matrix(nodes[sub.net.node.ind,2:3]))
title("risk area",cex.main=5)

sub.net.node.ind <- sort(c(which(V(net)$name==k.site.ID), get_upstream_nodes(net,which(V(net)$name==k.site.ID))))
sub.net <- induced_subgraph(net,sub.net.node.ind)

#initialize;
sub.net.manholes <- data.frame(cbind(V(net)$name[sub.net.node.ind],cnt.up.nodes[sub.net.node.ind]))
colnames(sub.net.manholes) <- c("ID","cnt")
sub.net.manholes$cnt <- as.numeric(sub.net.manholes$cnt)+1
sub.net.manholes$weight <- rep(1,length(sub.net.manholes$cnt))
sub.net.manholes$indegree <- degree(sub.net,mode="in")
sub.net.edge.list <- as_edgelist(sub.net)

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
#main_stream_ind <- as.numeric(main_stream)
main_stream_ID <- attributes(main_stream)$names
main_stream_manholes <- sub.net.manholes[which(sub.net.manholes$ID %in% main_stream_ID),]
#main_stream_manholes <- main_stream_manholes[order(main_stream_manholes$cnt,decreasing = T),]
main_branch_cut <- min(main_stream_manholes$cnt[which(main_stream_manholes$cnt>100 & main_stream_manholes$indegree>1)])
main_stream_ID <- main_stream_manholes$ID[which(main_stream_manholes$cnt>=main_branch_cut)]
main_stream_sub_ind <- which(V(sub.net)$name %in% main_stream_ID)
main_stream_manholes <- main_stream_manholes[which(main_stream_manholes$cnt>=main_branch_cut),]

target.n.branch.sites <- target.n.sites - 1 #one site at the downstream end point and one site on the main stream;

#here start the for loop adapt;
for (k.iter in 1:17){
  b4merging.manholes <- sub.net.manholes[which(sub.net.manholes$ID %in% b4merging.points.ID),]
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
  
  selected_sites_ID <- c(k.site.ID,branch_selected_sites_ID)
  selected_sites_ind <- sapply(selected_sites_ID, FUN=function(x){which(V(sub.net)$name==x)})
  
  sub.net.size <- sub.net.manholes$weight
  sub.net.size[which(sub.net.manholes$ID %in% selected_sites_ID)] <- 8
  sub.net.col <- rep("gray",length(V(sub.net)))
  sub.net.col[main_stream_sub_ind] <- "black"
  sub.net.label <- rep(NA,length(V(sub.net)))
  for (i in 1:length(selected_sites_ID)){
    sub.net.label[which(sub.net.manholes$ID==selected_sites_ID[i])] <- round(sub.net.manholes$cnt[which(sub.net.manholes$ID==selected_sites_ID[i])],0)
  }
  selected_sites_ID_nodes <- list()
  selected_sites_ID_n_nodes <- c()
  for (i in 1:length(selected_sites_ID)){
    selected_sites_ID_nodes[[i]] <- c(which(V(sub.net)$name==selected_sites_ID[i]),get_upstream_nodes(sub.net,which(V(sub.net)$name==selected_sites_ID[i])))
    selected_sites_ID_n_nodes[i] <- length(selected_sites_ID_nodes[[i]])
  }
  
  for (i in order(selected_sites_ID_n_nodes,decreasing = T)[-1]){
    sub.net.col[selected_sites_ID_nodes[[i]]] <- i
  }
  #sub.net.col[which(sub.net.manholes$ID %in% selected_sites_ID)] <- "red"  
  # pdf(paste0("./v1/output/plots/init_map_",k.site.ID,"_",target.n.sites,".pdf"),width=20,height=20)
  
  #adapt the weight of the sites based on results;
  inf.mat <- t(mat.conc[k.day+7*(1:n.week),selected_sites_ind])>LLOD.test
  inf.mat[which(is.na(inf.mat))] <- 0
  sub.net.col[which(sub.net.manholes$ID %in% selected_sites_ID)] <- "blue"
  if (n.week==1){
    sub.net.col[which(sub.net.manholes$ID %in% colnames(inf.mat)[which(inf.mat==1)])] <- "red" 
  } else {
    sub.net.col[which(sub.net.manholes$ID %in% rownames(inf.mat)[which(rowSums(inf.mat)==n.week)])] <- "red"
    sub.net.col[which(sub.net.manholes$ID %in% rownames(inf.mat)[which(rowSums(inf.mat)<n.week & rowSums(inf.mat)>0)])] <- "orangered"
  }
  
  plot(sub.net, 
       vertex.size=sub.net.size, edge.arrow.size=0.5, vertex.color=sub.net.col, vertex.label=sub.net.label, 
       layout=as.matrix(nodes[sub.net.node.ind,2:3]))#vertex.label=paste0(V(net)$name[sub.net.node.ID],"\n",cnt.up.nodes[sub.net.node.ID]))
  title(paste0("Update ",k.update),cex.main=5)
  legend("bottomleft",legend=c("main stream","not covered","Negative Site","Positive Site"),col=c("black","gray","blue","red"),pch=16,cex=2)
  k.update=k.update+1

  k.day <- k.day + 7*n.week
  
  n.sites.branch <- length(branch_selected_sites_ID)
  n.sites.main <- target.n.sites - n.sites.branch
  
  weight.decrease <- c(rep(95/100,n.sites.main),rep(3/4,n.sites.branch))
  weight.increase <- c(rep(100/95,n.sites.main),rep(100/95,n.sites.branch))
  
  area_sites_control <- list()
  branch_control <- c()
  for (i in 1:n.sites.branch){
    branch_control <- c(branch_control,selected_sites_ID_nodes[[i+n.sites.main]])
  }
  area_sites_control[[1]] <- setdiff(1:n.points,branch_control)
  
  for (j in 1:n.sites.branch){
    area_sites_control[[j+n.sites.main]] <- selected_sites_ID_nodes[[j+n.sites.main]]
  }
  
  for (i in 1:length(inf.mat[1,])){
    if (sum(inf.mat[,i])>0){
      for (j in 1:length(inf.mat[,1])){
        if (inf.mat[j,i]==0){
          sub.net.manholes$weight[area_sites_control[[j]]] = sub.net.manholes$weight[area_sites_control[[j]]] * weight.decrease[j]
        }
        if (inf.mat[j,i]==1){
          sub.net.manholes$weight[area_sites_control[[j]]] = sub.net.manholes$weight[area_sites_control[[j]]] * weight.increase[j]
        }
      }
    }
  }
  
  #set upper and lower bound for the weights;
  sub.net.manholes$weight[which(sub.net.manholes$weight>(n.manhole/250))] <- n.manhole/250
  sub.net.manholes$weight[which(sub.net.manholes$weight<(n.manhole/2500))] <- n.manhole/2500
  
  sub.net.manholes$weight <- sub.net.manholes$weight/sum(sub.net.manholes$weight)*length(sub.net.manholes$weight)
  sub.net.manholes$cnt <- mat.connect.points %*% sub.net.manholes$weight
}
dev.off()

