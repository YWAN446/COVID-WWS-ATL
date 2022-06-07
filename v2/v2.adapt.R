#Try the adapt steps;
k.update <- 0
pdf(paste0("./v2/output/plots/update_map_",k.site.ID,"_",target.n.sites,".pdf"),width=10,height=10)
k.site.ID <- "23070425401"
n.week=4
target.n.sites <- 5
sub.net.node.ind <- sort(c(which(V(net)$name==k.site.ID), get_upstream_nodes(net,which(V(net)$name==k.site.ID))))
sub.net <- subgraph(net,sub.net.node.ind)

#generate a high risk area;
risk.x <- 2206000
risk.y <- 1366000
#risk.y <- 1371000
risk.ind <- which((nodes[sub.net.node.ind,2]-risk.x)^2+(nodes[sub.net.node.ind,3]-risk.y)^2<500000)

my.col=rep("black",500)
my.col[risk.ind] <- "red"
plot(sub.net, 
     vertex.size=2, edge.arrow.size=0.5, vertex.color=my.col, vertex.label=NA, 
     layout=as.matrix(nodes[sub.net.node.ind,2:3]),main="risk area")#vertex.label=paste0(V(net)$name[sub.net.node.ID],"\n",cnt.up.nodes[sub.net.node.ID]))
risk.ID <- V(sub.net)$name[risk.ind]

#initialize;
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

#here start the for loop adapt;
for (k.iter in 1:9){
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
branch_cover_main_stream <- function(ID, sub.net, main_stream_ind){
  return(any(get_upstream_nodes(sub.net,which(V(sub.net)$name==ID)) %in% main_stream_ind))
}
branch_remove <- which(sapply(branch_manholes$ID,FUN = branch_cover_main_stream, sub.net=sub.net, main_stream_ind=main_stream_ind))
if (length(branch_remove)>0){
  branch_manholes <- branch_manholes[-branch_remove,]
}

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
selected_sites_ID <- c(k.site.ID,main_selected_sites_ID,branch_selected_sites_ID)

sub.net.size <- sub.net.manholes$weight
sub.net.size[which(sub.net.manholes$ID %in% selected_sites_ID)] <- 5
sub.net.col <- rep("black",length(V(sub.net)))
sub.net.col[which(sub.net.manholes$ID %in% selected_sites_ID)] <- "red"
sub.net.label <- rep(NA,length(V(sub.net)))
for (i in 1:length(selected_sites_ID)){
  sub.net.label[which(sub.net.manholes$ID==selected_sites_ID[i])] <- sub.net.manholes$cnt[which(sub.net.manholes$ID==selected_sites_ID[i])]
}

sub.net.col <- rep("black",length(V(sub.net)))
selected_sites_ID_nodes <- list()
selected_sites_ID_n_nodes <- c()
for (i in 1:length(selected_sites_ID)){
  selected_sites_ID_nodes[[i]] <- c(which(V(sub.net)$name==selected_sites_ID[i]),get_upstream_nodes(sub.net,which(V(sub.net)$name==selected_sites_ID[i])))
  selected_sites_ID_n_nodes[i] <- length(selected_sites_ID_nodes[[i]])
}

for (i in order(selected_sites_ID_n_nodes,decreasing = T)){
  sub.net.col[selected_sites_ID_nodes[[i]]] <- i
}
sub.net.col[which(sub.net.manholes$ID %in% selected_sites_ID)] <- length(selected_sites_ID_n_nodes)+1

# pdf(paste0("./v1/output/plots/init_map_",k.site.ID,"_",target.n.sites,".pdf"),width=20,height=20)
plot(sub.net, 
     vertex.size=sub.net.size, edge.arrow.size=0.5, vertex.color=sub.net.col, vertex.label=sub.net.label, 
     layout=as.matrix(nodes[sub.net.node.ind,2:3]),main=paste0("Update ",k.update))#vertex.label=paste0(V(net)$name[sub.net.node.ID],"\n",cnt.up.nodes[sub.net.node.ID]))
k.update=k.update+1

#adapt the weight of the sites based on results;
include.risk <- c()
for (i in 1:target.n.sites){
  include.risk[i] <- as.numeric(any(risk.ind %in% c(selected_sites_ID[i],get_upstream_nodes(sub.net,which(V(sub.net)$name==selected_sites_ID[i])))))
}
p <- 0.9*include.risk

inf.mat <- matrix(c(rbinom(n.week,size=1,prob=p[1]),
                    rbinom(n.week,size=1,prob=p[2]),
                    rbinom(n.week,size=1,prob=p[3]),
                    rbinom(n.week,size=1,prob=p[4]),
                    rbinom(n.week,size=1,prob=p[5])),
                  nrow=5,byrow=T)

n.sites.branch <- length(branch_selected_sites_ID)
n.sites.main <- target.n.sites - n.sites.branch

weight.decrease <- c(rep(0.8,n.sites.main),rep(0.6,n.sites.branch))

area_sites_control <- list()
branch_control <- c()
for (i in 1:n.sites.branch){
  branch_control <- c(branch_control,selected_sites_ID_nodes[[i+n.sites.main]])
}

for (i in 1:n.sites.main){
  if (i==1){
    area_sites_control[[i]] <- setdiff(selected_sites_ID_nodes[[i]],unique(c(selected_sites_ID_nodes[[i+1]],branch_control)))
  } else {
    area_sites_control[[i]] <- setdiff(selected_sites_ID_nodes[[i]],branch_control)
  }
}
for (j in 1:n.sites.branch){
  area_sites_control[[j+n.sites.main]] <- selected_sites_ID_nodes[[j+n.sites.main]]
}

for (i in 1:length(inf.mat[1,])){
  if (sum(inf.mat[,i])>0){
    for (j in 1:length(inf.mat[,1])){
      if (inf.mat[j,i]==0){
        sub.net.manholes$weight[area_sites_control[[j]]] = sub.net.manholes$weight[area_sites_control[[j]]] * weight.decrease[j]
      }
    }
  }
}

sub.net.manholes$weight <- sub.net.manholes$weight/sum(sub.net.manholes$weight)*length(sub.net.manholes$weight)
sub.net.manholes$cnt <- mat.connect.points %*% sub.net.manholes$weight
}
dev.off()

