###degree;
dat.manhole$indegree <- degree(net,mode = "in")
dat.manhole$outdegree <- degree(net,mode = "out")

indegree.size <- dat.manhole$indegree
indegree.size[which(indegree.size<=3)] <- 0.1
indegree.frame <- rep("gray",length(indegree.size))
indegree.frame[which(indegree.size>1)] <- "black"

outdegree.size <- dat.manhole$outdegree
outdegree.size[which(outdegree.size<=2)] <- 0.1
outdegree.frame <- rep("gray",length(outdegree.size))
outdegree.frame[which(outdegree.size>1)] <- "black"

pdf("./plots/degree_map_032022.pdf",width=18,height=6)
par(mfrow=c(1,3))
par(mar=c(1,1,1,1))
#plot(net,vertex.size=indegree.size,vertex.color="tomato",edge.arrow.size=0.001, vertex.label=NA,vertex.frame.color=indegree.frame,edge.width=0.2,edge.color ="gray",layout=as.matrix(nodes[,2:3]))
plot(net,vertex.shape = 'none',
     vertex.size = 0,
     vertex.label= NA,
     edge.arrow.size=0.01, 
     edge.width=1,
     edge.color ="gray",
     layout=as.matrix(nodes[,2:3]),
     xlim=range(nodes[,2]), ylim=range(nodes[,3]),
     rescale=FALSE)
idx <- which(indegree.size>0.1)
sub.ind.p <- induced_subgraph(net,idx)
plot(sub.ind.p,layout=as.matrix(nodes[idx,2:3]),
     edge.color="gray", 
     edge.arrow.size=0.01, 
     edge.width=1,
     vertex.label=NA,
     vertex.color="tomato", vertex.frame.color="black",
     vertex.size=indegree.size[idx]*25000,
     add=TRUE, xlim=range(nodes[,2]), ylim=range(nodes[,3]),
     rescale=FALSE)
legend(
  "topleft",
  legend = c("<=3","4","5","6","7","8","9","10"),
  pt.bg  = "tomato",
  pch    = 21,
  pt.cex = sort(unique(indegree.size))/4,
  bty    = "n",
  cex = 2,
  title="Indegree"
)
mtext("(a)",1,cex=2)

#plot(net,vertex.size=outdegree.size,vertex.color="gold",edge.arrow.size=0.001, vertex.label=NA,vertex.frame.color=outdegree.frame,edge.width=0.2,edge.color ="gray",layout=as.matrix(nodes[,2:3]))
plot(net,vertex.shape = 'none',
     vertex.size = 0,
     vertex.label= NA,
     edge.arrow.size=0.01, 
     edge.width=1,
     edge.color ="gray",
     layout=as.matrix(nodes[,2:3]),
     xlim=range(nodes[,2]), ylim=range(nodes[,3]),
     rescale=FALSE)
idx <- which(outdegree.size>0.1)
sub.out.p <- induced_subgraph(net,idx)
plot(sub.out.p,layout=as.matrix(nodes[idx,2:3]),
     edge.color="gray", 
     edge.arrow.size=0.01, 
     edge.width=1,
     vertex.label=NA,
     vertex.color="gold", vertex.frame.color="black",
     vertex.size=outdegree.size[idx]*50000,
     add=TRUE, xlim=range(nodes[,2]), ylim=range(nodes[,3]),
     rescale=FALSE)
legend(
  "topleft",
  legend = c("<=2","3","4","5"),
  pt.bg  = "gold",
  pch    = 21,
  pt.cex = sort(unique(outdegree.size))/2,
  bty    = "n",
  cex = 2,
  title="Outdegree"
)
mtext("(b)",1,cex=2)

plot(sewage_1,col ="gray")
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
  cex = 2
)
mtext("(c)",1,cex=2)
dev.off()




pdf("./plots/sub_net_topo.pdf",width=12,height=6)
par(mfrow=c(1,2))
par(mar=c(2,1,3,1))
k.site.ID="23370330601"
sub.net.node.ind <- sort(c(which(V(net)$name==k.site.ID), get_upstream_nodes(net,which(V(net)$name==k.site.ID))))
sub.net <- induced_subgraph(net,sub.net.node.ind)
plot(sub.net,vertex.size=1,vertex.color="tomato",edge.arrow.size=0.2, vertex.label=NA,
     layout=as.matrix(nodes[sub.net.node.ind,2:3]),
     main=paste0(k.site.ID,", avg indegree=",round(mean(degree(sub.net,mode = "in")[which(degree(sub.net,mode = "in")>0)]),2),
                 ", \navg outdegree=",round(mean(degree(sub.net,mode = "out")[which(degree(sub.net,mode = "out")>0)]),2)))
mtext("(a)",1,cex=2)

k.site.ID="23330211001"
sub.net.node.ind <- sort(c(which(V(net)$name==k.site.ID), get_upstream_nodes(net,which(V(net)$name==k.site.ID))))
sub.net <- induced_subgraph(net,sub.net.node.ind)
plot(sub.net,vertex.size=1,vertex.color="tomato",edge.arrow.size=0.2, vertex.label=NA,
     layout=as.matrix(nodes[sub.net.node.ind,2:3]),
     main=paste0(k.site.ID,", avg indegree=",round(mean(degree(sub.net,mode = "in")[which(degree(sub.net,mode = "in")>0)]),2),
                 ", \navg outdegree=",round(mean(degree(sub.net,mode = "out")[which(degree(sub.net,mode = "out")>0)]),2)))
mtext("(b)",1,cex=2)
dev.off()




dat.3$Type <- dat.3$location_type
com.3 <- dat.3 %>% filter(Collect_date >= '2021-03-30' & Collect_date <= "2022-04-15") %>% filter(!is.na(Type)) %>% filter(Type=="Community")
com.3$location <- as.character(com.3$location)
com.3$location <- factor(com.3$location, levels=c("1295 West Apartments","815 Old Flat Shoals Road","Belmonte Hills Townhomes",
                                                  "Cascade Commons","Cascade Glen Apartments","Columbia Tower at MLK Village",
                                                  "Country Oaks",
                                                  "Fairburn & Gordon II Apartments","Fairway Gardens Apartments","Gateway Capitol View",
                                                  "Heritage Station Apartments","Life at Greenbriar Apartments","Metropolitan Gardens Condominiums",
                                                  "Oakland Place Townhomes","Pavillion Place Apartments","Peyton Village Apartments",
                                                  "Venetian Hills Apartments","Veranda at Auburn Point","Victoria Place","Wildwood Townhomes",
                                                  "Atlanta Industrial Parkway","Cambridge Dr & Hogan Rd SW","Fair Street SW & Agnes Jones Place","Parsons St SW & Lawshe St SW",
                                                  "Peeples St SW & Cunningham Place","Rockwell St & Coleman St","Eloise St SE & Mercer St SE",
                                                  "Engelwood Ave SE & Boulevard SE","Pryor St & Richardson St",
                                                  "Butler Way NW","Sandy Creek","Spink St NW",
                                                  "Benjamin E Mays","Benjamin E Mays High School","Cascade Falls","Cascade Rd",
                                                  "Chatham Ave","Dodson Dr","Fairburn Rd","Forest Park Rd -- North Site","Forest Park Rd -- South Site",
                                                  "Guilford Forest Dr","Lilac Lane","Mt. Gilead Rd","Peyton Woods Trail","Plainville Trail",
                                                  "South Atlanta High School 1","South Atlanta High School 2","Ruby Harper Blvd","Southside Industrial Parkway",
                                                  "Village Dr","Walmart","Ivydale","Larchwood","Montgomery St and Woodbine Ave","Oakview Rd"
                                                  ))

p1 <- ggplot(data=com.3) +
  geom_point(aes(x=Collect_date, y=location, fill=positive), colour="black",shape=21,size=2.5) +
  scale_fill_manual(values=c("palegreen", "gold", "tomato")) +
  facet_grid(Type ~ ., scales = "free_y", space='free_y')
  #theme(legend.position="bottom") + xlab("collection date")
p2 <- ggplot(data = fulton_case %>% filter(report_date >= '2021-03-30' & report_date <= "2022-04-15")) +
  geom_bar(aes(x = report_date, y= cases),stat = "identity") + xlab("date") + 
  theme(text = element_text(size = 25))  
p3 <- grid.arrange(p1,p2,nrow = 2,heights = c(0.85, 0.15))

ggsave(paste0("./presentation/plots/res_all_",Sys.Date(),".pdf"),p3,height = 12,width = 12)
ggsave(paste0("./presentation/plots/res_all1_",Sys.Date(),".pdf"),p1,height = 9,width = 12)
ggsave(paste0("./presentation/plots/res_all2_",Sys.Date(),".pdf"),p2,height = 3,width = 12)

#school analysis
dat.sch <- dat.3[which(dat.3$location_type=="School" & !dat.3$location %in% c("Atlanta Metropolitan College","Humphries Elementary School","Sylvan Middle School")),]
#duplicated(dat.sch[,c("location","Collect_date")]) #no duplicated samples;
table(dat.sch$positive)


#school Cohen kappa
# Contingency table
xtab <- as.table(rbind(c(36, 27), c(23, 62)))
# Descriptive statistics
diagonal.counts <- diag(xtab)
N <- sum(xtab)
row.marginal.props <- rowSums(xtab)/N
col.marginal.props <- colSums(xtab)/N
# Compute kappa (k)
Po <- sum(diagonal.counts)/N
Pe <- sum(row.marginal.props*col.marginal.props)
k <- (Po - Pe)/(1 - Pe)
k




ipp <- readOGR("./data/shapefiles/IPP/","IPP")
ipp <- spTransform(ipp, CRS("+proj=longlat +datum=WGS84"))
ipp <-data.frame(ipp)

plot(sewage_1,col ="gray")
plot(ipp,add = TRUE)
legend(
  "topleft",
  legend = c("combined sewage","sewer line"),
  col = c(NA,"black"),
  fill = c(rgb(1, 99/255, 71/255, 0.5),NA),
  lty=c(NA,1),
  border = c(rgb(1, 99/255, 71/255, 0.5),NA), 
  lwd=1, 
  bty    = "n",
  cex = 2
)
