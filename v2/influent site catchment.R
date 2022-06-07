cnt.up.nodes[which(V(net)$name=="13760400901")] #Phillip Lee Influent Line
cnt.up.nodes[which(V(net)$name=="13760401701")] #Old Winn Dixie Influent Line
cnt.up.nodes[which(V(net)$name=="13760400801")] #South Fulton Influent Line
cnt.up.nodes[which(V(net)$name=="23330211001")] # Jonesboro
cnt.up.nodes[which(V(net)$name=="23340422106")] # Flint
cnt.up.nodes[which(V(net)$name=="23330209701")] # Intrenchment

select.sites <- c(which(V(net)$name=="13760400901"),which(V(net)$name=="13760401701"),which(V(net)$name=="13760400801"),
                  which(V(net)$name=="23330211001"),which(V(net)$name=="23340422106"),which(V(net)$name=="23330209701"))

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
pdf(file=paste0("./presentation/plots/Influent.pdf"),height=40,width=40)
plot(net,vertex.size=size.select.sites, edge.arrow.size=0.1, vertex.label=label.select.sites,vertex.color=col.select.sites,vertex.frame.color=col.select.sites,layout=as.matrix(nodes[,2:3]))
legend("topleft",c("South Fulton","Phillip Lee","Old Winn Dixie","Jonesboro","Intrenchment","Flint"),col=c("gold","tomato","palegreen","hotpink","red2","cyan"),pch=16,title="Influent Site",cex=8)
#legend("topleft",c("Phillip Lee","Old Winn Dixie","South Fulton","Jonesboro","Flint","Intrenchment"),col=temp.col,pch=16,title="Influent Site",cex=8)
dev.off()

#### Maps ####
library(tidyverse)
library(sf)
library(maps)

states <- st_as_sf(maps::map(database = "state",plot=F,fill=T)) 
ga <- states %>% filter(ID=="georgia")

counties <- st_as_sf(maps::map("county", plot = FALSE, fill = TRUE))
counties <- subset(counties, grepl("georgia", counties$ID))
counties$ID <- toupper(str_remove(counties$ID, "georgia,"))
counties$ID <- str_remove(counties$ID, " ")

sub_counties <- counties %>% filter(ID %in% c("FULTON","DEKALB"))

ggplot() + geom_sf(data=sub_counties, fill=NA, color=gray(.5)) +
  geom_path(sewage_1,mapping = aes(x=long, y=lat, group = group))

