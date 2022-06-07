#COVID-19 wastewater surveillance in Atlanta;
#set direction
setwd("~/stat/nCoV_SewageSurveillance/")
#packages
library(reshape2)
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

sewage_1 <- spTransform(sewage1, CRS("+proj=longlat +datum=WGS84"))
sewage_2 <- spTransform(sewage2, CRS("+proj=longlat +datum=WGS84"))

#load some functions;
source("./code/lucy-master/R/misc-utilities.R")
source("./code/lucy-master/R/graph-plots.R")
source("./code/lucy-master/R/graph-sim.R")
source("./code/lucy-master/R/lucy.R")
source("./code/lucy-master/R/propagation.R")

#load sewerage network, manhole date, nodes, links and catchment manhole size;
load("./net.rda")
#read the geocoded data;
geocode.cases <- read.csv("./data/geocoded data/emoryGeocodeNCOV__20220510.csv",stringsAsFactors = F)
long.range <- range(dat.manhole$long)
lat.range <- range(dat.manhole$lat)
#subset the cases to the area;
geocode.cases <- geocode.cases[which(geocode.cases$RES_LON>=long.range[1] & geocode.cases$RES_LON<=long.range[2] &
                                       geocode.cases$RES_LAT>=lat.range[1] & geocode.cases$RES_LAT<=lat.range[2]),]

#read pui data;
pui.data <- read.csv("./data/geocoded data/pui_emory_extract_v2_202205081315.csv",stringsAsFactors = F)

select.sites <- c(which(V(net)$name=="13760400901"),which(V(net)$name=="13760401701"),which(V(net)$name=="13760400801"),
                  which(V(net)$name=="23330211001"),which(V(net)$name=="23340422106"),which(V(net)$name=="23330209701"),
                  which(V(net)$name=="23090400801"),which(V(net)$name=="23090402703"),which(V(net)$name=="23090401601"))

temp.col <- c("tomato","palegreen","gold","hotpink","cyan","red2","darkorchid1","seagreen","tan3")
site.names <- c("Phillip Lee","Old Winn Dixie","South Fulton","Jonesboro","Flint","Intrenchment","Nancy Creek","Proctor Creek","Peachtree Creek")
select.sites.ID <- V(net)$name[select.sites]
col.select.sites <- rep("black",length(V(net)))
for (i in 1:length(select.sites)){
  col.select.sites[c(which(V(net)$name==select.sites.ID[i]), get_upstream_nodes(net,which(V(net)$name==select.sites.ID[i])))] <- temp.col[i]
}

#ww.plots <- list()
ww.cases <- list()
#ww.conc <- list()

#pdf("./influent_sites_res_080321.pdf",width=12,height=8)
for (k.site in 1:9){
  selected_manholes <- dat.manhole[which(col.select.sites==temp.col[k.site]),]
  selected_catch_area <- data.frame(concaveman(as.matrix(selected_manholes[,c("long","lat")]),1))
  names(selected_catch_area) <- c("long","lat")
  
  selected_polygon <- selected_catch_area %>%
    st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")
  selected_polygon <- st_buffer(selected_polygon, dist = 0)
  
  sp.cases <- st_as_sf(geocode.cases, coords = c("RES_LON","RES_LAT"), crs = 4326)
  selected_cases <- st_intersection(sp.cases, selected_polygon)
  
  selected_cases <- merge(selected_cases,pui.data[,c("QARESPONSEID","Q174631","Q174656")],by="QARESPONSEID",all.x=T)
  names(selected_cases)[which(names(selected_cases)=="Q174631")] <- "report_date"
  names(selected_cases)[which(names(selected_cases)=="Q174656")] <- "symptom_onset"
  selected_cases$report_date <- as.Date(selected_cases$report_date,format = "%m/%d/%Y")
  selected_cases$symptom_onset <- as.Date(selected_cases$symptom_onset,format = "%m/%d/%Y")
  
  # selected_sewage <- sewage_1[which(sewage_1$UNITID %in% selected_manholes$UNITID & sewage_1$UNITID2 %in% selected_manholes$UNITID),]
  # 
  # p_epi_curve <- selected_cases %>% filter(report_date>=as.Date("03/15/2021",format = "%m/%d/%Y")) %>%
  #   ggplot() + geom_bar(aes(report_date), stat="count") + xlim(as.Date("03/15/2021",format = "%m/%d/%Y"),as.Date("08/25/2021",format = "%m/%d/%Y"))
  # 
  # p_map <- ggplot() + geom_sf(data = selected_polygon, fill="tomato") +
  #   geom_path(selected_sewage,mapping = aes(x=long, y=lat,group=group)) +
  #   geom_sf(data = selected_cases %>% filter(report_date>=as.Date("03/15/2021",format = "%m/%d/%Y")), fill=NA, color=gray(.5)) +
  #   ggtitle(site.names[k.site])
  # 
  # p_ww_res <- ggplot() + geom_point(data=influ.res[which(influ.res$location==site.names[k.site]),], aes(x=Collect_date, y=log_copies_per_L),size=3,col="orangered") + 
  #   geom_smooth(method = "loess") + xlab("Date") + ylab("log10 copies per L") + xlim(as.Date("03/15/2021",format = "%m/%d/%Y"),as.Date("08/25/2021",format = "%m/%d/%Y"))
  # #  + geom_point(data=dat.3 %>% filter(location=="Phillip Lee") %>% filter(!is.na(Type)),aes(x=Collect_date, y=location, col=positive),shape=15)
  # #ww.plots[[k.site]] <- grid.arrange(p_map,p_epi_curve,p_ww_res,nrow = 3,heights = c(0.6,0.2,0.2))
  # #ww.plots[[k.site]] <- p_map | (p_epi_curve / p_ww_res)
  ww.cases[[k.site]] <- selected_cases %>% filter(report_date>=as.Date("03/01/2021",format = "%m/%d/%Y"))
  # ww.conc[[k.site]] <- influ.res[which(influ.res$location==site.names[k.site]),]
  
  # p_map | (p_epi_curve / p_ww_res)
}
#dev.off()

cases <- list()
for (i in 1:9){
  temp1 <- data.frame(seq(as.Date("03/01/2021",format = "%m/%d/%Y"),as.Date("05/08/2022",format = "%m/%d/%Y"),by=1))
  names(temp1) <- "report_date"
  if (length(ww.cases[[i]]$report_date)>0){
    temp2 <- data.frame(table(ww.cases[[i]]$report_date))
    names(temp2) <- c("report_date","case")
    temp2$report_date <- as.Date(as.character(temp2$report_date),format="%Y-%m-%d")
    temp3 <- merge(temp1,temp2,by="report_date",all.x=T)
    temp3$case[is.na(temp3$case)] <- 0.5
    cases[[i]] <- temp3
  } else {
    temp1$case <- 0.5
    cases[[i]] <- temp1
  }
}

#influent_data <- read_excel('~/stat/students/Haisu Code summarization/Atlanta wastewater surveillance/Data updating/ALL-RESULTS_12.23.21.xlsx',sheet="All Influent")
influent_data <- read_excel('~/stat/students/Haisu Code summarization/Atlanta wastewater surveillance/Data updating/ALL-RESULTS_4.11.22.xlsx',sheet="All Influent")

influent_data$Concen_meth[which(influent_data$Concen_meth %in% c("memFiltration","Memfiltration","MemFiltration"))] <- "memFiltration"
influent_data$Concen_meth[which(influent_data$Concen_meth %in% c("NanoTrap","Nanotrap"))] <- "NanoTrap"

# influent_data$method <- ifelse(influent_data$Concen_meth == "memFiltration", 1,
#                               ifelse(influent_data$Concen_meth == "NanoTrap", 2,
#                                      ifelse(influent_data$Concen_meth == "Skimmed Milk", 3, 0)))

# influent_data <- subset(influent_data, method == 1 | method == 2 | method == 3)

### Plot Membrane Filtration Over Time at All Sites

#filter for membrane filtration data
# memfiltration_data <- subset(influent_data, method == 1)
memfiltration_data <- influent_data
memfiltration_data$non_detect_1 <- NA
memfiltration_data$non_detect_1[which(!is.na(memfiltration_data$avg_cp))] <- "1"
memfiltration_data$non_detect_1[which(memfiltration_data$avg_cp %in% c(5,10))] <- "16"
#memfiltration_data <- memfiltration_data[-which(is.na(memfiltration_data$non_detect_1)),]

memfiltration_data$Collect_date <- as.Date(memfiltration_data$Collect_date)

#Import case data
temp <- tempfile()
download.file("https://ga-covid19.ondemand.sas.com/docs/ga_covid_data.zip",temp)
fulton_case <- read.csv(unz(temp, "epicurve_rpt_date.csv"))
#fulton_case <- read.csv('Data updating/covid/epicurve_rpt_date.csv')
unlink(temp)
fulton_case <- fulton_case %>% filter(county %in% 'Fulton')
fulton_case <- fulton_case[,c('county','report_date','cases','cases')]
fulton_case$date <- as.Date(fulton_case$report_date)

#####################################################################
p1 <- ggplot() +
  geom_point(data=memfiltration_data %>% filter(Influent_Location %in% c("Phillip Lee")), aes(x = Collect_date, y = cp_100mL, shape = non_detect_1),alpha = 0.8, size = 2.5, color = "purple") +
  theme_linedraw() +
  scale_shape_manual(values = c(19, 1), guide = FALSE) +
  geom_smooth(data=memfiltration_data %>% filter(Influent_Location %in% c("Phillip Lee")),aes(x = Collect_date, y = cp_100mL), method="loess",color = "black", span = 0.3, se = TRUE) +
  geom_line(data = fulton_case, mapping=aes(x = date, y = cases*100),col="tomato") +
  geom_line(data = cases[[1]]%>%filter(case>0), mapping=aes(x = report_date, y = case*100), col="gold") +
  scale_y_continuous("SARS-CoV-2 conc (cp/100mL)", trans='log10', sec.axis = sec_axis(~ ./100, name = "reported cases")) +
  xlim(as.Date(c('3/20/21','05/08/22'), format="%m/%d/%y")) + 
  theme(legend.position = "none", axis.title.x = element_blank(),
        axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"), 
        axis.title.y.right = element_text(color = "red")) +
  ggtitle(expression("Phillip Lee, Spearman "~rho~" = 0.52"))

p2 <- ggplot() +
  geom_point(data=memfiltration_data %>% filter(Influent_Location %in% c("Old Winn Dixie")), aes(x = Collect_date, y = cp_100mL, shape = non_detect_1),alpha = 0.8, size = 2.5, color = "purple") +
  theme_linedraw() +
  scale_shape_manual(values = c(19, 1), guide = FALSE) +
  geom_smooth(data=memfiltration_data %>% filter(Influent_Location %in% c("Old Winn Dixie")),aes(x = Collect_date, y = cp_100mL), method="loess",color = "black", span = 0.3, se = TRUE) +
  geom_line(data = fulton_case, mapping=aes(x = date, y = cases*100),col="tomato") +
  geom_line(data = cases[[2]]%>%filter(case>0), mapping=aes(x = report_date, y = case*100), col="gold") +
  scale_y_continuous("SARS-CoV-2 conc (cp/100mL)", trans='log10', sec.axis = sec_axis(~ ./100, name = "reported cases")) +
  xlim(as.Date(c('3/20/21','05/08/22'), format="%m/%d/%y")) + 
  theme(legend.position = "none", axis.title.x = element_blank(),
        axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"), 
        axis.title.y.right = element_text(color = "red")) +
  ggtitle(expression("Old Winn Dixie, Spearman "~rho~" = 0.65"))

p3 <- ggplot() +
  geom_point(data=memfiltration_data %>% filter(Influent_Location %in% c("South Fulton")), aes(x = Collect_date, y = cp_100mL, shape = non_detect_1),alpha = 0.8, size = 2.5, color = "purple") +
  theme_linedraw() +
  scale_shape_manual(values = c(19, 1), guide = FALSE) +
  geom_smooth(data=memfiltration_data %>% filter(Influent_Location %in% c("South Fulton")),aes(x = Collect_date, y = cp_100mL), method="loess",color = "black", span = 0.3, se = TRUE) +
  geom_line(data = fulton_case, mapping=aes(x = date, y = cases*100),col="tomato") +
  geom_line(data = cases[[3]]%>%filter(case>0), mapping=aes(x = report_date, y = case*100), col="gold") +
  scale_y_continuous("SARS-CoV-2 conc (cp/100mL)", trans='log10', sec.axis = sec_axis(~ ./100, name = "reported cases")) +
  xlim(as.Date(c('3/20/21','05/08/22'), format="%m/%d/%y")) + 
  theme(legend.position = "none", axis.title.x = element_blank(),
        axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"), 
        axis.title.y.right = element_text(color = "red")) +
  ggtitle(expression("South Fulton, Spearman "~rho~" = 0.33"))

p4 <- ggplot() +
  geom_point(data=memfiltration_data %>% filter(Influent_Location %in% c("Jonesboro")), aes(x = Collect_date, y = cp_100mL, shape = non_detect_1),alpha = 0.8, size = 2.5, color = "purple") +
  theme_linedraw() +
  scale_shape_manual(values = c(19, 1), guide = FALSE) +
  geom_smooth(data=memfiltration_data %>% filter(Influent_Location %in% c("Jonesboro")),aes(x = Collect_date, y = cp_100mL), method="loess",color = "black", span = 0.3, se = TRUE) +
  geom_line(data = fulton_case, mapping=aes(x = date, y = cases*100),col="tomato") +
  geom_line(data = cases[[4]]%>%filter(case>0), mapping=aes(x = report_date, y = case*100), col="gold") +
  scale_y_continuous("SARS-CoV-2 conc (cp/100mL)", trans='log10', sec.axis = sec_axis(~ ./100, name = "reported cases")) +
  xlim(as.Date(c('3/20/21','05/08/22'), format="%m/%d/%y")) + 
  theme(legend.position = "none", axis.title.x = element_blank(),
        axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"), 
        axis.title.y.right = element_text(color = "red")) +
  ggtitle(expression("Jonesboro, Spearman "~rho~" = 0.57"))

p5 <- ggplot() +
  geom_point(data=memfiltration_data %>% filter(Influent_Location %in% c("Flint")), aes(x = Collect_date, y = cp_100mL, shape = non_detect_1),alpha = 0.8, size = 2.5, color = "purple") +
  theme_linedraw() +
  scale_shape_manual(values = c(19, 1), guide = FALSE) +
  geom_smooth(data=memfiltration_data %>% filter(Influent_Location %in% c("Flint")),aes(x = Collect_date, y = cp_100mL), method="loess",color = "black", span = 0.3, se = TRUE) +
  geom_line(data = fulton_case, mapping=aes(x = date, y = cases*100),col="tomato") +
  geom_line(data = cases[[5]]%>%filter(case>0), mapping=aes(x = report_date, y = case*100), col="gold") +
  scale_y_continuous("SARS-CoV-2 conc (cp/100mL)", trans='log10', sec.axis = sec_axis(~ ./100, name = "reported cases")) +
  xlim(as.Date(c('3/20/21','05/08/22'), format="%m/%d/%y")) + 
  theme(legend.position = "none", axis.title.x = element_blank(),
        axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"), 
        axis.title.y.right = element_text(color = "red"))+
  ggtitle(expression("Flint, Spearman "~rho~" = 0.63"))


p6 <- ggplot() +
  geom_point(data=memfiltration_data %>% filter(Influent_Location %in% c("Intrenchment")), aes(x = Collect_date, y = cp_100mL, shape = non_detect_1),alpha = 0.8, size = 2.5, color = "purple") +
  theme_linedraw() +
  scale_shape_manual(values = c(19, 1), guide = FALSE) +
  geom_smooth(data=memfiltration_data %>% filter(Influent_Location %in% c("Intrenchment")),aes(x = Collect_date, y = cp_100mL), method="loess",color = "black", span = 0.3, se = TRUE) +
  geom_line(data = fulton_case, mapping=aes(x = date, y = cases*100),col="tomato") +
  geom_line(data = cases[[6]]%>%filter(case>0), mapping=aes(x = report_date, y = case*100), col="gold") +
  scale_y_continuous("SARS-CoV-2 conc (cp/100mL)", trans='log10', sec.axis = sec_axis(~ ./100, name = "reported cases")) +
  xlim(as.Date(c('3/20/21','05/08/22'), format="%m/%d/%y")) + 
  theme(legend.position = "none", axis.title.x = element_blank(),
        axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"), 
        axis.title.y.right = element_text(color = "red")) +
  ggtitle(expression("Intrenchment, Spearman "~rho~" = 0.57"))

p7 <- ggplot() +
  geom_point(data=memfiltration_data %>% filter(Influent_Location %in% c("Nancy Creek")), aes(x = Collect_date, y = cp_100mL, shape = non_detect_1),alpha = 0.8, size = 2.5, color = "purple") +
  theme_linedraw() +
  scale_shape_manual(values = c(19, 1), guide = FALSE) +
  geom_smooth(data=memfiltration_data %>% filter(Influent_Location %in% c("Nancy Creek")),aes(x = Collect_date, y = cp_100mL), method="loess",color = "black", span = 1, se = TRUE) +
  geom_line(data = fulton_case, mapping=aes(x = date, y = cases*100),col="tomato") +
  geom_line(data = cases[[7]]%>%filter(case>0), mapping=aes(x = report_date, y = case*100), col="gold") +
  scale_y_continuous("SARS-CoV-2 conc (cp/100mL)", trans='log10', sec.axis = sec_axis(~ ./100, name = "reported cases")) +
  xlim(as.Date(c('3/20/21','05/08/22'), format="%m/%d/%y")) + 
  theme(legend.position = "none", axis.title.x = element_blank(),
        axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"), 
        axis.title.y.right = element_text(color = "red")) +
  ggtitle(expression("Nancy Creek, Spearman "~rho~" = 0.60"))

p8 <- ggplot() +
  geom_point(data=memfiltration_data %>% filter(Influent_Location %in% c("Proctor Creek")), aes(x = Collect_date, y = cp_100mL, shape = non_detect_1),alpha = 0.8, size = 2.5, color = "purple") +
  theme_linedraw() +
  scale_shape_manual(values = c(19, 1), guide = FALSE) +
  geom_smooth(data=memfiltration_data %>% filter(Influent_Location %in% c("Proctor Creek")),aes(x = Collect_date, y = cp_100mL), method="loess",color = "black", span = 1, se = TRUE) +
  geom_line(data = fulton_case, mapping=aes(x = date, y = cases*100),col="tomato") +
  geom_line(data = cases[[8]]%>%filter(case>0), mapping=aes(x = report_date, y = case*100), col="gold") +
  scale_y_continuous("SARS-CoV-2 conc (cp/100mL)", trans='log10', sec.axis = sec_axis(~ ./100, name = "reported cases")) +
  xlim(as.Date(c('3/20/21','05/08/22'), format="%m/%d/%y")) + 
  theme(legend.position = "none", axis.title.x = element_blank(),
        axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"), 
        axis.title.y.right = element_text(color = "red")) +
  ggtitle(expression("Proctor Creek, Spearman "~rho~" = 0.48"))

p9 <- ggplot() +
  geom_point(data=memfiltration_data %>% filter(Influent_Location %in% c("Peachtree Creek")), aes(x = Collect_date, y = cp_100mL, shape = non_detect_1),alpha = 0.8, size = 2.5, color = "purple") +
  theme_linedraw() +
  scale_shape_manual(values = c(19, 1), guide = FALSE) +
  geom_smooth(data=memfiltration_data %>% filter(Influent_Location %in% c("Peachtree Creek")),aes(x = Collect_date, y = cp_100mL), method="loess",color = "black", span = 1, se = TRUE) +
  geom_line(data = fulton_case, mapping=aes(x = date, y = cases*100),col="tomato") +
  geom_line(data = cases[[9]]%>%filter(case>0), mapping=aes(x = report_date, y = case*100), col="gold") +
  scale_y_continuous("SARS-CoV-2 conc (cp/100mL)", trans='log10', sec.axis = sec_axis(~ ./100, name = "reported cases")) +
  xlim(as.Date(c('3/20/21','05/08/22'), format="%m/%d/%y")) + 
  theme(legend.position = "none", axis.title.x = element_blank(),
        axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"), 
        axis.title.y.right = element_text(color = "red")) +
  ggtitle(expression("Peachtree Creek, Spearman "~rho~" = 0.63"))

p <- (p1 | p2 | p3) / (p4 | p5 | p6) / (p7 | p8 | p9)
ggsave("./plots/influ_conc_case.pdf",p,width=12,height=9)

#####################################################################
#GAM with confidence interval;
p1 <- ggplot() +
  geom_point(data=memfiltration_data %>% filter(Influent_Location %in% c("Phillip Lee")), aes(x = Collect_date, y = cp_100mL, shape = non_detect_1),alpha = 0.8, size = 2.5, color = "purple") +
  theme_linedraw() +
  scale_shape_manual(values = c(19, 1), guide = FALSE) +
  geom_smooth(data=memfiltration_data %>% filter(Influent_Location %in% c("Phillip Lee")),aes(x = Collect_date, y = cp_100mL), method="gam",color = "black", span = 0.3, se = TRUE) +
  geom_line(data = fulton_case, mapping=aes(x = date, y = cases*100),col="tomato") +
  geom_line(data = cases[[1]]%>%filter(case>0), mapping=aes(x = report_date, y = case*100), col="gold") +
  scale_y_continuous("SARS-CoV-2 conc (cp/100mL)", trans='log10', sec.axis = sec_axis(~ ./100, name = "reported cases")) +
  xlim(as.Date(c('3/20/21','05/08/22'), format="%m/%d/%y")) + 
  theme(legend.position = "none", axis.title.x = element_blank(),
        axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"), 
        axis.title.y.right = element_text(color = "red")) +
  ggtitle(expression("Phillip Lee, Spearman "~rho~" = 0.52"))

p2 <- ggplot() +
  geom_point(data=memfiltration_data %>% filter(Influent_Location %in% c("Old Winn Dixie")), aes(x = Collect_date, y = cp_100mL, shape = non_detect_1),alpha = 0.8, size = 2.5, color = "purple") +
  theme_linedraw() +
  scale_shape_manual(values = c(19, 1), guide = FALSE) +
  geom_smooth(data=memfiltration_data %>% filter(Influent_Location %in% c("Old Winn Dixie")),aes(x = Collect_date, y = cp_100mL), method="gam",color = "black", span = 0.3, se = TRUE) +
  geom_line(data = fulton_case, mapping=aes(x = date, y = cases*100),col="tomato") +
  geom_line(data = cases[[2]]%>%filter(case>0), mapping=aes(x = report_date, y = case*100), col="gold") +
  scale_y_continuous("SARS-CoV-2 conc (cp/100mL)", trans='log10', sec.axis = sec_axis(~ ./100, name = "reported cases")) +
  xlim(as.Date(c('3/20/21','05/08/22'), format="%m/%d/%y")) + 
  theme(legend.position = "none", axis.title.x = element_blank(),
        axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"), 
        axis.title.y.right = element_text(color = "red")) +
  ggtitle(expression("Old Winn Dixie, Spearman "~rho~" = 0.65"))

p3 <- ggplot() +
  geom_point(data=memfiltration_data %>% filter(Influent_Location %in% c("South Fulton")), aes(x = Collect_date, y = cp_100mL, shape = non_detect_1),alpha = 0.8, size = 2.5, color = "purple") +
  theme_linedraw() +
  scale_shape_manual(values = c(19, 1), guide = FALSE) +
  geom_smooth(data=memfiltration_data %>% filter(Influent_Location %in% c("South Fulton")),aes(x = Collect_date, y = cp_100mL), method="gam",color = "black", span = 0.3, se = TRUE) +
  geom_line(data = fulton_case, mapping=aes(x = date, y = cases*100),col="tomato") +
  geom_line(data = cases[[3]]%>%filter(case>0), mapping=aes(x = report_date, y = case*100), col="gold") +
  scale_y_continuous("SARS-CoV-2 conc (cp/100mL)", trans='log10', sec.axis = sec_axis(~ ./100, name = "reported cases")) +
  xlim(as.Date(c('3/20/21','05/08/22'), format="%m/%d/%y")) + 
  theme(legend.position = "none", axis.title.x = element_blank(),
        axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"), 
        axis.title.y.right = element_text(color = "red")) +
  ggtitle(expression("South Fulton, Spearman "~rho~" = 0.33"))

p4 <- ggplot() +
  geom_point(data=memfiltration_data %>% filter(Influent_Location %in% c("Jonesboro")), aes(x = Collect_date, y = cp_100mL, shape = non_detect_1),alpha = 0.8, size = 2.5, color = "purple") +
  theme_linedraw() +
  scale_shape_manual(values = c(19, 1), guide = FALSE) +
  geom_smooth(data=memfiltration_data %>% filter(Influent_Location %in% c("Jonesboro")),aes(x = Collect_date, y = cp_100mL), method="gam",color = "black", span = 0.3, se = TRUE) +
  geom_line(data = fulton_case, mapping=aes(x = date, y = cases*100),col="tomato") +
  geom_line(data = cases[[4]]%>%filter(case>0), mapping=aes(x = report_date, y = case*100), col="gold") +
  scale_y_continuous("SARS-CoV-2 conc (cp/100mL)", trans='log10', sec.axis = sec_axis(~ ./100, name = "reported cases")) +
  xlim(as.Date(c('3/20/21','05/08/22'), format="%m/%d/%y")) + 
  theme(legend.position = "none", axis.title.x = element_blank(),
        axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"), 
        axis.title.y.right = element_text(color = "red")) +
  ggtitle(expression("Jonesboro, Spearman "~rho~" = 0.57"))

p5 <- ggplot() +
  geom_point(data=memfiltration_data %>% filter(Influent_Location %in% c("Flint")), aes(x = Collect_date, y = cp_100mL, shape = non_detect_1),alpha = 0.8, size = 2.5, color = "purple") +
  theme_linedraw() +
  scale_shape_manual(values = c(19, 1), guide = FALSE) +
  geom_smooth(data=memfiltration_data %>% filter(Influent_Location %in% c("Flint")),aes(x = Collect_date, y = cp_100mL), method="gam",color = "black", span = 0.3, se = TRUE) +
  geom_line(data = fulton_case, mapping=aes(x = date, y = cases*100),col="tomato") +
  geom_line(data = cases[[5]]%>%filter(case>0), mapping=aes(x = report_date, y = case*100), col="gold") +
  scale_y_continuous("SARS-CoV-2 conc (cp/100mL)", trans='log10', sec.axis = sec_axis(~ ./100, name = "reported cases")) +
  xlim(as.Date(c('3/20/21','05/08/22'), format="%m/%d/%y")) + 
  theme(legend.position = "none", axis.title.x = element_blank(),
        axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"), 
        axis.title.y.right = element_text(color = "red"))+
  ggtitle(expression("Flint, Spearman "~rho~" = 0.63"))


p6 <- ggplot() +
  geom_point(data=memfiltration_data %>% filter(Influent_Location %in% c("Intrenchment")), aes(x = Collect_date, y = cp_100mL, shape = non_detect_1),alpha = 0.8, size = 2.5, color = "purple") +
  theme_linedraw() +
  scale_shape_manual(values = c(19, 1), guide = FALSE) +
  geom_smooth(data=memfiltration_data %>% filter(Influent_Location %in% c("Intrenchment")),aes(x = Collect_date, y = cp_100mL), method="gam",color = "black", span = 0.3, se = TRUE) +
  geom_line(data = fulton_case, mapping=aes(x = date, y = cases*100),col="tomato") +
  geom_line(data = cases[[6]]%>%filter(case>0), mapping=aes(x = report_date, y = case*100), col="gold") +
  scale_y_continuous("SARS-CoV-2 conc (cp/100mL)", trans='log10', sec.axis = sec_axis(~ ./100, name = "reported cases")) +
  xlim(as.Date(c('3/20/21','05/08/22'), format="%m/%d/%y")) + 
  theme(legend.position = "none", axis.title.x = element_blank(),
        axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"), 
        axis.title.y.right = element_text(color = "red")) +
  ggtitle(expression("Intrenchment, Spearman "~rho~" = 0.57"))

p7 <- ggplot() +
  geom_point(data=memfiltration_data %>% filter(Influent_Location %in% c("Nancy Creek")), aes(x = Collect_date, y = cp_100mL, shape = non_detect_1),alpha = 0.8, size = 2.5, color = "purple") +
  theme_linedraw() +
  scale_shape_manual(values = c(19, 1), guide = FALSE) +
  geom_smooth(data=memfiltration_data %>% filter(Influent_Location %in% c("Nancy Creek")),aes(x = Collect_date, y = cp_100mL), method="gam",color = "black", span = 1, se = TRUE) +
  geom_line(data = fulton_case, mapping=aes(x = date, y = cases*100),col="tomato") +
  geom_line(data = cases[[7]]%>%filter(case>0), mapping=aes(x = report_date, y = case*100), col="gold") +
  scale_y_continuous("SARS-CoV-2 conc (cp/100mL)", trans='log10', sec.axis = sec_axis(~ ./100, name = "reported cases")) +
  xlim(as.Date(c('3/20/21','05/08/22'), format="%m/%d/%y")) + 
  theme(legend.position = "none", axis.title.x = element_blank(),
        axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"), 
        axis.title.y.right = element_text(color = "red")) +
  ggtitle(expression("Nancy Creek, Spearman "~rho~" = 0.60"))

p8 <- ggplot() +
  geom_point(data=memfiltration_data %>% filter(Influent_Location %in% c("Proctor Creek")), aes(x = Collect_date, y = cp_100mL, shape = non_detect_1),alpha = 0.8, size = 2.5, color = "purple") +
  theme_linedraw() +
  scale_shape_manual(values = c(19, 1), guide = FALSE) +
  geom_smooth(data=memfiltration_data %>% filter(Influent_Location %in% c("Proctor Creek")),aes(x = Collect_date, y = cp_100mL), method="gam",color = "black", span = 1, se = TRUE) +
  geom_line(data = fulton_case, mapping=aes(x = date, y = cases*100),col="tomato") +
  geom_line(data = cases[[8]]%>%filter(case>0), mapping=aes(x = report_date, y = case*100), col="gold") +
  scale_y_continuous("SARS-CoV-2 conc (cp/100mL)", trans='log10', sec.axis = sec_axis(~ ./100, name = "reported cases")) +
  xlim(as.Date(c('3/20/21','05/08/22'), format="%m/%d/%y")) + 
  theme(legend.position = "none", axis.title.x = element_blank(),
        axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"), 
        axis.title.y.right = element_text(color = "red")) +
  ggtitle(expression("Proctor Creek, Spearman "~rho~" = 0.48"))

p9 <- ggplot() +
  geom_point(data=memfiltration_data %>% filter(Influent_Location %in% c("Peachtree Creek")), aes(x = Collect_date, y = cp_100mL, shape = non_detect_1),alpha = 0.8, size = 2.5, color = "purple") +
  theme_linedraw() +
  scale_shape_manual(values = c(19, 1), guide = FALSE) +
  geom_smooth(data=memfiltration_data %>% filter(Influent_Location %in% c("Peachtree Creek")),aes(x = Collect_date, y = cp_100mL), method="gam",color = "black", span = 1, se = TRUE) +
  geom_line(data = fulton_case, mapping=aes(x = date, y = cases*100),col="tomato") +
  geom_line(data = cases[[9]]%>%filter(case>0), mapping=aes(x = report_date, y = case*100), col="gold") +
  scale_y_continuous("SARS-CoV-2 conc (cp/100mL)", trans='log10', sec.axis = sec_axis(~ ./100, name = "reported cases")) +
  xlim(as.Date(c('3/20/21','05/08/22'), format="%m/%d/%y")) + 
  theme(legend.position = "none", axis.title.x = element_blank(),
        axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"), 
        axis.title.y.right = element_text(color = "red")) +
  ggtitle(expression("Peachtree Creek, Spearman "~rho~" = 0.63"))

p <- (p1 | p2 | p3) / (p4 | p5 | p6) / (p7 | p8 | p9)
ggsave("./plots/influ_conc_case_gam.pdf",p,width=12,height=9)

#####################################################################

site.name <- c("Phillip Lee","Old Winn Dixie","South Fulton","Jonesboro","Flint","Intrenchment","Nancy Creek","Proctor Creek","Peachtree Creek")
for (i in 1:9){
  dat.ww <- memfiltration_data %>% filter(Influent_Location %in% site.name[i])
  c.ww <- log10(dat.ww$cp_100mL)
  t.waste <- dat.ww$Collect_date
  t.ref <- min(t.waste);
  t.ww <- as.numeric(difftime(t.waste,t.ref,units="days"));
  
  n.cs <- log10(fulton_case$cases)
  t.case <- fulton_case$date
  t.cs <- as.numeric(difftime(t.case,t.ref,units="days"));
  n.cs <- n.cs[which(t.cs>=0)]
  t.cs <- t.cs[which(t.cs>=0)]
  
  n.cs.sub <- log10(cases[[i]]$case)
  t.case.sub <- cases[[i]]$report_date
  t.cs.sub <- as.numeric(difftime(t.case.sub,t.ref,units="days"));
  n.cs.sub <- n.cs.sub[which(t.cs.sub>=0)]
  t.cs.sub <- t.cs.sub[which(t.cs.sub>=0)]
  
  #cs.lo <- loess(n.cs~t.cs,as.data.frame(cbind(t.cs,n.cs)),span=0.25,
  #               control=loess.control(surface="direct"));
  ww.lo <- loess(c.ww~t.ww,as.data.frame(cbind(t.ww,c.ww)),span=0.3,
                 control=loess.control(surface="direct"));
  
  cs.smooth <- n.cs
  cs.sub.smooth <- n.cs.sub[1:length(n.cs)]
  #cs.smooth <- predict(cs.lo,data.frame(t.cs=t.cs));
  ww.smooth <- predict(ww.lo,data.frame(t.ww=t.cs));
  
  print(paste0("Site: ",site.name[i],"; Pearson: ",cor(cs.smooth,ww.smooth),"; Spearman: ",cor(cs.smooth,ww.smooth,method = "spearman"),"; Kandall: ",cor(cs.smooth,ww.smooth,method = "kendall")))
  #print(paste0("Site: ",site.name[i],"; Pearson: ",cor(n.cs[t.ww+1],c.ww),"; Spearman: ",cor(n.cs[t.ww+1],c.ww,method = "spearman"),"; Kandall: ",cor(n.cs[t.ww+1],c.ww,method = "kendall")))
  print(paste0("Site: ",site.name[i],"; Pearson: ",cor(cs.sub.smooth,ww.smooth[1:length(cs.sub.smooth)]),"; Spearman: ",cor(cs.sub.smooth,ww.smooth[1:length(cs.sub.smooth)],method = "spearman"),"; Kandall: ",cor(cs.sub.smooth,ww.smooth[1:length(cs.sub.smooth)],method = "kendall")))
  #print(paste0("Site: ",site.name[i],"; Pearson: ",cor(n.cs.sub[t.ww+1][which(!is.na(n.cs.sub[t.ww+1]))],c.ww[which(!is.na(n.cs.sub[t.ww+1]))]),"; Spearman: ",cor(n.cs.sub[t.ww+1][which(!is.na(n.cs.sub[t.ww+1]))],c.ww[which(!is.na(n.cs.sub[t.ww+1]))],method = "spearman"),"; Kandall: ",cor(n.cs.sub[t.ww+1][which(!is.na(n.cs.sub[t.ww+1]))],c.ww[which(!is.na(n.cs.sub[t.ww+1]))],method = "kendall")))
  
  
  # pdf(paste0("./plots/smoothed_detrend_",site.name[i],".pdf"));
  # par(mfrow=c(3,1),mar=c(4,4,1,0.2));
  # plot(t.cs[-1],diff(cs.smooth),"l",main=site.name[i]);
  # points(t.cs,n.cs);
  # plot(t.cs[-1],diff(ww.smooth),"l");
  # points(t.ww,c.ww);
  # ccf(diff(ww.smooth),diff(cs.smooth),type="correlation");
  # dev.off();
}

#correlation analysis with smoothed lines with cases in fulton county;
site.name <- c("Phillip Lee","Old Winn Dixie","South Fulton","Jonesboro","Flint","Intrenchment","Nancy Creek","Proctor Creek","Peachtree Creek")
cor.res <- rep(NA,5)
for (i in 1:9){
  for (j in -7:14){
    dat.ww <- memfiltration_data %>% filter(Influent_Location %in% site.name[i]) %>% filter(!is.na(cp_100mL))
    c.ww <- log10(dat.ww$cp_100mL)
    t.waste <- dat.ww$Collect_date
    t.ref <- min(t.waste);
    t.ref.cs <- min(t.waste)+j;
    t.ww <- as.numeric(difftime(t.waste,t.ref,units="days"));
    
    ww.lo <- loess(c.ww~t.ww,as.data.frame(cbind(t.ww,c.ww)),span=0.3,
                   control=loess.control(surface="direct"));
    
    n.cs <- log10(fulton_case$cases)
    t.case <- fulton_case$date
    t.cs <- as.numeric(difftime(t.case,t.ref.cs,units="days"));
    if (j<0){
      n.cs <- n.cs[which(t.cs>=0)][1:(length(n.cs[which(t.cs>=0)])+j)]
      t.cs <- t.cs[which(t.cs>=0)][1:(length(t.cs[which(t.cs>=0)])+j)]
    } else {
      n.cs <- n.cs[which(t.cs>=0)]
      t.cs <- t.cs[which(t.cs>=0)]
    }
    ww.smooth <- predict(ww.lo,data.frame(t.ww=t.cs));
    cor.res <- rbind(cor.res,c(site.name[i],j,cor(n.cs,ww.smooth),cor(n.cs,ww.smooth,method = "spearman"),cor(n.cs,ww.smooth,method = "kendall")))
    #print(paste0("Site: ",site.name[i],", Delay: ",j-1,"; Pearson: ",cor(n.cs[t.ww+j],c.ww),"; Spearman: ",cor(n.cs[t.ww+j],c.ww,method = "spearman"),"; Kandall: ",cor(n.cs[t.ww+j],c.ww,method = "kendall")))
  }
}

cor.res <- data.frame(cor.res[-1,])
names(cor.res) <- c("site","case_delay","pearson","spearman","kendall")
cor.res$case_delay <- as.numeric(cor.res$case_delay)
cor.res$pearson <- as.numeric(cor.res$pearson)
cor.res$spearman <- as.numeric(cor.res$spearman)
cor.res$kendall <- as.numeric(cor.res$kendall)

cor_p1 <- ggplot(cor.res) + geom_line(aes(x=case_delay,y=pearson,col=site)) + theme(legend.position="none") + xlab("Delay of case reported (days)") + ylab("Pearson correlation")
cor_p2 <- ggplot(cor.res) + geom_line(aes(x=case_delay,y=spearman,col=site)) + theme(legend.position="none") + xlab("Delay of case reported (days)") + ylab("Spearman correlation")
cor_p3 <- ggplot(cor.res) + geom_line(aes(x=case_delay,y=kendall,col=site)) + theme(legend.position="right") + xlab("Delay of case reported (days)") + ylab("Kendall correlation")

cor_p <- cor_p1 | cor_p2 | cor_p3

ggsave(paste0("./plots/cor_delay_smooth_Fulton_",Sys.Date(),".pdf"),cor_p,width=12,height=8)

#correlation analysis with smoothed lines with cases in catchment area;
site.name <- c("Phillip Lee","Old Winn Dixie","South Fulton","Jonesboro","Flint","Intrenchment","Nancy Creek","Proctor Creek","Peachtree Creek")
cor.res <- rep(NA,5)
for (i in 1:9){
  for (j in -7:14){
    dat.ww <- memfiltration_data %>% filter(Influent_Location %in% site.name[i]) %>% filter(!is.na(cp_100mL))
    c.ww <- log10(dat.ww$cp_100mL)
    t.waste <- dat.ww$Collect_date
    t.ref <- min(t.waste);
    t.ref.cs <- min(t.waste)+j;
    t.ww <- as.numeric(difftime(t.waste,t.ref,units="days"));
    
    ww.lo <- loess(c.ww~t.ww,as.data.frame(cbind(t.ww,c.ww)),span=0.3,
                   control=loess.control(surface="direct"));
    
    n.cs <- log10(cases[[i]]$case)
    t.case <- cases[[i]]$report_date
    t.cs <- as.numeric(difftime(t.case,t.ref.cs,units="days"));
    if (j<0){
      n.cs <- n.cs[which(t.cs>=0)][1:(length(n.cs[which(t.cs>=0)])+j)]
      t.cs <- t.cs[which(t.cs>=0)][1:(length(t.cs[which(t.cs>=0)])+j)]
    } else {
      n.cs <- n.cs[which(t.cs>=0)]
      t.cs <- t.cs[which(t.cs>=0)]
    }
    ww.smooth <- predict(ww.lo,data.frame(t.ww=t.cs));
    cor.res <- rbind(cor.res,c(site.name[i],j,cor(n.cs,ww.smooth),cor(n.cs,ww.smooth,method = "spearman"),cor(n.cs,ww.smooth,method = "kendall")))
    #print(paste0("Site: ",site.name[i],", Delay: ",j-1,"; Pearson: ",cor(n.cs[t.ww+j],c.ww),"; Spearman: ",cor(n.cs[t.ww+j],c.ww,method = "spearman"),"; Kandall: ",cor(n.cs[t.ww+j],c.ww,method = "kendall")))
  }
}

cor.res <- data.frame(cor.res[-1,])
names(cor.res) <- c("site","case_delay","pearson","spearman","kendall")
cor.res$case_delay <- as.numeric(cor.res$case_delay)
cor.res$pearson <- as.numeric(cor.res$pearson)
cor.res$spearman <- as.numeric(cor.res$spearman)
cor.res$kendall <- as.numeric(cor.res$kendall)

cor.res <- cor.res[-which(cor.res$site=="Old Winn Dixie"),]

cor_p1 <- ggplot(cor.res) + geom_line(aes(x=case_delay,y=pearson,col=site)) + theme(legend.position="none") + xlab("Delay of case reported (days)") + ylab("Pearson correlation")
cor_p2 <- ggplot(cor.res) + geom_line(aes(x=case_delay,y=spearman,col=site)) + theme(legend.position="none") + xlab("Delay of case reported (days)") + ylab("Spearman correlation")
cor_p3 <- ggplot(cor.res) + geom_line(aes(x=case_delay,y=kendall,col=site)) + theme(legend.position="right") + xlab("Delay of case reported (days)") + ylab("Kendall correlation")

cor_p <- cor_p1 | cor_p2 | cor_p3

ggsave(paste0("./plots/cor_delay_smooth_catchment_",Sys.Date(),".pdf"),cor_p,width=12,height=8)

#correlation delayed with points;
site.name <- c("Phillip Lee","Old Winn Dixie","South Fulton","Jonesboro","Flint","Intrenchment","Nancy Creek","Proctor Creek","Peachtree Creek")
cor.res <- rep(NA,5)
for (i in 1:9){
  dat.ww <- memfiltration_data %>% filter(Influent_Location %in% site.name[i]) %>% filter(!is.na(cp_100mL))
  c.ww <- log10(dat.ww$cp_100mL)
  t.waste <- dat.ww$Collect_date
  t.ref <- min(t.waste)-14;
  t.ww <- as.numeric(difftime(t.waste,t.ref,units="days"));
  
  n.cs <- log10(fulton_case$cases)
  t.case <- fulton_case$date
  t.cs <- as.numeric(difftime(t.case,t.ref,units="days"));
  n.cs <- n.cs[which(t.cs>=0)]
  t.cs <- t.cs[which(t.cs>=0)]
  
  for (j in -6:11){
    cor.res <- rbind(cor.res,c(site.name[i],j-1,cor(n.cs[t.ww+j],c.ww),cor(n.cs[t.ww+j],c.ww,method = "spearman"),cor(n.cs[t.ww+j],c.ww,method = "kendall")))
    #print(paste0("Site: ",site.name[i],", Delay: ",j-1,"; Pearson: ",cor(n.cs[t.ww+j],c.ww),"; Spearman: ",cor(n.cs[t.ww+j],c.ww,method = "spearman"),"; Kandall: ",cor(n.cs[t.ww+j],c.ww,method = "kendall")))
  }
}

cor.res <- data.frame(cor.res[-1,])
names(cor.res) <- c("site","case_delay","pearson","spearman","kendall")
cor.res$case_delay <- as.numeric(cor.res$case_delay)
cor.res$pearson <- as.numeric(cor.res$pearson)
cor.res$spearman <- as.numeric(cor.res$spearman)
cor.res$kendall <- as.numeric(cor.res$kendall)

cor_p1 <- ggplot(cor.res) + geom_line(aes(x=case_delay,y=pearson,col=site)) + theme(legend.position="none") + xlab("Delay of case reported (days)") + ylab("Pearson correlation")
cor_p2 <- ggplot(cor.res) + geom_line(aes(x=case_delay,y=spearman,col=site)) + theme(legend.position="none") + xlab("Delay of case reported (days)") + ylab("Spearman correlation")
cor_p3 <- ggplot(cor.res) + geom_line(aes(x=case_delay,y=kendall,col=site)) + theme(legend.position="right") + xlab("Delay of case reported (days)") + ylab("Kendall correlation")

cor_p <- cor_p1 | cor_p2 | cor_p3

ggsave(paste0("./plots/cor_delay_",Sys.Date(),".pdf"),cor_p,width=12,height=8)





#site.name <- c("Phillip Lee","Old Winn Dixie","South Fulton","Jonesboro","Flint","Intrenchment","Nancy Creek","Proctor Creek","Peachtree Creek")
site.name <- c("Phillip Lee","Old Winn Dixie","South Fulton","Jonesboro","Flint","Intrenchment")

list.c.ww <- list()
cor.mat.pearson <- matrix(NA,nrow=6,ncol=6)
cor.mat.spearman <- matrix(NA,nrow=6,ncol=6)
cor.mat.kendall <- matrix(NA,nrow=6,ncol=6)
for (i in 1:6){
  t.ref <- min(memfiltration_data$Collect_date);
  dat.ww <- memfiltration_data %>% filter(Influent_Location %in% site.name[i])
  c.ww <- log10(dat.ww$cp_100mL)
  t.waste <- dat.ww$Collect_date
  #t.ref <- min(t.waste);
  t.ww <- as.numeric(difftime(t.waste,t.ref,units="days"));
  
  n.cs <- log10(fulton_case$cases)
  t.case <- fulton_case$date
  t.cs <- as.numeric(difftime(t.case,t.ref,units="days"));
  n.cs <- n.cs[which(t.cs>=0)]
  t.cs <- t.cs[which(t.cs>=0)]
  
  #cs.lo <- loess(n.cs~t.cs,as.data.frame(cbind(t.cs,n.cs)),span=0.25,
  #               control=loess.control(surface="direct"));
  ww.lo <- loess(c.ww~t.ww,as.data.frame(cbind(t.ww,c.ww)),span=0.5,
                 control=loess.control(surface="direct"));
  
  cs.smooth <- n.cs
  #cs.smooth <- predict(cs.lo,data.frame(t.cs=t.cs));
  ww.smooth <- predict(ww.lo,data.frame(t.ww=t.cs));
  list.c.ww[[i]] <- ww.smooth
}  
  
for (i in 1:6){
  for (j in i:6){
    cor.mat.pearson[i,j] <- cor(list.c.ww[[i]],list.c.ww[[j]])
    cor.mat.spearman[i,j] <- cor(list.c.ww[[i]],list.c.ww[[j]],method="spearman")
    cor.mat.kendall[i,j] <- cor(list.c.ww[[i]],list.c.ww[[j]],method="kendall")
  }
  #cor.mat.pearson[i,7] <- cor(list.c.ww[[i]],n.cs)
  #cor.mat.spearman[i,7] <- cor(list.c.ww[[i]],n.cs,method="spearman")
  #cor.mat.kendall[i,7] <- cor(list.c.ww[[i]],n.cs,method="kendall")
}
#cor.mat.pearson[7,7] <- 1
#cor.mat.spearman[7,7] <- 1
#cor.mat.kendall[7,7] <- 1

rownames(cor.mat.pearson) <- site.name
colnames(cor.mat.pearson) <- site.name
rownames(cor.mat.spearman) <- site.name
colnames(cor.mat.spearman) <- site.name
rownames(cor.mat.kendall) <- site.name
colnames(cor.mat.kendall) <- site.name

# Melt the correlation matrix
melted_cor.mat <- melt(round(cor.mat.pearson,2), na.rm = TRUE)
# Heatmap
cor.mat.p1 <- ggplot(data = melted_cor.mat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       limit = c(0.4,1), midpoint = 0.7, space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ xlab(NA) + ylab(NA) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   hjust = 1))+
  coord_fixed() + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

# Melt the correlation matrix
melted_cor.mat <- melt(round(cor.mat.spearman,2), na.rm = TRUE)
# Heatmap
cor.mat.p2 <- ggplot(data = melted_cor.mat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       limit = c(0.4,1), midpoint = 0.7, space = "Lab", 
                       name="Spearman\nCorrelation") +
  theme_minimal()+ xlab(NA) + ylab(NA) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   hjust = 1))+
  coord_fixed() + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))


# Melt the correlation matrix
melted_cor.mat <- melt(round(cor.mat.kendall,2), na.rm = TRUE)
# Heatmap
cor.mat.p3 <- ggplot(data = melted_cor.mat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       limit = c(0.5,1), midpoint = 0.75, space = "Lab", 
                       name="Kendall\nCorrelation") +
  theme_minimal()+ xlab(NA) + ylab(NA) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   hjust = 1))+
  coord_fixed() + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

cor.mat.p <- cor.mat.p1 | cor.mat.p2
ggsave("./plots/cor_matrix.pdf",cor.mat.p,width=8,height=4)

#residual of wastewater signals;
resid.p <- list()
site.name <- c("Phillip Lee","Old Winn Dixie","South Fulton","Jonesboro","Flint","Intrenchment","Nancy Creek","Proctor Creek","Peachtree Creek")
for (i in 1:9){
  dat.ww <- memfiltration_data %>% filter(Influent_Location %in% site.name[i]) %>% filter(!is.na(cp_100mL))
  c.ww <- log10(dat.ww$cp_100mL)
  t.waste <- dat.ww$Collect_date
  t.ref <- min(t.waste);
  t.ww <- as.numeric(difftime(t.waste,t.ref,units="days"));
  
  n.cs <- log10(fulton_case$cases)
  t.case <- fulton_case$date
  t.cs <- as.numeric(difftime(t.case,t.ref,units="days"));
  n.cs <- n.cs[which(t.cs>=0)]
  t.cs <- t.cs[which(t.cs>=0)]

  if (i %in% 1:6){
    ww.lo <- loess(c.ww~t.ww,as.data.frame(cbind(t.ww,c.ww)),span=0.3,
                   control=loess.control(surface="direct"));
  } else {
    ww.lo <- loess(c.ww~t.ww,as.data.frame(cbind(t.ww,c.ww)),span=1,
                   control=loess.control(surface="direct"));
  }
  
  ww.smooth <- predict(ww.lo,data.frame(t.ww=t.cs));
  
  dat.ww$residual <- ww.lo$residuals
  print(paste0(site.names[i]," MSE: ",mean(ww.lo$residuals^2,na.rm=T)))
  
  resid.p[[i]]<-ggplot(dat.ww,aes(x=Collect_date,y=residual)) + geom_point() + ggtitle(site.name[i])
}

resid.plots <- (resid.p[[1]] | resid.p[[2]] | resid.p[[3]]) / (resid.p[[4]] | resid.p[[5]] | resid.p[[6]]) / (resid.p[[7]] | resid.p[[8]] | resid.p[[9]])
ggsave("./plots/resid.pdf",resid.plots,width=8,height=8)

calcSSE <- function(x){
  loessMod <- try(loess(c.ww~t.ww,as.data.frame(cbind(t.ww,c.ww)),span=x,
                        control=loess.control(surface="direct")), silent=T)
  res <- try(loessMod$residuals, silent=T)
  if(class(res)!="try-error"){
    if((sum(res^2, na.rm=T) > 0)){
      sse <- sum(res^2)  
    }
  }else{
    sse <- 99999
  }
  return(sse)
}

# Run optim to find span that gives min SSE, starting at 0.5
optim(par=c(0.5), calcSSE, method="SANN")

ggplot() +
  geom_point(data=memfiltration_data %>% filter(Influent_Location %in% c("Phillip Lee")), aes(x = Collect_date, y = cp_100mL, shape = non_detect_1),alpha = 0.8, size = 2.5, color = "purple") +
  theme_linedraw() +
  scale_shape_manual(values = c(19, 1), guide = FALSE) +
  geom_smooth(data=memfiltration_data %>% filter(Influent_Location %in% c("Phillip Lee")),aes(x = Collect_date, y = cp_100mL), method="loess",color = "black", span = 0.07242214, se = FALSE) +
  geom_line(data = fulton_case, mapping=aes(x = date, y = cases*100),col="tomato") +
  geom_line(data = cases[[1]]%>%filter(case>0), mapping=aes(x = report_date, y = case*100), col="gold") +
  scale_y_continuous("SARS-CoV-2 conc (cp/100mL)", trans='log10', sec.axis = sec_axis(~ ./100, name = "reported cases")) +
  xlim(as.Date(c('3/20/21','05/08/22'), format="%m/%d/%y")) + 
  theme(legend.position = "none", axis.title.x = element_blank(),
        axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"), 
        axis.title.y.right = element_text(color = "red")) +
  ggtitle(expression("Phillip Lee, Spearman "~rho~" = 0.52")
