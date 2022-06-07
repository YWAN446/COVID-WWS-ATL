#pcakages;
library(ggplot2)
library(patchwork)

#fecal shedding study plots;
setwd("~/stat/nCoV_SewageSurveillance/")
dat.shed <- read.csv("./data/Moe COVID-19 Wastewater Study Stool Samples_021722.csv",stringsAsFactors = F)
dat.patient <- read.csv("./data/EP result updates.csv",stringsAsFactors = F)

#clean subject.ID;
for (i in which(vapply(dat.patient$Subject.ID,1,FUN=nchar)==9)){
  dat.patient$Subject.ID[i] <- paste0(substr(dat.patient$Subject.ID[i],1,5),substr(dat.patient$Subject.ID[i],7,9))
}

dat <- merge(dat.shed,dat.patient,by.x="Sample.ID",by.y="Subject.ID",all.x=T)

#clean data;
dat <- dat[-which(dat$Covid.19.Result==""),]

dat$Day[which(dat$Sample.ID=="WWMOE006" & dat$Day=="Unknown")] <- 27 #imput with data collected information;
dat$Day <- as.numeric(dat$Day)
dat$Cohort[which(dat$Cohort=="unvaccinated")] <- "Unvaccinated"

#adjusted_day based on confirmation day;
dat$Date.Collected <- as.Date(dat$Date.Collected, format="%m/%d/%y")
dat$COVID.19.Confirmed.date <- as.Date(dat$COVID.19.Confirmed.date, format="%m/%d/%y")
dat$adjust_day <- dat$Date.Collected - dat$COVID.19.Confirmed.date

p1 <- ggplot(data=dat) +
  geom_point(aes(x=Day, y=Sample.ID, col=Covid.19.Result), size=3) +
  geom_text(aes(x=Day, y=Sample.ID,label=CTs.N1.N2,vjust=-1,angle = 30)) +
  ylab("patient ID") +
  facet_grid(Cohort ~ ., scales = "free_y", space='free_y')
p1

ggsave(paste0("./plots/fecal_shedding_res_",Sys.Date(),".pdf"),height=8,width=14)

p2 <- ggplot(data=dat %>% filter(!is.na(adjust_day))) +
  geom_point(aes(x=adjust_day, y=Sample.ID, col=Covid.19.Result), size=3) +
  ylab("patient ID") + xlab("Days after COVID-19 confirmation")
  #geom_text(aes(x=adjust_day, y=Sample.ID,label=CTs.N1.N2,vjust=-1,angle = 30)) +
  facet_grid(Cohort ~ ., scales = "free_y", space='free_y')
p2

ggsave(paste0("./plots/fecal_shedding_res_adjusted_day_",Sys.Date(),".pdf"),height=8,width=14)

p1 / p2

library(lattice)
temp <- dat.patient[,8:20]
mat.symp <- dat.patient[,8:20]
for (i in 1:13){
  mat.symp[,i] <- 1
  mat.symp[which(temp[,i]=="No"),i] <- 0
  mat.symp[which(temp[,i]==""),i] <- NA
}
mat.symp <- as.matrix(mat.symp)
rownames(mat.symp) <- dat.patient[,1]
mat.symp <- mat.symp[which(dat.patient[,1] %in% unique(dat$Sample.ID)),]
levelplot(mat.symp,scales=list(x=list(rot=90)))

