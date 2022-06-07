#pcakages;
library(ggplot2)

#fecal shedding study plots;
setwd("~/stat/nCoV_SewageSurveillance/")
dat.shed <- read.csv("./data/Moe COVID-19 Wastewater Study Stool Samples PCR results as of Oct 20 2021.csv",stringsAsFactors = F)
dat.patient <- read.csv("./data/Emergent Phlebotomy Enrollement List.csv",stringsAsFactors = F)
dat.shed <- dat.shed[-which(dat.shed$Sample.ID==""),]

dat <- merge(dat.shed,dat.patient,by.x="Sample.ID",by.y="Subject.ID",all.x=T)

#clean data;
dat <- dat[-which(dat$Covid.19.Result==""),]

dat$Day[which(dat$Day=="Unknown")] <- 27 #imput with data collected information;
dat$Day <- as.numeric(dat$Day)
dat$Cohort[which(dat$Cohort=="unvaccinated")] <- "Unvaccinated"

p1 <- ggplot(data=dat) +
  geom_point(aes(x=Day, y=Sample.ID, col=Covid.19.Result)) +
  facet_grid(Cohort ~ ., scales = "free_y", space='free_y')
p1

ggsave(paste0("./plots/fecal_shedding_res",Sys.Date(),".pdf"),height=8,width=8)
