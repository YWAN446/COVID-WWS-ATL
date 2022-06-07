if(!exists("dat.cases")) load(paste("./","ww_dat_for_Peter",".rda",sep=""));

t.case <- as.POSIXlt(dat.cases$report_date,format="%Y-%m-%d");
t.waste <- as.POSIXlt(dat.ww$Collect_date,format="%Y-%m_%d");
t.ref <- t.waste[1]; # first wastewater sample

t.ind <- difftime(t.case,t.ref,units="days");
t.freq <- hist(as.numeric(t.ind),
               breaks=((min(t.ind,na.rm=TRUE)-1):max(t.ind,na.rm=TRUE))+0.5,
               plot=FALSE);
t.cs <- t.freq$mids;
n.cs <- t.freq$counts;

incl <- which(t.cs > -7); # one week before first wastewater sample
t.cs <- t.cs[incl];
n.cs <- n.cs[incl];

t.ww <- difftime(t.waste,t.ref,units="days");
c.ww <- dat.ww$log_copies_per_L

cs.lo <- loess(n.cs~t.cs,as.data.frame(cbind(t.cs,n.cs)),span=0.15,
               control=loess.control(surface="direct"));
ww.lo <- loess(c.ww~t.ww,as.data.frame(cbind(t.ww,c.ww)),span=0.5,
               control=loess.control(surface="direct"));

cs.smooth <- predict(cs.lo,data.frame(t.cs=t.cs));
ww.smooth <- predict(ww.lo,data.frame(t.ww=t.cs));

pdf("smoothed.pdf");
par(mfrow=c(3,1),mar=c(4,4,0.2,0.2));
plot(t.cs,cs.smooth,"l");
points(t.cs,n.cs);
plot(t.cs,ww.smooth,"l");
points(t.ww,c.ww);
ccf(ww.smooth,cs.smooth,type="correlation");
dev.off();
