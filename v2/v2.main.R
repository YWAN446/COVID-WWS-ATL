#Simulation of adaptive ES sampling allocation in Kolkata;
#set direction
setwd("~/stat/nCoV_SewageSurveillance/")
#version
#This version compare PS, PSU, Latrine sensistivity in 4 scenarios.
version <- "v2"
part <- "1"

#install packages if needed;
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE, repos="http://cran.rstudio.com/")
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("igraph","sp","calibrate","rgeos","ggplot2","doParallel","doRNG")

#packages
#library(SSN)
library(igraph)
library(sp)
library(calibrate)
library(rgeos)
library(ggplot2)
library(doParallel)
library(doRNG)

#generate the sewage map and lartine locations
#source(paste0("./",version,"/",version,".simulSewage.R"))
#loading data
#if not generate ssn, then load it;
source(paste0("./",version,"/",version,".load.R"))
#loading parameters and settings
source(paste0("./",version,"/",version,".param.R"))
#loading functions
source(paste0("./",version,"/",version,".func.R"))

#rm(mat.connect.line)
rm(mat.connect.points)
#rm(mat.upstream.line)
#rm(mat.upstream.points)

no_cores <- detectCores()
cl <- makeForkCluster(nnodes = no_cores)
registerDoParallel(cl)
getDoParWorkers()
registerDoRNG()

#scenario 1: flat + low decay + high/low lab method sens;
#scenario 2: flat + high decay + high/low lab method sens;
#scenario 3: risk zone + low decay + high/low lab method sens;
#scenario 4: risk zone + high decay + high/low lab method sens;

#for (k.scenario in 1:4){
k.scenario <- 4
  if (k.scenario %in% c(1,3)) {
    #low decay rate;
    gamma.shape <- 1
    gamma.rate <- 0.25
  } else if (k.scenario %in% c(2,4)) {
    #high decay rate;
    gamma.shape <- 1
    gamma.rate <- 2
  }
  vec.lambda <- c(1,2,3,4,5,6,seq(7,100,by=3),seq(105,140,by=5),seq(150,200,by=10))
  sens <- array(NA,dim=c(n.sim,length(vec.lambda),10))
  for(lam in 1:length(vec.lambda)){
    lambda=vec.lambda[lam]
    result <- foreach(k.sim = 1:n.sim, .combine = rbind) %dopar% {
      #source(paste0("./",version,"/",version,".simul.R"))
      n.infected <- rpois(n.days,getLambda(1:n.days,lambda,season = FALSE))
      
      #simulate which latrines the shedders will go;
      #unequal length
      latrine_point <- sort(c(runif(n.latrine-1,0,n.latrine),0,n.latrine))
      #equal length
      # latrine_point <- 0:n.latrine
      latrine_range <- data.frame(start=latrine_point[-length(latrine_point)], end=latrine_point[-1])
      latrine_range$length <- latrine_range$end - latrine_range$start
      
      if (k.scenario %in% c(3,4)){
        gps.points.range <- iterative.ssn@obspoints@SSNPoints[[1]]@points.bbox
        gps.points.range.x <- gps.points.range[1,2]-gps.points.range[1,1]
        gps.points.range.y <- gps.points.range[2,2]-gps.points.range[2,1]
        gps.points <- iterative.ssn@obspoints@SSNPoints[[1]]@point.coords
        
        outbreak.x <- runif(n.outbreak.area,gps.points.range[1,1],gps.points.range[1,2])
        outbreak.y <- runif(n.outbreak.area,gps.points.range[2,1],gps.points.range[2,2])
        outbreak.points <- which(((gps.points[,1]-outbreak.x[1])/gps.points.range.x)^2+((gps.points[,2]-outbreak.y[1])/gps.points.range.y)^2<=outbreak.range)
        for (j in 1:n.outbreak.area){
          outbreak.points <- unique(c(outbreak.points,which(((gps.points[,1]-outbreak.x[j])/gps.points.range.x)^2+((gps.points[,2]-outbreak.y[j])/gps.points.range.y)^2<=outbreak.range)))
        }
        latrine_range$outbreak.length <- latrine_range$length
        latrine_range$outbreak.length[outbreak.points] <- latrine_range$length[outbreak.points]*outbreak.factor
        latrine_range$outbreak.length <- latrine_range$outbreak.length/sum(latrine_range$outbreak.length)*n.latrine #standardize to n.latrine;
        latrine_range$outbreak.end <- cumsum(latrine_range$outbreak.length)
        latrine_range$outbreak.start <- c(0,latrine_range$outbreak.end[-n.latrine])  
      }
      
      mat.latrine.pos <- matrix(0,nrow=n.days,ncol=n.latrine)
      for (t in which(n.infected>0)){
        vec.loc.pos <- sample(x=1:n.latrine,size=n.infected[t],prob=latrine_range$outbreak.length,replace = TRUE)
        mat.latrine.pos[t,unique(sort(vec.loc.pos))] <- table(vec.loc.pos)
      }
      rm(latrine_range)
      
      #shedding
      mat.count.gen <- matrix(0,nrow=n.days,ncol=n.latrine)
      array.count.gen <- array(0, dim=c(n.days,n.latrine,n.days.shed))
      
      k.shed <- sort(unique(as.vector(mat.latrine.pos)))
      k.shed <- k.shed[which(k.shed>0)]
      
      Mu.shed <- log(mu.shed)-0.5*sigma.shed^2
      
      for (k in 1:n.days.shed){
        mat.count.tmp <- matrix(0,nrow=n.days,ncol=n.latrine)
        for (i in k.shed){
          latrine.shed.i <- which(mat.latrine.pos==i)
          if (i==1){
            mat.count.tmp[latrine.shed.i] <- round(rlnorm(length(latrine.shed.i),Mu.shed,sigma.shed)*rbinom(length(latrine.shed.i),1,5*dnbinom(k,3,0.4)),0)
          } else {
            mat.count.tmp[latrine.shed.i] <- round(rowSums(matrix(rlnorm(length(latrine.shed.i)*i,Mu.shed,sigma.shed)*rbinom(length(latrine.shed.i)*i,1,5*dnbinom(k,3,0.4)),ncol=i)),0)
          }
        }
        array.count.gen[k:n.days,,k] <- mat.count.tmp[1:(n.days+1-k),]
        rm(mat.count.tmp)
      }
      
      mat.count.gen <- rowSums(array.count.gen,dims=2)

      rm(array.count.gen)
      rm(mat.latrine.pos)
      #assume decay and lost as pgamma(x,2.5,0.5);
      
      mat.upstream.dis.points.prop <- 1-pgamma(mat.upstream.dis.points,shape=gamma.shape,rate=gamma.rate)
      mat.upstream.dis.points.prop[which(is.na(mat.upstream.dis.points.prop))] <- 0
      mat.count <- matrix(NA,nrow=n.days,ncol=n.latrine)
      
      mat.count <- round(mat.count.gen%*%t(mat.upstream.dis.points.prop),0)
      rm(mat.upstream.dis.points.prop)
      
      #prob of detecting positive
      vol.dilution <- total.vol * iterative.ssn@obspoints@SSNPoints[[1]]@point.data$addfunccol
      
      mat.conc <- t(apply(mat.count,1,FUN = function(x) x/vol.dilution))
      # mat.pos <- mat.conc
      # mat.pos[which(mat.conc<LLOD.test.low)] <- 0
      # mat.pos[which(mat.conc>=LLOD.test.low)] <- 1
      # vec.prob.pos <- colMeans(mat.pos)
      # 
      # std.vec.prob.pos <- vec.prob.pos/max(vec.prob.pos)
      
      mat.dis.pump.stat <- as.numeric(iterative.ssn@obspoints@SSNPoints[[1]]@point.data$upDist)
      mat.dis.pump.stat.prop <- 1-pgamma(mat.dis.pump.stat,shape=gamma.shape,rate=gamma.rate)
      mat.dis.pump.stat.prop[which(is.na(mat.dis.pump.stat.prop))] <- 0
      
      total.count <- round(rowSums(sweep(mat.count.gen,MARGIN=2,mat.dis.pump.stat.prop,`*`)),0)
      rm(mat.count.gen)
      rm(mat.count)
      rm(mat.dis.pump.stat.prop)
      uf.conc <- total.count/total.vol
      #low sensitivity
      uf.pos <- uf.conc
      uf.pos[which(uf.conc<LLOD.uf.test.low)] <- 0
      uf.pos[which(uf.conc>=LLOD.uf.test.low)] <- 1
      uf.prob.pos1 <- mean(uf.pos,na.rm = TRUE)
      
      sample.latrine <- siteInitLatrine(iterative.ssn, n.row, n.col)
      result.latrine <- siteAnalysisLatrine(sample.latrine, mat.conc, uf.pos, LLOD.test.low)
      res.latrine1 <- mean(result.latrine[[2]])
      res.latrine.PS1 <- mean(result.latrine[[3]])
      
      sample.PSU <- siteInit(iterative.ssn, n.row, n.col, n.minimum.latrine)
      sample.PSU <- sample.PSU[which(!is.na(sample.PSU$ID)),]
      sample.PSU$PSU <- rep(1:(length(sample.PSU$PSU)/n.sample.pooling),each=n.sample.pooling)
      result.PSU <- siteAnalysis(sample.PSU, mat.conc, uf.pos, LLOD.test.low)
      res.PSU1 <- mean(result.PSU[[2]])
      res.PSU.PS1 <- mean(result.PSU[[3]])
      
      #high sensitivity
      uf.pos <- uf.conc
      uf.pos[which(uf.conc<LLOD.uf.test.high)] <- 0
      uf.pos[which(uf.conc>=LLOD.uf.test.high)] <- 1
      uf.prob.pos2 <- mean(uf.pos,na.rm = TRUE)
      
      sample.latrine <- siteInitLatrine(iterative.ssn, n.row, n.col)
      result.latrine <- siteAnalysisLatrine(sample.latrine, mat.conc, uf.pos, LLOD.test.high)
      res.latrine2 <- mean(result.latrine[[2]])
      res.latrine.PS2 <- mean(result.latrine[[3]])
      
      sample.PSU <- siteInit(iterative.ssn, n.row, n.col, n.minimum.latrine)
      sample.PSU <- sample.PSU[which(!is.na(sample.PSU$ID)),]
      sample.PSU$PSU <- rep(1:(length(sample.PSU$PSU)/n.sample.pooling),each=n.sample.pooling)
      result.PSU <- siteAnalysis(sample.PSU, mat.conc, uf.pos, LLOD.test.high)
      res.PSU2 <- mean(result.PSU[[2]])
      res.PSU.PS2 <- mean(result.PSU[[3]])
      
      rm(mat.conc)
      
      #sens[k.sim,lam,] <- c(uf.prob.pos1,res.latrine1,res.latrine.PS1,res.PSU1,res.PSU.PS1,
      #                 uf.prob.pos2,res.latrine2,res.latrine.PS2,res.PSU2,res.PSU.PS2)
      c(uf.prob.pos1,res.latrine1,res.latrine.PS1,res.PSU1,res.PSU.PS1,
        uf.prob.pos2,res.latrine2,res.latrine.PS2,res.PSU2,res.PSU.PS2)
    }
    sens[,lam,] <- result
    save(sens,file=paste0("./v10/output/temp_scen_",k.scenario,"_lambda_",lambda,"_part_",part,".rda"))
    cat(paste0("k.scenario = ",k.scenario," & lambda = ", lambda, "\n"))
    print(gc())
  }
  save(sens,file=paste0("./v10/output/scenario",k.scenario,"_part_",part,".rda"))
#}
#stopImplicitCluster()


# sens.label <- c("low","high")
# sens.index <- c(0,5)
# for (k.scenario in 1:4){
#   load(paste0("./v10/output/scenario",k.scenario,".rda"))
#   for (k.sens in 1:2){
#     pdf(file=paste0("./v10/output/latrine_PSU_scenario",k.scenario,"sens",sens.label[k.sens],"_",Sys.Date(),".pdf"))
#     par(mar=c(5.1,5.1,4.1,1))
#     plot(1, type="n",col="black",xlim=c(0,max(vec.lambda)),ylim=c(0,1),xlab="lambda",ylab="sensitivity of detection",cex.main=2,cex.lab=2,cex.axis=2)
#     #polygon(c(vec.lambda,rev(vec.lambda)),c(apply(sens[,,1],2,function(x) quantile(x,probs=0.95)),rev(apply(sens[,,1],2,function(x) quantile(x,probs=0.05)))),col="grey")
#     lines(vec.lambda, apply(sens[,,sens.index[k.sens]+1],2,function(x) quantile(x,probs=0.5)),col="black",lty=1,lwd=2)
#     lines(vec.lambda, apply(sens[,,sens.index[k.sens]+1],2,function(x) quantile(x,probs=0.95)),col="black",lty=2,lwd=2)
#     lines(vec.lambda, apply(sens[,,sens.index[k.sens]+1],2,function(x) quantile(x,probs=0.05)),col="black",lty=2,lwd=2)
#     
#     #polygon(c(vec.lambda,rev(vec.lambda)),c(apply(sens[,,2],2,function(x) quantile(x,probs=0.95)),rev(apply(sens[,,2],2,function(x) quantile(x,probs=0.05)))),col="skyblue")
#     lines(vec.lambda, apply(sens[,,sens.index[k.sens]+2],2,function(x) quantile(x,probs=0.5)),col="skyblue",lty=1,lwd=2)
#     lines(vec.lambda, apply(sens[,,sens.index[k.sens]+2],2,function(x) quantile(x,probs=0.95)),col="skyblue",lty=2,lwd=2)
#     lines(vec.lambda, apply(sens[,,sens.index[k.sens]+2],2,function(x) quantile(x,probs=0.05)),col="skyblue",lty=2,lwd=2)
#     
#     #polygon(c(vec.lambda,rev(vec.lambda)),c(apply(sens[,,4],2,function(x) quantile(x,probs=0.95)),rev(apply(sens[,,4],2,function(x) quantile(x,probs=0.05)))),col="brown1")
#     lines(vec.lambda, apply(sens[,,sens.index[k.sens]+4],2,function(x) quantile(x,probs=0.5)),col="brown1",lty=1,lwd=2)
#     lines(vec.lambda, apply(sens[,,sens.index[k.sens]+4],2,function(x) quantile(x,probs=0.95)),col="brown1",lty=2,lwd=2)
#     lines(vec.lambda, apply(sens[,,sens.index[k.sens]+4],2,function(x) quantile(x,probs=0.05)),col="brown1",lty=2,lwd=2)
#     
#     legend("bottomright",c("PS","Latrine","PSU"),col=c("black","skyblue","brown1"),lty=1,bty="n",lwd=2)
#     
#     dev.off()
#     
#     pdf(file=paste0("./v10/output/latrine_PSU_scenario",k.scenario,"sens",sens.label[k.sens],"_",Sys.Date(),".pdf"))
#     par(mar=c(5.1,5.1,4.1,1))
#         plot(1, type="n",col="black",xlim=c(0,max(vec.lambda)),ylim=c(0,1),xlab="lambda",ylab="sensitivity of detection",cex.main=2,cex.lab=2,cex.axis=2)
#         #polygon(c(vec.lambda,rev(vec.lambda)),c(apply(sens[,,1],2,function(x) quantile(x,probs=0.95)),rev(apply(sens[,,1],2,function(x) quantile(x,probs=0.05)))),col="grey")
#         lines(vec.lambda, apply(sens[,,sens.index[k.sens]+1],2,function(x) quantile(x,probs=0.5)),col="black",lty=1,lwd=2)
#         lines(vec.lambda, apply(sens[,,sens.index[k.sens]+1],2,function(x) quantile(x,probs=0.95)),col="black",lty=2,lwd=2)
#         lines(vec.lambda, apply(sens[,,sens.index[k.sens]+1],2,function(x) quantile(x,probs=0.05)),col="black",lty=2,lwd=2)
#         
#         #polygon(c(vec.lambda,rev(vec.lambda)),c(apply(sens[,,2],2,function(x) quantile(x,probs=0.95)),rev(apply(sens[,,2],2,function(x) quantile(x,probs=0.05)))),col="skyblue")
#         lines(vec.lambda, apply(sens[,,sens.index[k.sens]+4],2,function(x) quantile(x,probs=0.5)),col="brown1",lty=1,lwd=2)
#         lines(vec.lambda, apply(sens[,,sens.index[k.sens]+4],2,function(x) quantile(x,probs=0.95)),col="brown1",lty=2,lwd=2)
#         lines(vec.lambda, apply(sens[,,sens.index[k.sens]+4],2,function(x) quantile(x,probs=0.05)),col="brown1",lty=2,lwd=2)
#         
#         #polygon(c(vec.lambda,rev(vec.lambda)),c(apply(sens[,,4],2,function(x) quantile(x,probs=0.95)),rev(apply(sens[,,4],2,function(x) quantile(x,probs=0.05)))),col="brown1")
#         lines(vec.lambda, apply(sens[,,sens.index[k.sens]+5],2,function(x) quantile(x,probs=0.5)),col="skyblue",lty=1,lwd=2)
#         lines(vec.lambda, apply(sens[,,sens.index[k.sens]+5],2,function(x) quantile(x,probs=0.95)),col="skyblue",lty=2,lwd=2)
#         lines(vec.lambda, apply(sens[,,sens.index[k.sens]+5],2,function(x) quantile(x,probs=0.05)),col="skyblue",lty=2,lwd=2)
#         
#         legend("bottomright",c("PS","PSU","PS+PSU"),col=c("black","brown1","skyblue"),lty=1,bty="n",lwd=2)
#         
#       dev.off()
#   }
# }

