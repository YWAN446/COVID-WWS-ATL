#functions

#seasonality;
getLambda <- function(t, lambda, season=FALSE){ #The seasonality was set to follow a sin curve, and September is the peak.
  if (all(t>=0) & season==TRUE){
    lam <- ((-sin(t*pi*2/365)+1)/2)*lambda
    return(lam)
  } else if (season==FALSE){
    return(lambda)
  } else {print("ERROR: The day should be larger than 0")}
}

#adaptive sampling function
siteInit <- function(iterative.ssn, n.row=4, n.col=5, n.minimum.latrine){
  n.sample = n.row * n.col
  gps.points.range <- iterative.ssn@obspoints@SSNPoints[[1]]@points.bbox
  gps.points.range.x <- gps.points.range[1,2]-gps.points.range[1,1]
  gps.points.range.y <- gps.points.range[2,2]-gps.points.range[2,1]
  gps.points <- iterative.ssn@obspoints@SSNPoints[[1]]@point.coords
  
  samples <- c()
  PSUs <- c()
  i=1
  j=1
  sample.x <- c()
  sample.y <- c()
  while (i <= n.sample){
    sample.x[i] <- runif(1,gps.points.range[1,1]+((i-1) %% n.col)/n.col*gps.points.range.x,gps.points.range[1,1]+(((i-1) %% n.col)+1)/n.col*gps.points.range.x)
    sample.y[i] <- runif(1,gps.points.range[2,1]+((i-1) %/% n.col)/n.row*gps.points.range.y,gps.points.range[2,1]+(((i-1) %/% n.col)+1)/n.row*gps.points.range.y)
    samples.pot <- which((sample.x[i]-percent.x*gps.points.range.x)<=gps.points[,1] & gps.points[,1]<=(sample.x[i]+percent.x*gps.points.range.x) &
                           (sample.y[i]-percent.y*gps.points.range.y)<=gps.points[,2] & gps.points[,2]<=(sample.y[i]+percent.y*gps.points.range.y))
    j=j+1
    if (length(samples.pot)>=n.minimum.latrine){
      samples.cand <- sample(samples.pot,n.sample.pooling,replace=FALSE)
      while (any(samples.cand %in% samples)){
        samples.cand <- sample(samples.pot,n.sample.pooling,replace=FALSE)
      }
      samples <- c(samples,samples.cand)
      PSUs <- c(PSUs,rep(i,n.sample.pooling))
      i=i+1
      j=1
    }
    if (j>100){
      samples <- c(samples,rep(NA,n.sample.pooling))
      PSUs <- c(PSUs,rep(i,n.sample.pooling))
      i=i+1
      j=1
    }
  }
  samples <- as.data.frame(cbind(as.numeric(PSUs),as.numeric(samples),rep(sample.x,each=n.sample.pooling),rep(sample.y,each=n.sample.pooling)),col.names=c("PSU","ID","sample.x","sample.y"))
  names(samples) <- c("PSU","ID","x","y")
  return(samples)
}

siteSample <- function(iterative.ssn, n.sample, n.minimum.latrine){
  gps.points.range <- iterative.ssn@obspoints@SSNPoints[[1]]@points.bbox
  gps.points.range.x <- gps.points.range[1,2]-gps.points.range[1,1]
  gps.points.range.y <- gps.points.range[2,2]-gps.points.range[2,1]
  gps.points <- iterative.ssn@obspoints@SSNPoints[[1]]@point.coords
  
  samples <- c()
  i=1
  sample.x <- c()
  sample.y <- c()
  while (i <= n.sample){
    sample.x[i] <- runif(1,gps.points.range[1,1],gps.points.range[1,2])
    sample.y[i] <- runif(1,gps.points.range[2,1],gps.points.range[2,2])
    samples.pot <- which((sample.x[i]-percent.x*gps.points.range.x)<=gps.points[,1] & gps.points[,1]<=(sample.x[i]+percent.x*gps.points.range.x) &
                           (sample.y[i]-percent.y*gps.points.range.y)<=gps.points[,2] & gps.points[,2]<=(sample.y[i]+percent.y*gps.points.range.y))
    if (length(samples.pot)>=n.minimum.latrine){
      i=i+1
      samples.cand <- sample(samples.pot,n.sample.pooling,replace=FALSE)
      while (any(samples.cand %in% samples)){
        samples.cand <- sample(samples.pot,n.sample.pooling,replace=FALSE)
      }
      samples <- c(samples,samples.cand)
    }
  }
  samples <- as.data.frame(cbind(rep(1:n.sample,each=n.sample.pooling),as.numeric(samples),rep(sample.x,each=n.sample.pooling),rep(sample.y,each=n.sample.pooling)),col.names=c("PSU","ID","sample.x","sample.y"))
  names(samples) <- c("PSU","ID","x","y")
  return(samples)
}

siteAnalysis <- function(samples, mat.conc, uf.pos, LLOD.test){
  samples.res <- cbind(samples,t(mat.conc[,samples$ID]))
  psu.res <- aggregate(samples.res[,5:length(samples.res[1,])], by=list(samples.res$PSU),FUN=mean)
  names(psu.res)[1] <- "PSU"
  psu.res.wo.id <- as.matrix(psu.res[,-1])
  psu.res.wo.id[which(psu.res.wo.id<LLOD.test)] <- 0
  psu.res.wo.id[which(psu.res.wo.id>=LLOD.test)] <- 1
  psu.res <- cbind(psu.res$PSU,rowMeans(psu.res.wo.id),psu.res.wo.id)
  
  system.res <- as.numeric(colSums(psu.res[,-c(1,2)]))
  system.res[which(system.res>=1)]=1
  
  system.res.all <- as.numeric(colSums(rbind(psu.res[,-c(1,2)],uf.pos)))
  system.res.all[which(system.res.all>=1)]=1
  return(list(psu.res,system.res,system.res.all))
}

siteDrop <- function(psu.res,uf.pos,n.drop,alpha=0.1){ #alpha is the weight for total number of positive
  k.psu <- psu.res[,1]
  obs <- psu.res[,-c(1,2)]
  fullInfo <- colSums(rbind(obs,uf.pos))
  fullInfo[fullInfo>1]=1
  infoLoss <- c()
  totalPos <- c()
  eff.site <- c()
  for (i in 1:length(obs[,1])){
    jacknifeInfo <- colSums(obs[-i,])
    jacknifeInfo[jacknifeInfo>1]=1
    infoLoss[i] <- sum(fullInfo) - sum(jacknifeInfo)
    totalPos[i] <- sum(obs[i,])
  }
  #define a temp indictor about effeciency of a site;
  eff.site <- infoLoss + alpha * totalPos + runif(length(obs[,1]),0,0.0001)
  if (sum(eff.site)==0){
    return(sample(k.psu,n.drop,replace=FALSE))
  }
  return(k.psu[order(eff.site)][1:n.drop])
}

siteAdd <- function(n.add, psu.drop, iterative.ssn, n.minimum.latrine, samples){
  for (i in 1:n.add){
    k.add <- siteSample(iterative.ssn, 1, n.minimum.latrine)
    while (any(k.add$ID %in% samples$ID)){
      k.add <- siteSample(iterative.ssn, 1, n.minimum.latrine)
    }
    k.add$PSU <- psu.drop[i]
    samples <- rbind(samples,k.add)
  }
  return(samples)
}

siteInitLatrine <- function(iterative.ssn, n.row, n.col){
  n.sample = n.row * n.col
  gps.points.range <- iterative.ssn@obspoints@SSNPoints[[1]]@points.bbox
  gps.points.range.x <- gps.points.range[1,2]-gps.points.range[1,1]
  gps.points.range.y <- gps.points.range[2,2]-gps.points.range[2,1]
  gps.points <- iterative.ssn@obspoints@SSNPoints[[1]]@point.coords

  samples <- c()
  sample.x <- c()
  sample.y <- c()
  for (i in 1:n.sample){
    samples.pot <- which(gps.points[,1]>gps.points.range[1,1]+((i-1) %% n.col)/n.col*gps.points.range.x & gps.points[,1]<gps.points.range[1,1]+(((i-1) %% n.col)+1)/n.col*gps.points.range.x &
                           gps.points[,2]>gps.points.range[2,1]+((i-1) %/% n.col)/n.row*gps.points.range.y & gps.points[,2]<gps.points.range[2,1]+(((i-1) %/% n.col)+1)/n.row*gps.points.range.y)
    if (length(samples.pot)==0){
      samples[i] <- NA
      sample.x[i] <- NA
      sample.y[i] <- NA
    } else {
      samples[i] <- sample(samples.pot,1)
      sample.x[i] <- gps.points[samples[i],1]
      sample.y[i] <- gps.points[samples[i],2]
    }
  }
  samples <- as.data.frame(cbind(as.numeric(samples),as.numeric(sample.x),as.numeric(sample.y)),col.names=c("ID","sample.x","sample.y"))
  names(samples) <- c("ID","x","y")
  samples <- samples[which(!is.na(samples$ID)),]
  return(samples)
}

siteAnalysisLatrine <- function(samples, mat.conc, uf.pos, LLOD.test){
  samples.res <- cbind(samples,t(mat.conc[,samples$ID]))
  latrine.res.wo.id <- as.matrix(samples.res[,-c(1,2,3)])
  latrine.res.wo.id[which(latrine.res.wo.id<LLOD.test)] <- 0
  latrine.res.wo.id[which(latrine.res.wo.id>=LLOD.test)] <- 1
  latrine.res <- cbind(samples,rowMeans(latrine.res.wo.id),latrine.res.wo.id)
  
  system.res <- as.numeric(colSums(latrine.res[,-c(1,2,3,4)]))
  system.res[which(system.res>=1)]=1
  
  system.res.all <- as.numeric(colSums(rbind(latrine.res[,-c(1,2,3,4)],uf.pos)))
  system.res.all[which(system.res.all>=1)]=1
  return(list(latrine.res,system.res,system.res.all))
}
