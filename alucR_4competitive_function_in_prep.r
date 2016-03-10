



# suit | preprocessed suitabilities 
# demandE | vector of demand for the relevant epoche
# demandC | vector of demand change from one epoche to the next
# max.iter | maximum iterations before best allocation from the temporary allocation is chosen (in case stop.cript was not meet)
# stop.crit | stoping criteria defined as a vector with two values: first one refers to max percent difference from change between two epoches, second to the maximum pixel difference allowed. If any is reached the allocation stops
# print.log |
# print.plot | 

# returns a list. at position [[1]] a ratsre with the allocated land use classes, at [[2]] a data frame with the lofile informations from the iterations.


#require(raster) 
#install.packages("snow")
#library(snow)


alucR_competitive <- function(suit, demandE, demandC, var.list, max.iter, stop.crit, ncores=ncores ,print.log=FALSE, print.plot=FALSE ){
  lc_suit <-   var.list [[4]][["lc_suit"]]
  pseudo.N <- var.list [[9]][["pseudo.N"]]
  lc.N <- var.list [[10]][["lc.N"]]
  
  min.demand <- which.min(demandE) 
  
  if(nlayers(suit)!= length(demandE)){print("Warning: layers of suit != length of demand")}
  
  logfile <- data.frame (matrix(data=NA, ncol= (1+ length(lc.N)+ length(lc.N)+ length(lc.N)), nrow=1)) 
  pix_d_names<- paste("pix_d",lc.N, sep="")
  adj.p_names <- paste("next.adj.p",lc.N, sep="")
  iter_names <- paste("iter",lc.N, sep="")
  names(logfile) <-  c("u", pix_d_names ,adj.p_names , iter_names)
  
  iter <- rep(0, times=length(lc.N))
  
  #start allocation
  u=1
  #beginCluster(n=ncores)
  repeat {
    ##### 
    if (u==1){
      p_suit <- suit
    }else{
      p_suit <- suit + as.numeric(iter)
    }
    # temporary allocation using clusterR and which.max 
    beginCluster(n=ncores)
    t_aloc <- clusterR(p_suit, which.max) # all NA which is not "suit" or "natural" - returns number of layer with max value. 
    endCluster()
    
    #assess number of pixels per suit class resulting from the temporary allocation
    f_class <- as.data.frame(freq(t_aloc, useNA="no")) 
    f_class$reclass <- lc.N[match(f_class$value, 1:nlayers(p_suit))]
    ind <- match(lc.N , f_class$reclass)
    t_class <- f_class[ind,"count"]
    if(any(is.na(t_class))){
      t_class[is.na(t_class)] <- 0
    }
    
    #calc difference in pixel and percent todemand 
    pix_d <- t_class - demandE # make sure they always have the same dimensions
    perc_d <- pix_d/demandE # 
    
    if(u==1){ # initializing adj.p 
      adj.p <- as.vector(ifelse (sign(pix_d)!= 0, -1*sign(pix_d) *1/100, 0), mode="numeric") 
      
    } else { 
      proportion <- abs(unlist(ifelse(logfile[u-1,pix_d_names]!=0,pix_d/logfile[u-1,pix_d_names], 0)))# check if that makes sense and is needed
      adj.p <- as.vector(ifelse(pix_d == 0 , 0, 
                                ifelse(pix_d != 0 & logfile[u-1, adj.p_names]== 0, -1*sign(pix_d)*1/sample(50:150,1),
                                       ifelse(sign(pix_d)!=sign(logfile[u-1, pix_d_names]), -1* logfile[u-1,adj.p_names]/2,
                                              ifelse (perc_d > 0.2 & proportion < 1, (logfile[u-1,adj.p_names]*2)+(logfile[u-1,adj.p_names]*proportion),  
                                                      ifelse (perc_d > 0.05 & proportion < 1, logfile[u-1,adj.p_names]+(logfile[u-1,adj.p_names]*proportion),   
                                                              logfile[u-1,adj.p_names]*proportion))))), mode="numeric") 	
    }
    #prevent all adj to have the same sign on from the secnd iteration and set the last one (lowest hierchy) to 0
    if (all(sign(adj.p)==-1) | all(sign(adj.p)==+1)){ 
      adj.p[length(adj.p)] <- 0
    }
    #define upper and lower boundraies of adjustment
    adj.p <- ifelse (adj.p < -2, -2, ifelse(adj.p > 2,2, adj.p ))
    
    #adjust iter
    iter <- as.numeric(iter) + as.numeric(adj.p)
    iter <- ifelse(iter < -2, -2, ifelse(iter > 2,2, iter)) 
    
    #update logfile 
    if (u==1){
      logfile[u , ] <- c( u , pix_d, adj.p, rep(0, times=length(lc.N)))
    }else{
      log_tmp <- c( u , pix_d, adj.p, iter)
      names(log_tmp) <- names(logfile)
      logfile <- rbind (logfile, log_tmp)
    }
    
    if(print.log==TRUE){
      print (logfile[u,])
    }
    
    if(print.plot == TRUE){
      plot(0,0,xlim = c(2,max.iter),ylim = c(-1,1),ylab="iter", xlab="iteration", type = "n")
      grid()
      names.legend <- paste ("LC", c(lc.N))
      legend("topright", legend= names.legend , col=rainbow(lc.N), pch=15)
      for (i in 1:length(lc.N)){
        lines(c(1:nrow(logfile)),logfile[,iter_names[i]],col=rainbow(lc.N)[i],type = 'l', lwd=2);
      }
    }
    
    #stop criteria meet?
    # are the min defined percent of change pixels allocated 
    if (max(abs(pix_d[1:length(demandC)]/demandC)*100) < stop.crit[1] & max(abs(pix_d[min.demand])) < stop.crit[2]) {
      if (print.log==TRUE){print("Stop criterium: percent of change")}
      beginCluster(n=ncores)
      aloc <- clusterR(t_aloc , reclassify, args=list(rcl =  matrix (c(1:nlayers(p_suit), lc.N), ncol=2)))
      endCluster()
      break
    }
    # is the max differences in pixel between allocated and demand smaller than defined
    if(max(abs(pix_d)) < stop.crit[2]){
      if (print.log==TRUE){print("Stop criterium: number of Pixel")}
      beginCluster(n=ncores)
      aloc <- clusterR(t_aloc , reclassify, args=list(rcl =  matrix (c(1:nlayers(p_suit), lc.N), ncol=2)))
      endCluster()
      break
    }
    
    # stop argument iteration if ITERmax reached and take the ITER with the minimum deviation from the demand from all iterations 
    if (u == max.iter) {
      if (print.log==TRUE){print("Stop criterium: number of max. iterations reached")}
      #which iteration had the smallest overall pixel difference
      ind <- which.min (rowSums (abs(logfile [, "pix_d"])))
      iterfinal <- logfile [ind, iter_names]
      
      p_suit <- p_suit + as.numeric(iterfinal)
      beginCluster(n=ncores)
      t_aloc <- clusterR(p_suit, which.max) # all NA which is not "suit" or "natural" - returns number of layer with max value. 
      aloc <- clusterR(t_aloc , reclassify, args=list(rcl =  matrix (c(1:nlayers(p_suit), lc.N), ncol=2))) # only the final map...
      endCluster()
      # update logfile
      log_tmp <- c( u , logfile[ind, pix_d_names], logfile[ind, adj.p_names], iterfinal)
      logfile <- rbind (logfile, log_tmp)
      
      break;
    }
    
    #initialize next u sequence
    u=u+1;
  }
  #endCluster()
  out <- list(aloc, logfile)
  return (out)
}