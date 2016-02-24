
# not optimized for raster processing yet. 
#####################################
# competitive allocation
#2. Allocation sub module 
#####
# Description: function providing the allocation routine
#
# suit.N | preprocessed suitability raster - including spatial and trajectory based restriction, elasticities etc.
# lc | raster of initial land cover
# lu.N | class numbers  of all classes to be modelled (incl. pseudo natural class) 
# demand.new | preprocessed demand file including demand for land use classes plus natural vegetation. Adjusted for spatial restrictions
# stop.crit | stop criteria
# iter.max | maximum of iterations
# ncores | amount of cores
# print.plot | TRUE or FALSE
# print.log | TRUE or FALSE
#####
#  2.2 Function - allocation module
#####
allocation.module <- function(suit.N , lc, lu.N ,demandE.new, stop.crit, iter.max, ncores, print.plot, print.log) {
  #####
  #    2.2.1 Initiate & start iteration
  #####
  # initialize iteration
  u = 1
  p_vector.N <- getValues(suit.N)
  demand.new <- demandE.new
  
  lu_layer.N <- 1:length(lu.N)# layers of suitability including natural veg. for later use
  lu_suit.N <- length(lu.N) 
  # empty vectors
  logfile1 <- c()
  names.log <- c("u", paste("pix.d",lu_layer.N, sep=""), paste("adj.p",lu_layer.N, sep=""), paste("iter",lu_layer.N, sep=""))
  pix.d.hist <- c()
  perc.d.hist <- c()
  change.p.hist <- c()
  adj.p.hist <- c()
  # iter
  iter <- rep(0,ncol(p_vector.N))
  iter.hist <- c(iter)
  # descriptive variables
  min.demand <- as.integer(which.min(demand.new))
  # initialize cluster
  cl <- makeCluster(getOption("cl.cores", ncores))
  #cl <- makeCluster(ncores, type = "PSOCK") # "MPI" - to test
  on.exit(stopCluster(cl))
  
  
  ########
  # start iteration to allocate the requested amount of land use plus natural vegetation
  repeat {
    #####
    #  	2.2.2 Read data & add iter
    ##### 
    if (u==1){
      p2_suit.N <- suit.N
    }else{
      for (i in lu_layer.N){
        p2_suit.N[,i] <- p_suit.N[,i]+as.numeric(iter[order(lu.N)[i]])
      }
    }
    #####
    #  	2.2.3 Evaluate competitive advantages
    #####
    # main routine to identify competitive advantages between pixels for each location
    
    tprop_vector_tmp <- parRapply(cl,p2_vector,FUN=function(w) ifelse(all(is.na(w)),NA,which.max(w)))
    tprop_vector <- lu.N[tprop_vector_tmp] 
    #####
    #  	2.2.4 Compare amount of received classes to demand     
    #####
    # evaluate result - how many pixels have been assigned to which class
    n <- tabulate(tprop_vector,nbins=max(lu.N))[sort(lu.N)]
    #print(paste("n:" , n))
    # difference of pixels betwen allocated and requested land use/cover
    pix.d  <- n - demand.new 
    perc.d <- pix.d/demand.new
    #save to history
    pix.d.hist <- rbind(pix.d.hist, pix.d)
    perc.d.hist <- rbind(perc.d.hist, perc.d)
    #####      
    #  	2.2.5 Adjust iter for next iteration (starting 2.2.2)
    #####
    if(u==1){ # initializing adj.p 
      adj.p= -1*sign(perc.d) # *1/100
    }else{ # modifying adj.p based on prior results
      change.perc <- abs((pix.d.hist[u-1,]-pix.d.hist[u,])/(pix.d.hist[u-1,])*100) 
      proportion <- abs(pix.d.hist[u,])/ colSums(abs(pix.d.hist[c(u,u-1),]),na.rm=TRUE)
      better<- abs(pix.d.hist[u,])< abs(pix.d.hist[u-1,])
      
      adj.p <- as.vector(ifelse(pix.d.hist[u,]== 0 , 0, # if pixels are correctly allocated no change
                                # if pixels are not correctly allocated 
                                # if last adj hist == 0 then initialize adj.p hist 
                                ifelse(pix.d.hist[u,]!=0 & adj.p.hist[u-1,]==0,-1*sign(perc.d)*1/runif(1,0.5,1.5), # sample(50:150, 1)
                                       #if the last adjustment did not lead to better results and the sign of pix dif is still the same use the double adj.p from last time
                                       ifelse(better==FALSE & sign(pix.d.hist[u,])==sign(pix.d.hist[u-1,]), adj.p.hist[u-1,]*2,
                                              # if the difference is <= 0.001% and <=20 pixel and the sign is the same half of the proportional adjustment              
                                              ifelse(abs(perc.d)<= 0.001 & abs(pix.d.hist[u,])<= 20 & sign(pix.d.hist[u,])==sign(pix.d.hist[u-1,]), (adj.p.hist[u-1,]/2)+((adj.p.hist[u-1,]/2)*proportion),
                                                     # percent change smaller than 20 and the sign is the same, than use the double of last adj.p
                                                     #ifelse(change.perc< 20& sign(perc.d)==sign(perc.d.hist[u-1,]),adj.p.hist[u-1,]*2,
                                                     # percent change is smaller than 40 and larger than 20 - add proportional adj.p to last adj.p
                                                     #ifelse(change.perc< 40& change.perc >= 20 & sign(perc.d)==sign(perc.d.hist[u-1,]), adj.p.hist[u-1,]+(adj.p.hist[u-1,]*proportion),
                                                     ifelse(change.perc< 40&  sign(perc.d)==sign(perc.d.hist[u-1,]), adj.p.hist[u-1,]+(adj.p.hist[u-1,]*proportion),
                                                            # if sign switch adjust proportional  
                                                            ifelse(sign(pix.d.hist[u,])  !=sign(pix.d.hist[u-1,]), -1* adj.p.hist[u-1,]*proportion,
                                                                   # everything else use the last adj.p
                                                                   adj.p.hist[u-1,])))))), mode="numeric") 
      change.p.hist <- rbind(change.p.hist, change.perc)
    }
    #upper and lower boundaries of adj.p
    adj.p <- ifelse (adj.p < -100, -100, ifelse(adj.p > 100,100, adj.p ))
    # adjust iter values for 
    iter <- iter + adj.p
    #assign("global.iter", iter , envir = .GlobalEnv) 
    iter <- as.numeric (ifelse(iter < -150, -150, ifelse(iter > 150,150, iter))) # upper and lower bound of iter (should never be reached)
    if (u > 1){
      if (all(sign(iter)==-1) | all(sign(iter)==+1)){ # prevent all iter to have the same sign in the second iteration sign(0) returns 0 
        if (all(sign(iter.hist[nrow(iter.hist),])==-1) | all(sign(iter.hist[nrow(iter.hist),])==+1)){              
          iter[which.min(abs(iter))] <-  0  
          adj.p [which.min(abs(iter))] <- 0 # 
        }}}        
    
    ###
    #save to history
    adj.p.hist <- rbind(adj.p.hist, adj.p)
    iter.hist <- rbind(iter.hist, iter)
    assign("global.iter.hist", iter.hist , envir = .GlobalEnv) 
    iter.hist <-iter.hist
    #####    
    if(print.plot==TRUE){
      plot(0,0,xlim = c(2,iter.max),ylim = c(-100,100),ylab="iter", xlab="iteration", type = "n")
      grid()
      names.legend <- paste ("LC", c (sort(lu.N[-lu_suit.N]),"N"))
      legend("topright", legend=names.legend, col=rainbow(lu_suit.N), pch=15)
      for (i in 1:lu_suit.N){
        lines(c(1:nrow(iter.hist)),iter.hist[,order(lu.N)[i]],col=rainbow(lu_suit.N)[i],type = 'l', lwd=2);
      }
    }
    # write logfile
    log.tmp <- as.vector(c(u, pix.d, adj.p, iter), mode="numeric")
    names(log.tmp) <- names.log
    
    logfile1 <- rbind(logfile1,log.tmp)
    #print tail of logfile
    if(print.log==TRUE){
      #print(tail(logfile1))
      names(log.tmp) <-  c("u", paste ("LC", c (sort(lu.N[-lu_suit.N]),"N")))
      print(log.tmp [1:max(lu_layer.N)+1])
    }
    #####
    #  	2.2.6 Stop iteration when amount of classes ~ demand 
    #####
    #stop argument iteration 
    if (max(abs(perc.d),na.rm=TRUE) < stop.crit[1] & abs(pix.d [min.demand]) <= stop.crit[2] )  {
      break;
    }
    if (abs(pix.d [min.demand]) <= stop.crit[2] & max(abs(pix.d),na.rm=TRUE)<=stop.crit[3]){
      break;
    }
    # stop argument iteration if ITERmax reached and take the ITER with the minimum deviation from the demand from all iterations 
    if (u >= iter.max) {
      current.log <- logfile1[(nrow(logfile1)-iter.max+1):nrow(logfile1),]
      iterfinal_index <-  which.min(rowSums(abs(current.log[,2:(lu_suit.N +1)])))
      iterfinal <- current.log[iterfinal_index, (2*lu_suit.N +2):(3*lu_suit.N +1)]
      if(print.log==TRUE){print(iterfinal)}
      for (i in 1:lu_suit.N){ 
        p2_vector[,i] <- p_vector.N[,i]+ as.numeric(iterfinal[i]);
      }      
      #evaluate best p2_vector
      tprop_vector_tmp <- parRapply(cl,p2_vector,FUN=function(w) ifelse(all(is.na(w)),NA,which.max(w)))
      tprop_vector <- lu.N[tprop_vector_tmp] 
      #
      log.tmp <- as.vector(c(I(iter.max+1), current.log [iterfinal_index,2:(3*lu_suit.N +1)]), mode="numeric")
      logfile1 <- rbind(logfile1, log.tmp)
      if(print.log==TRUE){print("break")
                          print(log.tmp )}
      break;
    }
    #initialize next u sequence
    u=u+1;
  }# next iteration over "u"
  #####
  #  	2.2.7 return allocation vector and logfile
  #####
  #stopCluster(cl);
  
  print("allocation done")
  out <- lc
  out <- setValues(lc, tprop_vector)
  return(list(out, logfile1))
}




