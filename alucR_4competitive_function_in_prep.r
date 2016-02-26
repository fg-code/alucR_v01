



# suit | preprocessed suitabilities 
# demandE | vector of demand for the relevant epoche
# demandC | vector of demand change from one epoche to the next
# max.iter | maximum iterations before best allocation from the temporary allocation is chosen (in case stop.cript was not meet)
# stop.crit | stoping criteria defined as a vector with two values: first one refers to max percent difference from change between two epoches, second to the maximum pixel difference allowed. If any is reached the allocation stops
# print.log |
# print.plot | 

# returns a list. at position [[1]] a ratsre with the allocated land use classes, at [[2]] a data frame with the lofile informations from the iterations.


require(raster) 
install.packages("snow")
library(snow)


alucR_competitive <- function(suit, demandE, demandC, var.list, max.iter=100, stop.crit=c(), ncores=ncores ,print.log==FALSE, print.plot==FALSE ){
  #lc_suit <-   var.list [[4]][["lc_suit"]]
  #lc_slookup <- var.list [[4]] [["lc_slookup"]]
  #nochange <- var.list [[5]][["nochange"]]
  #lc_unique <- var.list [[3]][["lc_unique"]]
  #lc_lookup <- var.list [[3]][["lc_lookup"]]
  #natural <- var.list [[8]][["natural"]]
  #naturallookup <-  var.list [[8]][["naturallookup"]]
  lc.N <- var.list [[10]][[lc.N]]
 
min.demand <- which.min(demandE) 
 
logfile <- data.frame (matrix(data=NA, ncol= (1+ length(lc.N)+ length(lc.N)+ length(lc.N))), nrow=1) 
pix_d_names<- paste("pix_d",lc.N, sep="")
adj.p_names <- paste("adj.p",lc.N, sep="")
iter_names <- paste("iter",lc.N, sep="")
names(logfile) <-  c("u", pix_d_names ,adj.p_names , iter_names)

iter <- rep(0, times=length(lc.N))
  
#start allocation
u=1
beginCluster(n=ncores)
repeat {
##### 
if (u==1){
	p2_suit.N <- suit.N
    }else{
	p_suit.N <- p_suit.N + as.numeric(iter)
}
# temporary allocation using clusterR and which.max 
t_aloc_tmp <- clusterR(p_suit.N, which.max) # all NA which is not "suit" or "natural" - returns number of layer with max value. 
t_aloc <- clusterR(t_aloc_tmp , reclassify, args=list(rcl =  matrix (c(1:nlayers(p_suit.N), lc.N), ncol=2))) # possibly change to only the final map...

#assess number of pixels per suit class resulting from the temporary allocation
f_class <- as.data.frame(freq(t_aloc, useNA="no")) 
ind <- match(lc.N , f_class$value)
t_class <- f_class[ind,]

#calc difference in pixel and percent todemand 
pix_d <- t_class - demandE # make sure they always have the same dimensions
perc_d <- pix_d/demandE # 

if(u==1){ # initializing adj.p 
	adj.p <- -1*sign(perc_d) # *1/100
	#prevent all adj to have the same sign on from the secnd iteration and set the last one (lowest hierchy) to 0
	if (all(sign(iter)==-1) | all(sign(iter)==+1)){ 
	adj.p[length(adj.p)] <- 0
	}   
} else { 
# modifying adj.p based on prior results saved in the logfile
	change.perc <- abs((logfile[u-1,pix_d_names]-pix_d)/logfile[u-1,pix_d_names]*100) 
    proportion <- abs(pix_d)/ colSums(abs(logfile[c(u,u-1),pix_d_names]),na.rm=TRUE) # check if that makes sense and is needed
    better <- abs(logfile[u,pix_d_names]) < abs(logfile[u-1,pix_d_names])
	
	#adj adjusting iter based on results at u-1
	 adj.p <- as.vector(ifelse(logfile[u,pix_d_names] == 0 , 0, # if pixels are correctly allocated the adj of iter will be 0
            # if pixels are not correctly allocated 
            # if last adj hist == 0 then initialize adj.p hist, but randomized between 0.5 an 1.5 
            ifelse(logfile[u,pix_d_names] !=0 & logfile[u-1, adj.p_names]== 0, -1*sign(perc_d)*1/runif(1,0.5,1.5), # sample(50:150, 1)
            #if the last adjustment did not lead to better results and the sign of pix dif is still the same use the double adj.p from last time (could also include u-2 in another else function.)
            ifelse(better==FALSE & sign(logfile[u,pix_d_names])==sign(logfile[u-1,pix_d_names]), logfile[u-1,adj.p_names] * 2,
			# if better but sign switch - adjust proportional
			ifelse (better == TRUE & sign(logfile[u,pix_d_names])!=sign(logfile[u-1,pix_d_names]),  -1* logfile[u-1,adj.p_names] * proportion,
            # percent change is smaller than 40 the change direction is the sign is the same - add proportional adj.p to last adj.p
			ifelse(change.perc < 40 &  sign(logfile[u,pix_d_names])==sign(logfile[u-1,pix_d_names]),logfile[u-1,adj.p_names] + logfile[u-1,adj.p_names] * proportion),
			# everything else use the last adj.p
            logfile[u-1,adj.p_names])))))), mode="numeric") 	
	}
	#prevent all adj to have the same sign on from the secnd iteration and set the last one (lowest hierchy) to 0
	if (all(sign(adj.p)==-1) | all(sign(adj.p)==+1)){ 
	adj.p[length(adj.p)] <- 0
	}
	#define upper and lower boundraies of adjustment
	adj.p <- ifelse (adj.p < -200, -200, ifelse(adj.p > 200,200, adj.p ))
		
	#adjust iter
	iter <- as.numeric(iter + adj.p)
	iter <- ifelse(iter < -200, -200, ifelse(iter > 200,200, iter))) 
	
	#update logfile 
	if (u==1){
	logfile[u , ] <- c( u , pix_d, adj.p, rep(0, times(lc.N)))
	}else{
	log_tmp <- c( u , pix_d, adj.p, iter)
	logfile <- rbind (logfile, log_tmp)
	}
	
	if(print.log==TRUE){
	print (logfile[u,])
	}
	
	if(print.plot == TRUE){
	plot(0,0,xlim = c(2,iter.max),ylim = c(-100,100),ylab="iter", xlab="iteration", type = "n")
    grid()
    names.legend <- paste ("LC", c (sort(lu.N[-lu_suit.N]),"N"))
    legend("topright", legend= iter_names , col=rainbow(lu_suit.N), pch=15)
    for (i in 1:lu_suit.N){
      lines(c(1:nrow(logfile)),logfile [,iter_names[i]],col=rainbow(lu_suit.N)[i],type = 'l', lwd=2);
    }
    }
	
	#stop criteria meet?
	# are the min defined percent of change pixels allocated 
	if (max(abs(pix_d/demandC) < stop.crit[1]) {
	break
	}
	# is the max differences in pixel between allocated and demand smaller than defined
	if(max(abs(pix_d) ) < stop.crit[2]){
	break
	}
	    
    # stop argument iteration if ITERmax reached and take the ITER with the minimum deviation from the demand from all iterations 
    if (u == iter.max) {
	#which iteration had the smallest overall pixel difference
	ind <- which.min (row.sums (abs(logfile [, "pix_d"])))
	iterfinal <- logfile [ind, iter_names]
	
	p_suit.N <- p_suit.N + as.numeric(iterfinal)
	t_aloc_tmp <- clusterR(p_suit.N, which.max) # all NA which is not "suit" or "natural" - returns number of layer with max value. 
	t_aloc <- clusterR(t_aloc_tmp , reclassify, args=list(rcl =  matrix (c(1:nlayers(p_suit.N), lc.N), ncol=2))) # only the final map...
	
    # update logfile
    log_tmp <- c( u , logfile[ind, pix_d_names], logfile[ind, adj.p_names], iterfinal)
	logfile <- rbind (logfile, log_tmp)
	
	break;
    }
	
    #initialize next u sequence
    u=u+1;
}
endCluster()

return (list(t_aloc, logfile))
}