# Date: 15.03.2016 florian.gollnow@geo.hu-berlin.de


# suit | preprocessed suitabilities demandE | vector of demand for the relevant epoche demandC | vector of demand change from one epoche to the next iter.max
# | maximum iterations before best allocation from the temporary allocation is chosen (in case stop.cript was not meet) stop.crit | stoping criteria defined
# as a vector with two values: first one refers to max percent difference from change between two epoches, second to the maximum pixel difference allowed. If
# any is reached the allocation stops print.log | print.plot |

# returns a list. at position [[1]] a ratsre with the allocated land use classes, at [[2]] a data frame with the lofile informations from the iterations.


# require(raster) install.packages('snow') library(snow)


alucR_competitive <- function(suit, demandE, demandC, var.list, iter.max, stop.crit, ncores = ncores, print.log = FALSE, print.plot = FALSE)
{
    lc_n <- var.list[[2]] [["lc_n"]] 
    lc_suit <- var.list[[4]][["lc_suit"]]
    pseudo.N <- var.list[[8]][["pseudo.N"]]
    lc.N <- var.list[[9]][["lc.N"]]

    min.demand <- which.min(demandE)
    
    if (nlayers(suit) != length(demandE))
    {
       cat("\n", "Warning: layers of suit != length of demand") 
      #print("Warning: layers of suit != length of demand")
    }
    
    logfile <- data.frame(matrix(data = NA, ncol = (1 + length(lc.N) + length(lc.N) + length(lc.N)), nrow = 1))
    pix_d_names <- paste("pix_d", lc.N, sep = "")
    adj.p_names <- paste("next.adj.p", lc.N, sep = "")
    iter_names <- paste("iter", lc.N, sep = "")
    names(logfile) <- c("u", pix_d_names, adj.p_names, iter_names)
    
    iter <- rep(0, times = length(lc.N))
    
    # start allocation
    u <- 1
    # beginCluster(n=ncores)
    repeat {
        ##### 
        if (u == 1)
        {
            p_suit <- suit
        } else
        {
            p_suit <- suit + as.numeric(iter)
        }
        # temporary allocation using clusterR and which.max
        beginCluster(n = ncores)
        t_aloc <- clusterR(p_suit, which.max)  # all NA which is not 'suit' or 'natural' - returns number of layer with max value. 
        endCluster()
        
        # assess number of pixels per suit class resulting from the temporary allocation
        f_class <- as.data.frame(freq(t_aloc, useNA = "no"))
        f_class$reclass <- lc.N[match(f_class$value, 1:nlayers(p_suit))]
        ind <- match(lc.N, f_class$reclass)
        t_class <- f_class[ind, "count"]
        if (any(is.na(t_class)))
        {
            t_class[is.na(t_class)] <- 0
        }
        
        # calc difference in pixel and percent todemand
        pix_d <- t_class - demandE  # make sure they always have the same dimensions
        perc_d <- (pix_d/demandE) * 100  # 
        
        #####new
        linearA <- abs(1/lc_n * pix_d)/2  # how much to increase weight to add, if we asume a linear increase, to get to the required pixel numbers
        
        if (u == 1)
        {
          # initializing adj.p
          #adj.p <- as.vector(ifelse(sign(pix_d) != 0, -1 * sign(pix_d) * linearA, 0), mode = "numeric")
          adj.p <- as.vector(ifelse(sign(pix_d) != 0, -1 * sign(pix_d) * 1/10, 0), mode = "numeric")
        } else { 
          
          signchange <- (logfile[u-1, adj.p_names]/(logfile[u-1, pix_d_names] - pix_d))  * pix_d
          
          
          #if (any(sign(pix_d) != sign(logfile[u - 1, pix_d_names]))){
          #  print(signchange)
          #} 
          
          better <- abs(pix_d) < abs(logfile[u - 1, pix_d_names]) 
          
          
          adj.p <- as.vector(ifelse(pix_d == 0, 0,   
                                    ifelse(pix_d < 0 & logfile[u - 1, adj.p_names] == 0, abs(adj.0* pix_d), #-1*sign(pix_d)* adj.0,
                                           ifelse(pix_d > 0 & logfile[u - 1, adj.p_names] == 0, -1*abs(adj.0* pix_d), 
                                                  ifelse(sign(pix_d) != sign(logfile[u - 1, pix_d_names]),  signchange/2, 
                                                         ifelse(better == TRUE & sign(logfile[u - 1, pix_d_names])== +1 ,  -1*  (abs(logfile[u - 1, adj.p_names]) *1.4) , #abs(logfile[u - 1, adj.p_names] * proportion4),#proportion /2), 
                                                                ifelse(better == TRUE & sign(logfile[u - 1, pix_d_names])== - 1 , logfile[u - 1, adj.p_names] *1.4 ,# abs(logfile[u - 1, adj.p_names] * proportion4),#proportion/2), 
                                                                       ifelse (better==FALSE & sign(logfile[u - 1, pix_d_names])== +1, -1* (abs(logfile[u - 1, adj.p_names]) *2),
                                                                               ifelse (better==FALSE & sign(logfile[u - 1, pix_d_names])== -1, abs(logfile[u - 1, adj.p_names]) *2, 
                                                                                       0)))))))), mode = "numeric")
          
        }  
        
        
        if (u > 1){
          if(any(pix_d==0)){
            if (all(logfile [u-1,pix_d_names[which(pix_d==0)]] !=0)){
              adj.0 <- abs(logfile[u-1,adj.p_names]/logfile [u-1,pix_d_names])
            }}}
        
        adj.p <- as.vector(ifelse(adj.p < -0.2, -0.2, ifelse(adj.p > 0.2, 0.2, adj.p)), mode = 'numeric' )  
        
        iter.last <- iter
        iter <- as.numeric(iter) + as.numeric(adj.p)
        iter <- as.vector (ifelse(iter < -2, -2, ifelse(iter > 2, 2, iter)), mode = 'numeric' )
        
        logfile [u,"u" ] <- u
        logfile [u,pix_d_names ] <- pix_d
        logfile [u, adj.p_names] <- adj.p
        logfile [u,iter_names ] <- iter.last
 
        if (print.log == TRUE)
        {
          #if(u==1){cat("\n", names(logfile))}
          cat("\n")
          print(logfile[u, ])
        }
        
        if (print.plot == TRUE)
        {
            plot(0, 0, xlim = c(2, iter.max), ylim = c(-1, 1), ylab = "iter", xlab = "iteration", type = "n")
            grid()
            names.legend <- paste("LC", c(lc.N))
            legend("topright", legend = names.legend, col = rainbow(lc.N), pch = 15)
            for (i in 1:length(lc.N))
            {
                lines(c(1:nrow(logfile)), logfile[, iter_names[i]], col = rainbow(lc.N)[i], type = "l", lwd = 2)
            }
        }
        
        # stop criteria meet?  are the min defined percent of change pixels allocated
        if (max(abs(pix_d[1:length(demandC)]/demandC) * 100, na.rm=TRUE) < stop.crit[1] & max(abs(pix_d[min.demand])) < stop.crit[2])
        {
            if (print.log == TRUE)
            {
               cat ("\n","Stop criterium: percent of change" ) 
            }
            beginCluster(n = ncores)
            aloc <- clusterR(t_aloc, reclassify, args = list(rcl = matrix(c(1:nlayers(p_suit), lc.N), ncol = 2)))
            endCluster()
            break
        }
        # is the max differences in pixel between allocated and demand smaller than defined
        if (max(abs(pix_d)) < stop.crit[2])
        {
            if (print.log == TRUE)
            {
              cat ("\n","Stop criterium: number of Pixel" )   
              #print("Stop criterium: number of Pixel")
            }
            beginCluster(n = ncores)
            aloc <- clusterR(t_aloc, reclassify, args = list(rcl = matrix(c(1:nlayers(p_suit), lc.N), ncol = 2)))
            endCluster()
            break
        }
        
        # stop argument iteration if ITERmax reached and take the ITER with the minimum deviation from the demand from all iterations
        if (u == iter.max)
        {
            if (print.log == TRUE)
            {
              cat ("\n","Stop criterium: number of max. iterations reached" )    
              #print("Stop criterium: number of max. iterations reached")
            }
            # which iteration had the smallest overall pixel difference
            ind <- which.min(rowSums(abs(logfile[, pix_d_names])))
            iterfinal <- logfile[ind, iter_names]
            
            p_suit <- p_suit + as.numeric(iterfinal)
            beginCluster(n = ncores)
            t_aloc <- clusterR(p_suit, which.max)  # all NA which is not 'suit' or 'natural' - returns number of layer with max value. 
            aloc <- clusterR(t_aloc, reclassify, args = list(rcl = matrix(c(1:nlayers(p_suit), lc.N), ncol = 2)))  # only the final map...
            endCluster()
            # update logfile
            log_tmp <- data.frame(u, logfile[ind, pix_d_names], logfile[ind, adj.p_names], iterfinal)
            logfile <- rbind(logfile, log_tmp)
            
            break
        }
        
        # initialize next u sequence
        u <- u + 1
    }
    # endCluster()
    out <- list(aloc, logfile)
    return(out)
}