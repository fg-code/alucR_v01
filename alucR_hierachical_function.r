
# hierarchical allocation works on matrix only
# suit.N
# lc 
# lu.N has to be defined earlier
# demandE.new
# print.log


allocation.hierarchical <- function (suit.N, lc, lu.N , demandE.new,  print.log=TRUE) {
  suit.N_vector <- getValues(suit.N)
  tprop_vector <- rep(NA,times=nrow(suit.N_vector) ) 
  demand.init <- demandE.new 
  logfile.tmp<- c()
  
  if  (print.log == TRUE){ pb <- txtProgressBar(min=0, max(length(lu.N)), style=3)}  
  for (i in 1:length (lu.N)) {
    ind <-  order(suit.N_vector[,i],decreasing =TRUE ,na.last=NA)
    if (demandE.new[order(lu.N)[i]] > length(ind)){
      demandE.new[order(lu.N)[i]] <- length(ind)
      print("not enough pixel for allocation")
    }
    tprop_vector [ind [1:demandE.new[order(lu.N)[i]]]] <- lu.N[i]
    suit.N_vector [ind [1:demandE.new[order(lu.N)[i]]],-i] <- NA
    if (print.log == TRUE){
      setTxtProgressBar(pb, i)
    }
  } 
  logfile.tmp <- (demandE.new - demand.init)
  if (print.log == TRUE){
    setTxtProgressBar(pb, i)
    close(pb)
    print (logfile.tmp)
  }
  out <- raster(lc, tprop_vector)
  return(list(out, logfile.tmp))
}