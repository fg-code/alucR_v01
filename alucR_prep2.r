

# lc | raster layer, integer values
# suit | raster stack,
# natural.lc | character string defining natural land cover trajectories, i.e. c("lc1", "lc2")
# nochange.lc | character string defining lc of no change , i.e. c("lc5", "lc6", "lc8")
# spatial | raster layer defining protected areas
# demand | data.frame with demand for each land use class, columns refer to lu, i.e. names ("lc7", "lc4", "lc3"), while sorting to hierarchy between lu classes
# elas | vector of elasticity values for each lc, between 0 and 1
# traj | matrix of trajectories of land use cover change
# init.years | raster layer, vales integer, referring to the number of years since the last lc change
# n.cores | not yet implemented
# filename | optional file name for preprocessed raster
#  ... | additional raster functions


alucR_prep2 <- function (lc, suit, natural.lc ,nochange.lc, spatial, demand, elas, traj, init.years, lc_unique, lu_suit, natural, ncores, filename='', ...) {
  # trajectory matrix 
  if (length(traj[traj==0 | is.na(traj)] )>0){
    # for not allowed changes (100 years more than modelling years)
    traj[traj==0 | is.na (traj)] <- nrow(demand)+ 100 
  }
  
  # raster data function preps
  out.n <- setValues(lc ,0.5) ; names(out.n)<- "lcN"
  suit.n <- stack (suit, out.n) 
  # chunk prep
  out <- brick(suit.n, values=FALSE)
  
  small <- canProcessInMemory(out, 3)
  filename <- trim(filename)
  
  if (!small  & filename == '' ){
    filename <- rasterTmpFile()
  }
  if (filename != ''){
    out <- writeStart(out, filename, ...)
    todisk <- TRUE
  } else{
    vv <- array(dim=dim(out)) 
    todisk <- FALSE
  }
  
  bs <- blockSize(out)
  pb <- pbCreate(nsteps= bs$n, ...)
  
  # chunk processing	
    for (i in 1:bs$n) {
    #read chunks
      v.lc <- getValues(lc, row=bs$row[i], nrows=bs$nrows[i] )
      v.suit <- getValues(suit , row=bs$row[i], nrows=bs$nrows[i] )
      if (length (spatial) > 0) { v.spatial <- getValues(spatial, row=bs$row[i], nrows=bs$nrows[i] )}
      v.init.years <- getValues(init.years, row=bs$row[i], nrows=bs$nrows[i] )
      if(length (natural) >=1 ){
        v.natural<- getValues(out.n, row=bs$row[i], nrows=bs$nrows[i]) # natural vegetation vector 
        v.natural[is.na(v.lc)]<- NA
      }
      #process
      #no.change
      if (length(nochange.lc) > 0 ){
        nochange_index <- is.element(v.lc, nochange.lc)  
        v.suit[nochange_index, ] <- NA
        if(length (natural) >=1 ){
        v.natural[nochange_index]  <- NA  # include if exists
        }
      }
      
      #spatial restrictions
      if (length(v.spatial) > 0 ){ # make sure to 
        sp.rest_index <- which(!is.na(v.spatial)) # set those to NA which have a value in the restriction layer
        v.suit[sp.rest_index,] <- NA
        if(length (natural) >=1 ){
        v.natural [sp.rest_index] <- NA
        }
      } else { sp.rest_index <- c()}
      
      #Trajectories of land use change
      #####
      # general:
      # transitions which are not allowed are set to NA in the respective suitability layer (target)
      # transitions different to 1, referring to transition possible after one iteration (year) are identified 
      # those identified are checked against the transition years vector. if years < transition years the target suitability is set to NA
      # specific steps: 
      
      # conversion restrictions from all land covers to the  land use classes (suitability layer)
      # for all unique land cover classes to land use classes 
      
      for (j in 1:length(lc_unique)){ 
        # identify classes with restricted trajectories to land use
        traj_ind <-  which(traj[lc_unique[j],lu_suit] != 1) # fuer 1 an der stelle 2
        # in case no restriction due to trajectories apply 
        if (length(traj_ind) > 0){  
          # index classes with restricted trajectories
          cat_index <- which(v.lc==lc_unique[j])  # fuer 2 an der stelle 1
          for (a in 1:length(traj_ind)){
            # set v.suit at the specific location for the specific layer  to NA if the amount of years is not reached
            v.suit[ cat_index, traj_ind[a]]<- ifelse (v.init.years[cat_index] < traj[lu_suit [traj_ind[a]], lc_unique[j]], NA, v.suit[ cat_index, traj_ind[a]])
          }
        }
      }
      if(length (natural) >=1 ){
      # conversion restrictions from natural vegetation class to any other land cover class
      lc.nonatural <-  lc_unique [-(natural)]
      for (j in 1:length(lc.nonatural)){
        traj_ind <- which(is.element (1 ,  traj[natural,lc.nonatural[j]])==FALSE) # identify which trajectories are unequal 1 (are not allowed after one year)
        if (length(traj_ind) > 0){ 
          cat_index <- which(v.lc==lc.nonatural[j])
          v.natural[cat_index] <- ifelse (v.init.years[cat_index] < min(traj[lc.nonatural[j], natural]), NA, v.natural[cat_index])
        }
      }	
      }
      #####
      #combine suit and natural
      ####
      if(length (natural) >=1 ){
        v <- cbind(v.suit, v.natural)
      } else {
        v <- v.suit
      }
      
      # write to raster
      if (todisk) {
        out <- writeValues(out, v, bs$row[i])
        } else {
          cols <- bs$row[i]:(bs$row[i]+bs$nrows[i]-1)
          for (k in 1:dim(out)[3]){
            vv.t <- t(matrix( v[, k], nrow=dim(out)[2]))
            vv [cols,, k] <- vv.t
          }
         }
      pbStep(pb, i)
      }
      if (todisk) {
      out <- writeStop(out)
      } else {
      out <- setValues(out, vv)
      }
  pbClose(pb)
  return(out)
}


	
	