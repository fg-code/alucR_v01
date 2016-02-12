

# lc | raster layer, integer values
# p_raster | raster stack,
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


alucR_prep2 <- function (lc, p_raster, spatial, init.years, var.list, epoche =epoche ,  elas, traj,  filename='', ...) {
  # extract variables from var.list
	lu_suit <-   var.list [[1]][["lu_suit"]]
	lu_slookup <- var.list [[]] [["lu_slookup"]]
	nochange <- var.list [[2]][["nochange"]]
	lc_unique <- var.list [[7]][["lc_unique"]]
	lc_lookup <- var.list [[7]][["lc_lookup"]]
	natural <- var.list [[]][["natural"]]
  
  # check nochange.lc and p_raster - nochange.lc classes cannot be included in p_raster
  if (any(match (lu_suit, nochange)){
	drop.layer <- lu_suit [is.element (lu_suit,nochange)] 
	p_raster <- droplayer (p_raster, drop.layer)
  }

  # read protected areas raster in case it it is defined differently for each epoche
	if (length (spatial)> 0){
		if (class(spatial)=="character"){
		get(spatial[epoche])# in case different stacks for each episode are specified - possibly useful if the protected area network will be expanded during the modelling experiment
		} # else it is defined as raster in the input to the function 
}
	
 # land use history 
 if (epoche == 1){
  init.years <- if (class(init.years)=="RasterLayer"){
        init.years
        } else {
			setValues (lc, as.numeric(init.years))
	}}
  
	# check if rasters belong to the same projection, have same extend and origine

	if (extent(lc) != extent (p_raster)){print ("Raster extents do not match (lc!=suit)")}
	if (proj4string(lc) != proj4string (p_raster)){print ("Raster projections do not match (lc!=suit)")}
	if (origine(lc) != origine (p_raster)){print ("Raster origine's do not match (lc!=suit)")}
	
	if (length (spatial)> 0){ 
	if (extent(lc) != extent (spatial)){print ("Raster extents do not match (lc!=spatial)")}
	if (proj4string(lc) != proj4string (spatial)){print ("Raster projections do not match (lc!=spatial)")}
	if (origine(lc) != origine (spatial)){print ("Raster origine's do not match (lc!=spatial)")}
	}

	if (extent(lc) != extent (init.years)){print ("Raster extents do not match (lc!=init.years)")}
	if (proj4string(lc) != proj4string (init.years)){print ("Raster projections do not match (lc!=init.years)")}
	if (origine(lc) != origine (init.years)){print ("Raster origine's do not match (lc!=init.years)")}

	
  # if natural suitability required 
  if (length (natural) > 0){ 
	out.n <- setValues(lc ,0.5) ; names(out.n)<- "lcN"
	p_raster <- addlayer (p_raster, out.n) 
	}

  # chunk prep
  out <- brick(p_raster, values=FALSE)
  
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
      v.suit <- getValues(p_raster , row=bs$row[i], nrows=bs$nrows[i] )
      if (length (spatial) > 0) { v.spatial <- getValues(spatial, row=bs$row[i], nrows=bs$nrows[i] )}
      v.init.years <- getValues(init.years, row=bs$row[i], nrows=bs$nrows[i] )
      if(length (natural) > 0 ){
        v.natural<- getValues(out.n, row=bs$row[i], nrows=bs$nrows[i]) # natural vegetation vector 
        v.natural[is.na(v.lc)]<- NA
      }
      #process
      #no.change classes masked from suitability 
      if (length(nochange) > 0 ){
        nochange_index <- is.element(v.lc, nochange)  
        v.suit[nochange_index, ] <- NA
        if(length (natural) > 0 ){
        v.natural[nochange_index]  <- NA  # include if exists
        }
      }
      
      #spatial restrictions masked from suitability
      if (length (spatial)> 0){ # make sure to 
        sp.rest_index <- which(!is.na(v.spatial)) # set those to NA which have a value in the restriction layer
        v.suit[sp.rest_index,] <- NA
        if(length (natural) > 0 ){
        v.natural [sp.rest_index] <- NA
        }
      } else { sp.rest_index <- c()}
      
      
#####
# elasticities Matrix for suitability classes 
####	
# for suitabilities
    for (i in 1:length (lc_unique)){
      # identify classes changes in probaility due to elas
      elas_ind <-  which(elas[lc_lookup[i],lu_slookup] != 0) #
      # in case no elasticities apply for the conversion probability
      if (length(elas_ind) > 0){  
        # index cases where elasticities apply
        cat_index <- which(v.lc==lc_unique[i])
        if (length (cat_index) > 0 ){
        for (a in 1:length(elas_ind)){
          v.suit[cat_index, elas_ind[a]] <- v.suit[ cat_index, elas_ind[a]] + elas [lc_lookup[i], lu_slookup[elas_ind[a]]] 
         }
        }
      }
    }
    
# for natural land cover
    if (length (natural) > 0 ){
      for (i in 1:length(lc_unique)){
        # identify classes with restricted trajectories to land use
        elas_ind <-  which(elas[lc_lookup[i],natural] != 0) # 
        # in case no elasticities apply for the conversion probability
        if (length(elas_ind) > 0){  
          # index cases where elasticities apply
          cat_index <- which(v.lc==lc_unique[i])  
          if (length (cat_index) > 0 ){
          v.natural[cat_index] <- v.natural[ cat_index] + max( elas [lc_unique[i],natural[elas_ind]])
        }
      }
    }
    } 

	  
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
        traj_ind <-  which(traj[lc_lookup[j],lu_suit] != 1) # fuer 1 an der stelle 2 - lookup table in case classes start with 0
        # in case no restriction due to trajectories apply 
        if (length(traj_ind) > 0){  
          # index classes with restricted trajectories
          cat_index <- which(v.lc==lc_unique[j])  # fuer 2 an der stelle 1
          for (a in 1:length(traj_ind)){
            # set v.suit at the specific location for the specific layer  to NA if the amount of years is not reached
            v.suit[ cat_index, traj_ind[a]]<- ifelse (v.init.years[cat_index] < traj[lu_suit [traj_ind[a]], lc_lookup[j]], NA, v.suit[ cat_index, traj_ind[a]])
          }
        }
      }
      if(length (natural) > 0 ){
      # conversion restrictions from any class to natural vegetation class 
      lc.nonatural <-  lc_nonatural [-(natural)] #any classes except of natural
	  lc.nonalookup <- lc_lookup [-(natural)] 
      for (j in 1:length(lc.nonatural)){
        traj_ind <- which(is.element ( 1 ,  traj[natural,lc.nonalookup[j]])==FALSE) # identify which trajectories are unequal 1 (are not allowed after one year) # lookup table in case classes start at 0
        if (length(traj_ind) > 0){ 
          cat_index <- which(v.lc==lc.nonatural[j])
          v.natural[cat_index] <- ifelse (v.init.years[cat_index] < min(traj[lc.nonalookup[j], natural]), NA, v.natural[cat_index])
        }
      }	
      }
      #####
      #combine suit and natural
      ####
      if(length (natural) > 0 ){
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


	
	