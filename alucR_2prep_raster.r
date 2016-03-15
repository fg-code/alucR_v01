

#lc | initial land cover categories. 
#suit | RasterStack of suitabilities (derived from suit earlier) for the specific land use classes to model. Layer names "lc1", "lc2"... with the numbers refering to the land cover classes in the initial land cover map lc
#nochange.lc | (optional) vector of charachter naming the land cover classes which do not change during the modelling experiment example c("lc5","lc6") when class 5 refers for example to water and 5 and 6 refer to water. Nochange classes should not be included in the suit RasterStack (will be dropped if any)
#natural.lc | (optional) vector of charachter naming the land cover classes which refer to natural vegetation. for example c("lc1","lc2") when landcover 1 refers to Forest and land cover 2 to secondary vegetation 
# spatial | (optional) RasterLayer defining Protected Areas (no change allowed within these areas). Locations of NA represent areas outside Protected areas, location != NA represent areas of Protection. If nessesary you can also provide a vector of RasterLayer Object names instead to define different Protected Areas for different sceanario years
#elas | (optional, but recomendet) matrix of values between 0 and 1 referring to the conversion/trajectory elasticity of the land use/cover classes. Rows: initial land use/cover (1 to n), Columns: following land use/cover (1 to n). Definition 0: no change due to elasticities, 0.5: incresed likelyness for the class or conversion, 1: very high likelyness for the class or conversion.
#traj | (optional, but recomendet) matrix describing the temporal trajectories of land use/cover. Rows: initial land use/cover (1 to n), Columns: following land use/cover (1 to n). Values define the years of transition, e.g. 0: no transition allowed, 1: transition allowed after first iteration, 10: transition allowed after 10 iterations. must be specified for all land_cover classes.
# init.years | (optional) RasterLayer, vales integer, referring to the number of years since the last lc change. 

#OUT: adapted suitabilities


alucR_prep2  <- function(lc, suit, spatial, init.years, var.list, epoche=epoche ,  elas, traj, filename='', ...) {
  #prepare additional function input
  # extract variables from var.list
  lc_suit <-   var.list [[4]][["lc_suit"]]
  lc_slookup <- var.list [[4]] [["lc_slookup"]]
  nochange <- var.list [[5]][["nochange"]]
  lc_unique <- var.list [[3]][["lc_unique"]]
  lc_lookup <- var.list [[3]][["lc_lookup"]]
  natural <- var.list [[7]][["natural"]]
  naturallookup <-  var.list [[7]][["naturallookup"]]
  
  # check nochange.lc and suit - nochange.lc classes cannot be included in suit
  if (any(is.element (lc_suit,nochange))){
    drop.layer <- which(lc_suit == lc_suit[is.element (lc_suit,nochange)]) #which layer to drop cause defined as no change
    suit <- dropLayer (suit, drop.layer)
  }
  
  
  # preparing out raster  
  # if natural suitability required 
  if (length (natural) > 0){ 
    out.n <- setValues(lc ,0.5) ; names(out.n)<- "lcN"
    out_raster <- addLayer (suit, out.n) 
    out <- brick(out_raster, values=FALSE)
    #rm(suit)
  } else {
    assign ("out_raster", suit)
    out <- brick(out_raster, values=FALSE)
    #rm(suit)
  }
  
  
  # prepare chung processing
  big <- ! canProcessInMemory(out_raster, 3)
  
  filename <- trim(filename)
  if (big & filename == '') {
    filename <- rasterTmpFile()
  }
  if (filename != '') {
    out <- writeStart(out, filename, ...)
    todisk <- TRUE
    
  } else {
    #vv <- matrix(ncol=dim (out)[3], nrow=ncol(out)*nrow(out))
    todisk <- FALSE
  }
  
  bs <- blockSize(out_raster)
  pb <- pbCreate(bs$n, ...)
  
  
  # start chunk processing
  if (todisk) {
    for (i in 1:bs$n) {
      
      v.lc <- getValues(lc, row=bs$row[i], nrows=bs$nrows[i] )
      v.suit <- getValues(suit , row=bs$row[i], nrows=bs$nrows[i] )
      v.suit[is.na(v.lc)] <- NA
      if (length (spatial) > 0) { 
        v.spatial <- getValues(spatial, row=bs$row[i], nrows=bs$nrows[i] )
      }
      v.init.years <- getValues(init.years, row=bs$row[i], nrows=bs$nrows[i] )
      if(length (natural) > 0 ){
        v.natural<- getValues(out.n, row=bs$row[i], nrows=bs$nrows[i]) # natural vegetation vector 
        v.natural[is.na(v.lc)] <- NA
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
        sp.rest_index <- !is.na(v.spatial) # set those to NA which have a value in the restriction layer
        v.suit[sp.rest_index,] <- NA
        if(length (natural) > 0 ){
          v.natural [sp.rest_index] <- NA
        }
      } 
      
      
      #########
      # for suitabilities
      for (b in 1:length (lc_unique)){
        # identify classes changes in probaility due to elas
        elas_ind <-  which(elas[lc_lookup[b],lc_slookup] != 0)
        # in case no elasticities apply for the conversion probability
        if (length(elas_ind) > 0){  
          # index cases where elasticities apply
          cat_index <- which(v.lc==lc_unique[b])
          if (length (cat_index) > 0 ){
            for (a in 1:length(elas_ind)){
              v.suit[cat_index, elas_ind[a]] <- v.suit[ cat_index, elas_ind[a]] + elas [lc_lookup[b], lc_slookup[elas_ind[a]]] 
            }
          }
        }
      }
      
      
      # for natural land cover
      if (length (natural) > 0 ){
        for (d in 1:length(lc_unique)){
          # identify classes with restricted trajectories to land use
          elas_ind <-  which(elas[lc_lookup[d],naturallookup] != 0) # 
          # in case no elasticities apply for the conversion probability
          if (length(elas_ind) > 0){
            # index cases where elasticities apply
            cat_index <- which(v.lc==lc_unique[d]) 
            if (length (cat_index) > 0 ){
              v.natural[cat_index] <- v.natural[ cat_index] + max( elas [lc_unique[d],naturallookup[elas_ind]])
            }
          }
        }
      } 
      
      #########
      #####
      #  1.7 Trajectories of land use change
      #####
      # general:
      # transitions which are not allowed are set to NA in the respective suitability layer (target)
      # transitions different to 1, referring to transition possible after one iteration (year) are identified 
      # those identified are checked against the transition years vector. if years < transition years the target suitability is set to NA
      # specific steps: 
      # first edit trajectory matrix
      if (length(traj[traj==0 | is.na(traj)] )> 0){
        # for not allowed changes (100 years more than modelling years)
        traj[traj==0 | is.na (traj)] <- nrow(demand)+ 100 
      }
      #####
      #  	1.7.1 Trajectories for land use classes
      #####
      ##
      ##DataSpecific
      ##increase suitability for croplands on current pasture areas by 0.5
      #ind_past <- which (tprop.previous_vector==3)
      #tprop.previous_vector[ind_past] <- tprop.previous_vector[ind_past] + 0.5
      ##increase suitability for pasture on current croplands areas by 0.5
      #ind_crop <- which (tprop.previous_vector==4)
      #tprop.previous_vector[ind_crop] <- tprop.previous_vector[ind_crop] + 0.5
      # 
      #restricted conversions - from any land use civer class to land use
      ###trajectories
      for (j in 1:length(lc_unique)){ 
        # identify classes with restricted trajectories to land use
        traj_ind <-  which(traj[lc_lookup[j],lc_slookup] != 1) # fuer 1 an der stelle 2 - lookup table in case classes start with 0
        #traj_ind
        if (length(traj_ind) > 0){  
          cat_index <- which(v.lc==lc_unique[j])  
          if (length(cat_index) < 0 ){
            for (a in 1:length(traj_ind)){
              # set p_vector at the specific location for the specific layer  to NA if the amount of years is not reached
              v.suit[ cat_index, traj_ind[a]]<- ifelse (v.init.years[cat_index] < traj[lc_lookup[j], lc_slookup [traj_ind[a]]], NA, v.suit[ cat_index, traj_ind[a]])
            }
          }
        }
      } 
      # conversion restrictions from any land use cover class to natural vegetation
      if(length (natural) > 0 ){
        lc.nonatural <- lc_unique [-naturallookup]
        lc.nonalookup <- lc_lookup [-naturallookup] 
        for (j in 1:length(lc.nonalookup)){
          traj_ind <- which(is.element ( 1 ,  traj[naturallookup,lc.nonalookup[j]])==FALSE) # identify which trajectories are unequal 1 (are not allowed after one year) # lookup table in case classes start at 0
          if (length(traj_ind) > 0){ 
            cat_index <- which(v.lc==lc.nonatural[j])
            v.natural[cat_index] <- ifelse (v.init.years[cat_index] < min(traj[lc.nonalookup[j], naturallookup]), NA, v.natural[cat_index])
          }
        }
      }
      #########
      if(length (natural) > 0 ){
        v <- cbind(v.suit, v.natural)
      } else {
        v <- v.suit
      }
      out <- writeValues(out, v, bs$row[i])
      pbStep(pb, i)
    }
    out <- writeStop(out)
  } else { # if(todisk==FALSE)
    #####################
    #for (i in 1:bs$n) {
    #v.lc <- getValues(lc, row=bs$row[i], nrows=bs$nrows[i] )
    v.lc <- getValues(lc)
    #v.suit <- getValues(suit , row=bs$row[i], nrows=bs$nrows[i] )
    v.suit <- getValues(suit)
    v.suit[is.na(v.lc)] <- NA
    if (length (spatial) > 0) { 
      #v.spatial <- getValues(spatial, row=bs$row[i], nrows=bs$nrows[i] )
      v.spatial <- getValues(spatial)
    }
    #v.init.years <- getValues(init.years, row=bs$row[i], nrows=bs$nrows[i] )
    v.init.years <- getValues(init.years)
    if(length (natural) > 0 ){
      #v.natural<- getValues(out.n, row=bs$row[i], nrows=bs$nrows[i]) # natural vegetation vector 
      v.natural<- getValues(out.n)
      v.natural[is.na(v.lc)] <- NA
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
      sp.rest_index <- !is.na(v.spatial) # set those to NA which have a value in the restriction layer
      if (length(sp.rest_index)> 0){
        v.suit[sp.rest_index,] <- NA
        if(length (natural) > 0 ){
          v.natural [sp.rest_index] <- NA
        }
      } 
    }
    
    #########
    # for suitabilities
    for (b in 1:length (lc_unique)){
      # identify classes changes in probaility due to elas
      elas_ind <-  which(elas[lc_lookup[b],lc_slookup] != 0)
      # in case no elasticities apply for the conversion probability
      if (length(elas_ind) > 0){  
        # index cases where elasticities apply
        cat_index <- which(v.lc==lc_unique[b])
        if (length (cat_index) > 0 ){
          for (a in 1:length(elas_ind)){
            v.suit[cat_index, elas_ind[a]] <- v.suit[ cat_index, elas_ind[a]] + elas [lc_lookup[b], lc_slookup[elas_ind[a]]] 
          }
        }
      }
    }
    
    
    # for natural land cover
    if (length (natural) > 0 ){
      for (d in 1:length(lc_unique)){
        # identify classes with restricted trajectories to land use
        elas_ind <-  which(elas[lc_lookup[d],naturallookup] != 0) # 
        # in case no elasticities apply for the conversion probability
        if (length(elas_ind) > 0){
          # index cases where elasticities apply
          cat_index <- which(v.lc==lc_unique[d]) 
          if (length (cat_index) > 0 ){
            v.natural[cat_index] <- v.natural[ cat_index] + max( elas [lc_unique[d],naturallookup[elas_ind]])
          }
        }
      }
    } 
    
    #########
    #####
    #  1.7 Trajectories of land use change
    #####
    # general:
    # transitions which are not allowed are set to NA in the respective suitability layer (target)
    # transitions different to 1, referring to transition possible after one iteration (year) are identified 
    # those identified are checked against the transition years vector. if years < transition years the target suitability is set to NA
    # specific steps: 
    # first edit trajectory matrix
    if (length(traj[traj==0 | is.na(traj)] )> 0){
      # for not allowed changes (100 years more than modelling years)
      traj[traj==0 | is.na (traj)] <- nrow(demand)+ 100 
    }
    #####
    #  	1.7.1 Trajectories for land use classes
    #####
    ##
    ##DataSpecific
    ##increase suitability for croplands on current pasture areas by 0.5
    #ind_past <- which (tprop.previous_vector==3)
    #tprop.previous_vector[ind_past] <- tprop.previous_vector[ind_past] + 0.5
    ##increase suitability for pasture on current croplands areas by 0.5
    #ind_crop <- which (tprop.previous_vector==4)
    #tprop.previous_vector[ind_crop] <- tprop.previous_vector[ind_crop] + 0.5
    # 
    #restricted conversions - from any land use civer class to land use
    ###trajectories
    for (j in 1:length(lc_unique)){ 
      # identify classes with restricted trajectories to land use
      traj_ind <-  which(traj[lc_lookup[j],lc_slookup] != 1) # fuer 1 an der stelle 2 - lookup table in case classes start with 0
      #traj_ind
      if (length(traj_ind) > 0){  
        cat_index <- which(v.lc==lc_unique[j])  
        if (length(cat_index) < 0 ){
          for (a in 1:length(traj_ind)){
            # set p_vector at the specific location for the specific layer  to NA if the amount of years is not reached
            v.suit[ cat_index, traj_ind[a]]<- ifelse (v.init.years[cat_index] < traj[lc_lookup[j], lc_slookup [traj_ind[a]]], NA, v.suit[ cat_index, traj_ind[a]])
          }
        }
      }
    } 
    # conversion restrictions from any land use cover class to natural vegetation
    if(length (natural) > 0 ){
      lc.nonatural <- lc_unique [-naturallookup]
      lc.nonalookup <- lc_lookup [-naturallookup] 
      for (j in 1:length(lc.nonalookup)){
        traj_ind <- which(is.element ( 1 ,  traj[naturallookup,lc.nonalookup[j]])==FALSE) # identify which trajectories are unequal 1 (are not allowed after one year) # lookup table in case classes start at 0
        if (length(traj_ind) > 0){ 
          cat_index <- which(v.lc==lc.nonatural[j])
          v.natural[cat_index] <- ifelse (v.init.years[cat_index] < min(traj[lc.nonalookup[j], naturallookup]), NA, v.natural[cat_index])
        }
      }
    }
    if(length (natural) > 0 ){
      #v <- cbind(v.suit, v.natural)
      vv <- cbind(v.suit, v.natural)
    } else {
      #v <- v.suit
      vv <- v.suit
    }
    #cols <- bs$row[i]:(bs$row[i]+bs$nrows[i]-1)
    
    #vv[cols,] <- matrix(v)
    
    pbStep(pb, i)
    #}
    
    out <- setValues(out, vv)
  }
  pbClose(pb)
  #return raster 
  return(out)
}
