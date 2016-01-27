# outputs:
# scenario
# init.years (new)
 

# alloc | raster returned from allocation routine 


raster.post.process <- function (alloc == allocation [[1]],lc , natural.lc ,nochange.lc, spatial, traj, init.years, pseudo.N, filename='', ...){
					#initial variables
                    #lu_classes <- as.numeric(gsub("lu", "", names(demandE)))
                    if (length (natural.lc) > 0) { natural <- as.numeric(gsub("lc", "", natural.lc))}
                    if (length(nochange.lc) > 0 ){ no.change <- as.numeric(gsub("lc", "", nochange.lc))}
                    #lc_unique <- unique(lc)
                    #lc_n <- sum (tabulate (getValues (lc)), nbins=max(lc_unique) ,na.rm=TRUE)
						   
					# trajectory matrix 
					if (length(traj[traj==0 | is.na(traj)] )>0){
					# for not allowed changes (100 years more than modelling years)
					traj[traj==0 | is.na (traj)] <- nrow(demand)+ 100 
					}
						
					#raster settings	
					out.stack <- stack(alloc, init.years)
					out <- brick(out.stack, values=FALSE)
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
					v.alloc <- getValues(alloc, row=bs$row[i], nrows=bs$nrows[i] )
					v.init.years <- getValues(init.years, row=bs$row[i], nrows=bs$nrows[i] )
					if(length (spatial) > 0 ){  v.spatial <- getValues(spatial, row=bs$row[i], nrows=bs$nrows[i] )}
										
					#process
					#no.change
					if (length(nochange.lc) > 0 ){
						nochange_index <- is.element(v.lc, nochange.lc)  
						v.alloc[nochange_index] <- v.lc [nochange_index]
					}
					#spatial restrictions
					if (length(v.spatial) > 0 ){
						sp.rest_index <- which(!is.na(v.spatial)) # set those to NA which have a value in the restriction layer
						v.alloc[sp.rest_index] <- v.lc [sp.rest_index]
					}
					#  3.3 Natural vegetation and succession
					#####
					#  	3.3.1 Reclassify all natural to pseudo natural classes
					if (length (natural.lc) > 0 ){
					v.alloc [is.element(v.alloc, natural)] <- pseudo.N # natural vegetation to pseudo.N class (including areas of spatial restrictions)
					#  	3.3.2 Reclassify pseudo natural class based on trajectory and succession order
					#####
					pseudo.index <- which(is.element(v.alloc, pseudo.N))
					if (length (pseudo.index) > 0 ){  
					if(length (natural > 1)){
					for (j in 1:length(pseudo.index)){
					#can before.n be translated to natural 
					for (a in length(natural):2){
					if (traj[natural[a], natural[a-1]] < v.init.years[pseudo.index[j]] |
						v.lc[pseudo.index[j]] == natural[a-1]) {
						v.alloc[pseudo.index[j]] <- natural[a-1]
					} else{
					v.alloc[pseudo.index[j]] <- natural[a]
					} 
					}
					}
					} 
					if (length (natural)== 1) {v.alloc[pseudo.index[j]] <- natural}
					if (sum(is.element (v.alloc, pseudo.N))!= 0) {print( "error in natural vegetation module")}
					}}
    
					#####
					#  3.4 Saving results and preparing next epoche
					#####
					#  	3.4.1 Updating transition years vector
					#####
					v.init.years <- ifelse(v.alloc==v.lc, v.init.years + 1, 1) #compare this allocation for transition years, inc if changed, reset to 1 if change
					
					v <- cbind(v.alloc, v.init.years)
					
					#### save to raster
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