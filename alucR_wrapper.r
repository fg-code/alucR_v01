aluc <- function(lc, 						
                 suit, 						
                 natural.lc=c(),				
                 nochange.lc=c(),			
                 spatial=c(), 
                 demand=c(), 
                 elas=matrix(data=0, ncol=max(lc_unique), nrow=max(lc_unique)), 
                 traj=matrix(data=1, ncol=max(lc_unique), nrow=max(lc_unique)), 
                 init.years= 5,  
                 method = "competitive",
				 rule.mw = c()
                 stop.crit=c(0.0003 , 1, 10),
                 iter.max=100, 
                 ncores=detectCores()/2, 
                 print.log=TRUE, 
                 print.plot=FALSE, 
                 writeRaster=FALSE){
		
		epoche=1
		logfile1 <- c()
		while (epoche <= nrow(demand)){
		
		print(paste("EPOCHE:", epoche , "Date:", date() ,sep=" "))
		# variables as input for all functions 
		#Only at the beginning calculated
		
		if (epoche==1){
			#names of land use classes (those of the suitability stack) to be modelled
			lu_suit <- as.numeric(gsub("lc","",colnames(p_vector)))   # tolower(colnames(p_vector))
			
			# no change classes
			no.change <- ifelse(length(nochange.lc )> 0 ), as.numeric(gsub("lc","",nochange.lc)), c())
			
			#natural land cover classes (as.numeric)
			natural <- if(length(natural.lc)>0 ,as.numeric(gsub("lc","",natural.lc)), c())
			
			# same as length (lu_suit) number of suitability layers to be modeled
			#lu_suit_l <- ncol(p_vector)
			
			# number of pixel for all land use/cover classes within the initial lland use/cover raster (excluding NA)
			lc_pix <- tabulate(data_vector, nbins=max(data_vector, na.rm=TRUE)) 
			
			# total amount of pixels (excl. NAs)
			lc_n <- sum(lc_pix)
			
			# unique classes land use/cover classes
			lc_unique <- sort(unique(data_vector))
			
			# +1 pseudo natural layer for allocation algorithm
			pseudo.N <- max(lc_unique) + 1
			lu.N <- c(lu_suit,  pseudo.N) # class numbers  of all classes to be modelled (incl. pseudo natural class)   
		}
		
		# rule.mw 
		if (length(nrow(rule.mw)) > 0 ){
			p_raster <- alucR_rule.mw(lc=lc, suit=suit, rule.mw=rule.mw)
		} else { 
			p_raster <- alucR_prep1 <- (suit=suit)
		} 	
					
		#raster prep
		prep <- raster.prep (lc=lc, p_raster, natural.lc ,nochange.lc, spatial, demand, elas, traj, init.years, ncores)
		
		#demand prep
		demandE.new <- demand.prep ()
		
		#allocation
		print("start allocation")
		if (method == "hierarchical"){
		allocation <- allocation.hierarchical ()
		} 
		if (method == "competitive"){
		allocation <- allocation.competitive ()
		}
		
		logfile1  <- rbind (logfile1 , allocation[[2]]) 
		
		# post processing: spatial restrictions, nochange lc, natural lc module
		post.process <- raster.post.process
		
		lc <- subset(post.process, subset=1)
		init.years <- subset(post.process , subset=2)

		assign(paste("scenario", epoche, sep=""), lc)

		print("epoche done")
		#initialize new epoche
		epoche <- epoche+1
		if (print.plot==TRUE){plot(lc)}
		} # end of epoche loop 
		out <- list(stack (mget (paste("scenario", rep(1:nrow(demand)),sep=""))), logfile1))
		return (out)
}