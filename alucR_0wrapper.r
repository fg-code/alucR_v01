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
                 stop.crit=c(0.10 , 10),
                 iter.max=100, 
                 ncores=(detectCores()-1), 
                 print.log=TRUE, 
                 print.plot=FALSE, 
                 writeRaster=FALSE){
		
	#Initial Values
	epoche=1
	logaluc <- c()
		
	if (length (init.years)==1 ){
		init.hist <- setValues (lc, init.years)
	} else	{
		assign ("init.hist", init.years)
	}
	
	# updated values
	while (epoche <= nrow(demand)){
	print(paste("EPOCHE:", epoche , "Date:", date() ,sep=" "))
	
	#read data if provided as list of RasteStacks (possible for suit) or character string (possible for spatial) 
	if (class(suit)=="list"){
		p_raster <- suit[[epoche]]
	} else { 
		assign ("p_raster", suit) 
	}
	
	if (length(spatial)>0){ # only if spatial restrictions are defined
	if (class(spatial)=="character"){
		spatially <- get(spatial[epoche])
	}else{
	if (epoche==1){
		assign("spatially", spatial)
	}}
			
	#check for major input assumptions
	alucR_check_input(lc, p_raster, spatially, init.years, demand, nochange.lc, natural.lc)
			
	#variables as input for all functions 
	#Only at the beginning calculated
	if (epoche==1){
	var.list <- alucR_prep1 (lc=lc, p_raster=p_raster, nochange.lc=nochange.lc, natural.lc)
	}
	
	# rule.mw 
	if (nrow(rule.mw) > 0 ){
		p_raster <- alucR_rule.mw(lc=lc, p_raster=p_raster, rule.mw=rule.mw)
	} 
				
	#raster prep
	p_raster <- alucR_prep2 (lc=lc, p_raster=p_raster, spatially=spatially, init.years=init.years, var.list=var.list, epoche=epoche ,  elas=elas, traj=traj, filename='')
	
	#demand prep
	demandList <- alucR_demand.prep(demand=demand , lc=lc, spatially=spatially, varl.list=varl.list, epoche=epoche)
	
	#allocation
	#print("start allocation")
	#if (method == "hierarchical"){
	#allocation <- allocation.hierarchical ()
	#} 
	if (method == "competitive"){
	allocation <- alucR_competitive(p_raster, demandE=demandList[[1]], demandC=demandList[[2]], var.list, max.iter=100, stop.crit=c(), print.log==FALSE, print.plot==FALSE )
	}
	if (epoche==1){
	alucLog <- allocation [[2]]
	} else{
	alucLog <- rbind(alucLog, allocation [[2]])
	}
		
	# post processing: spatial restrictions, nochange lc, natural lc module
	post.process <- alucR_postprocess (alloc == allocation [[1]],lc , spatial, var.list, traj, init.years)
	
	lc <- subset(post.process, subset=1)
	init.years <- subset(post.process , subset=2)
		assign(paste("scenario", epoche, sep=""), lc)
	print("epoche done")
	#initialize new epoche
	epoche <- epoche+1
	if (print.plot==TRUE){
	plot(lc)
	}
	} # end of epoche loop 
	out <- list(stack (mget (paste("scenario", rep(1:nrow(demand)),sep=""))), alucLog))
	return (out)
}