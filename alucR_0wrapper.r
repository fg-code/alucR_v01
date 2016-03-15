aluc <- function(lc, 						
                 suit, 						
                 natural.lc=NULL,				
                 nochange.lc=NULL,			
                 spatial=NULL, 
                 demand=NULL, 
                 elas=matrix(data=0, ncol=max(lc_unique), nrow=max(lc_unique)), 
                 traj=matrix(data=1, ncol=max(lc_unique), nrow=max(lc_unique)), 
                 init.years= 5,  
                 method = "competitive",
				 rule.mw = NULL
                 stop.crit=c(0.10 , 10),
                 iter.max=100, 
                 ncores=(detectCores()-1), 
                 print.log=TRUE, 
                 print.plot=FALSE, 
                 write.raster=FALSE){
		
#Initial Values
epoche=1
#

if (length (init.years)==1 ){
  init.hist <- setValues (lc, init.years)
} else	{
  assign ("init.hist", init.years)
}


# while loop: calculations for the scearnio maps 

while (epoche <= nrow(demand)){
print(paste("EPOCHE:", epoche , "Date:", date() ,sep=" "))

  
#Suitability maps 
#Reads the provided suitability maps. Suitabilities may be added as a RasteStacks with each layer refering to one of the modeled land use classes or a List of RasterStacks, one list object for each modelling epoche.
if (class(suit)=="list"){
    p_raster <- suit[[epoche]]
  } else { 
    assign ("p_raster", suit) 
  }
  
 if (length(rule.mw) > 0){
p_raster <- alucR_rule.mw(lc, suit=p_raster, rule.mw)
 } 
  
#Spatial restrictions
#reads the provided spatial restrictions maps. Spatial Restrictions can be provided as a Rasterlayer or character string of teh Names of RasterLayers. The character string need needs to be as long as the epoches to be modelled. 
if (length(spatial)>0){ # only if spatial restrictions are defined
  if (class(spatial)=="character"){
      spatially <- get(spatial[epoche])
  }else{
    if (epoche==1){
      assign("spatially", spatial)
    }}}

#Run submodules 
if (epoche==1){
alucR_check_input(lc, suit=p_raster, spatial, init.years, demand, nochange.lc, natural.lc)
var.list <- alucR_prep1(lc, suit=p_raster, nochange.lc, natural.lc )
}

suit.prep <- alucR_prep2(lc, suit=p_raster, spatial, init.years, var.list, epoche=epoche ,  elas, traj)

demand.prep<- alucR_demand.prep(demand , lc, spatial, varl.list, epoche)

if (method=="competitive"){
sceanrioaloc <- alucR_competitive(suit=suit.prep, demandE=demand.prep[[1]], demandC=demand.prep[[2]], var.list, max.iter, stop.crit, ncores=ncores ,print.log=TRUE, print.plot=TRUE)
}
sceanrioL <- alucR_postprocess(alloc = sceanrioaloc [[1]],lc , spatial, var.list, traj, init.years)

#extract results from submodules and save them 
if (epoche==1){
  alucLog <- data.frame(epoche=epoche, sceanrioaloc [[2]])
  
} else{
  alucLog <- rbind(alucLog,  data.frame(epoche=epoche, sceanrioaloc [[2]]))
}

lc <- subset(sceanrioL, subset=1)
assign(paste("scenario", epoche, sep=""), lc)
if (write.raster==TRUE){
writeRaster (lc, filename=paste("scenario", epoche, sep=""))
}


print("epoche done")
#initialize new epoche
epoche <- epoche+1
if (print.plot==TRUE){
  plot(lc)
}
rm(suit.prep)
rm(demand.prep)
rm(sceanrioaloc)
rm(sceanrioL)
} # end of epoche loop 
 
out <- list(stack (mget (paste("scenario", rep(1:nrow(demand)),sep=""))), alucLog))
return (out)
}