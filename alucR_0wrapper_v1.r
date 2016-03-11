# load data
library(rgdal)
library(raster)
rasterOptions (tmpdir = "q:/carbiocial/florian/data/no_backup/tmp1") 
library(snow)
library(parallel)
setwd("q:/carbiocial/florian/data/studyclue/BR_extend")

dir(patter=".tif$")

lc <- raster("tc10_recl.tif")
lc[Which(lc==9)] <- NA # clouds
lc[Which(lc==10)] <- NA # mask


crop <- raster("predictlogit/crop_suitMW.tif")
pasture <-  raster ("predictlogit/pasture_suitMW.tif")
suit <- stack( crop, pasture)
names(suit)<- c("lc4", "lc3") 

spatial.pi <- raster("variables/protected/pa_pi.tif")# c() #raster("pa_all.tif")
spatial.us <- raster("variables/protected/pa_us.tif")# c() #raster("pa_all.tif")
spatial.rp1 <- raster("variables/protected/br163_rpa.tif")
spatial.rp <- crop (spatial.rp1, spatial.pi)
origin(spatial.rp) <- origin(spatial.pi)

spatial <- merge(spatial.pi,spatial.us, spatial.rp)
demand<- read.csv("Q:/CarBioCial/Florian/Data/StudyCouple/demandtc/illegal.csv", row.names=2,header=T)[,-1] # uses the same demand as the illegal
demand <- demand [-1,] # exclude 2010
names(demand) <- c("lc4","lc3")
demand <- demand[c(1,2),c(2,1)] # 

elas <- matrix(data=c(0.3,0 ,0  ,0  ,0,0,0.3,0,
                      0 ,0.1,0  ,0  ,0,0,0.1,0,
                      0 ,0  ,0.8,0.7,0,0,0.8,0,
                      0 ,0  ,0.8,0.9,0,0,0.9,0,
                      0 ,0  ,0  ,0  ,0,0,0,0,
                      0 ,0  ,0  ,0  ,0,0,0,0,
                      0,0,0,0,0,0,1,0,
                      0 ,0  ,0  ,0  ,0,0,0,0), nrow=8, byrow=TRUE)

traj <- matrix (data=c(1,  1, 1, 1, 0, 0, 1, 1,
                       70, 1, 1, 1, 0, 0, 1, 1,
                       0,  1, 1, 1, 0, 0, 1, 1,
                       0,  1, 1, 1, 0, 0, 1, 1,
                       0,  0, 0, 0, 0, 0, 0, 0,
                       0,  0, 0, 0, 0, 0, 0, 0,
                       0,  1, 1, 1, 0, 0, 1, 1,
                       0,  1, 1, 1, 0, 0, 1, 1), nrow=8, byrow=TRUE)



nochange.lc <- c("lc5","lc6","lc7","lc8") 
natural.lc <- c("lc1","lc2")

#land use history
init.years <- raster("variables/landUseHist/luhist.tif")

# max# cores to use 
ncores=4
max.iter=100
stop.crit =c(0.1,10)


####WRAPPER

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
#Spatial restrictions
#reads the provided spatial restrictions maps. Spatial Restrictions can be provided as a Rasterlayer or character string of teh Names of RasterLayers. The character string need needs to be as long as the epoches to be modelled. 
if (length(spatial)>0){ # only if spatial restrictions are defined
  if (class(spatial)=="character"){
      spatially <- get(spatial[epoche])
  }else{
    if (epoche==1){
      assign("spatially", spatial)
    }}}
# Run submodules 
if (epoche==1){
alucR_check_input(lc, suit=p_raster, spatial, init.years, demand, nochange.lc, natural.lc)
var.list <- alucR_prep1(lc, suit=p_raster, nochange.lc, natural.lc )
}
suit.prep <- alucR_prep2(lc, suit=p_raster, spatial, init.years, var.list, epoche=epoche ,  elas, traj)
demand.prep<- alucR_demand.prep(demand , lc, spatial, varl.list, epoche)
sceanrioaloc <- alucR_competitive(suit=suit.prep, demandE=demand.prep[[1]], demandC=demand.prep[[2]], var.list, max.iter, stop.crit, ncores=ncores ,print.log=TRUE, print.plot=TRUE)
sceanrioL <- alucR_postprocess(alloc = sceanrioaloc [[1]],lc , spatial, var.list, traj, init.years)

#extra results from submodules and save them 
if (epoche==1){
  alucLog <- sceanrioaloc [[2]]
} else{
  alucLog <- rbind(alucLog, sceanrioaloc  [[2]])
}
lc <- subset(sceanrioL, subset=1)
assign(paste("scenario", epoche, sep=""), lc)
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


