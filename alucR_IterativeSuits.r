library(rgdal)
library(raster)
# rasterOptions (tmpdir = 'q:/carbiocial/florian/data/no_backup/tmp3')
library(snow)
library(parallel)

# setwd('q:/carbiocial/florian/masterStudents/AntoniaWeiller/Data/model')
setwd("Q:/Florian/MasterStudents/AntoniaWeiller/Data/model/tiffs")


######################################## Read Data LandCover; suitability layer that do not change during the modelling procedure;
lc <- raster("reclass10.tif")
cropSuit <- NULL
pastureSuit <- NULL

# Protected areas if required
spatial <- NULL

# set variables
nochange.lc <- NULL
natural.lc <- NULL
demand <- NULL
traj <- NULL
elas <- NULL

# eiter Raster or a number
init.years <- 0

# Secondary Model - model and explanatory variables needed as Raster data (object Names need to be simiar to variables anames in the Model)
sec.model <- load("")

# Non changing explanatory variables
EucDist_prot <- raster()
EucDist_roads <- raster()

########## Model specific parameters
ncores <- (detectCores() - 1)
print.plot <- TRUE
print.log <- TRUE
write.raster <- FALSE

############################################### MODEl START WRAPPER
epoche <- 1
# 

if (length(init.years) == 1 & class(init.years) == "numeric")
{
  init.hist <- setValues(lc, init.years)
} else
{
  assign("init.hist", init.years)
}


# while loop: calculations for the scearnio maps iteartively updated

while (epoche <= nrow(demand))
{
  print(paste("EPOCHE:", epoche, "Date:", date(), sep = " "))
  
  ############################################################ Add specific suitability calculation if suitability models are dependent of the former land use/cover you will need the model object and all explanatory
  ############################################################ variables
  
  # distance to crop
  cropRater <- subs(lc, data.frame(which = 4, by = 1), by = 2, which = 1, subsWithNA = TRUE)  # in case cropland is class 4   
  EucDis_crop <- distance(cropRaster)
  rm(cropRater)
  # distance to forest
  forestRater <- subs(lc, data.frame(which = 1, by = 1), by = 2, which = 1, subsWithNA = TRUE)  # in case forest is class 1   
  EucDis_forest <- distance(forestRate)
  rm(forestRater)
  # distance to urban
  urbanRaster <- subs(lc, data.frame(which = 7, by = 1), by = 2, which = 1, subsWithNA = TRUE)
  EucDis_urban <- distance(urbanRaster)
  rm(urbanRaster)
  
  # distance to deforestation - if nessesary you would actually need to updtae the deforestation map set the deforested areas to 1 forest calculate the distance
  # and set Forest NA?
  
  # stack all explanatory variables
  expla <- stack(EucDis_forest, EucDis_crop, ....)
  
  
  # EucDist_def # predict new suitability layer
  secVegSuit <- predict(expla, sec.model, type = "response")
  
  
  # stack suitabilities
  p_raster <- stack(cropSuit, pastureSuit, secVegSuit)
  names(p_raster) <- c("lc4", "lc3", "lc2")
  
  
  
  ############################################################
  # Do not touch 
  #Spatial restrictions reads the provided spatial restrictions maps. Spatial Restrictions can be provided as a Rasterlayer 
  #or character string of the Names of RasterLayers. The character string need needs to be as long as the epoches to be modelled.
  if (length(spatial) > 0)
  {
    if (class(spatial) == "character")
    {
      spatially <- get(spatial[epoche])
    } else
    {
      if (epoche == 1)
      {
        assign("spatially", spatial)
      }
    }
  }
  # Run submodules
  if (epoche == 1)
  {
    alucR_check_input(lc, suit = p_raster, spatial, init.years, demand, nochange.lc, natural.lc)
    var.list <- alucR_prep1(lc, suit = p_raster, nochange.lc, natural.lc)
  }
  suit.prep <- alucR_prep2(lc, suit = p_raster, spatial, init.years, var.list, epoche = epoche, elas, traj)
  demand.prep <- alucR_demand.prep(demand, lc, spatial, varl.list, epoche)
  sceanrioaloc <- alucR_competitive(suit = suit.prep, demandE = demand.prep[[1]], demandC = demand.prep[[2]], var.list, max.iter, stop.crit, ncores = ncores, 
                                    print.log = TRUE, print.plot = TRUE)
  sceanrioL <- alucR_postprocess(alloc = sceanrioaloc[[1]], lc, spatial, var.list, traj, init.years)
  
  # extra results from submodules and save them
  if (epoche == 1)
  {
    alucLog <- data.frame(epoche = epoche, sceanrioaloc[[2]])
    
  } else
  {
    alucLog <- rbind(alucLog, data.frame(epoche = epoche, sceanrioaloc[[2]]))
  }
  lc <- subset(sceanrioL, subset = 1)
  assign(paste("scenario", epoche, sep = ""), lc)
  if (write.raster == TRUE)
  {
    writeRaster(lc, paste("scenario", epoche, ".tif", sep = ""))
  }
  
  
  print("epoche done")
  # initialize new epoche
  epoche <- epoche + 1
  if (print.plot == TRUE)
  {
    plot(lc)
  }
  rm(suit.prep)
  rm(demand.prep)
  rm(sceanrioaloc)
  rm(sceanrioL)
}  # end of epoche loop 

# all sceanrios schould be saved as objects named 'scenario1', 'sceanrio'2, etc... 