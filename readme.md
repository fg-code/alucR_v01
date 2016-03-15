#alucR_v01 - allocation of land use change Version 01

alucR - Project is a first step to implement a Land Use Change Model in R (http://www.r-project.org). We have been following the basic framework provided by Verburg et al. (2002). Land use is spatially allocated following the suitability of a certain cell for the specific land use. The suitability might be assessed using statistical methods (for example logistic regression), machine learning algorithms (for example boosted regression trees) or other modelling techniques (for example Multi Criteria Analysis). The amount of future land use demands for the scenario assessment has to be estimated for the total study area and provided as numbers of pixels. Natural land cover and possible succession stages can be modelled based on the temporal trajectories of succession stages defined before in the trajectories matrix. The code uses basic R-language and packages. This makes it possible to easily adapt the code to the users specific needs.

#Differece to alucR
The version alucR_v01 takes a modular approach following a set of function with specified in and output. This approach makes it easier to add new submodules as for example nessesary when your suitability layers depend on the last landcover distribution from your sceanrios (i.e. if spatial lags are important).

#Submodules stucture:
* initializing
* preprocessing
* allocation of change
* postprocessin
* saving results

Thes submodules are called from the wrapper function 'alucR_0wrapper.r' script. Wile alle the required functions need to be sources seperately


description:

aluc(lc, suit, natural.lc=NULL, nochange.lc=NULL, spatial=NULL, demand, elas=matrix(data=0, ncol=max(lc_unique), nrow=max(lc_unique)), traj=matrix(data=1, ncol=max(lc_unique), nrow=max(lc_unique)), init.years= 5, method = "competitive", rule.mw = NULL, stop.crit=c(0.10 , 10), iter.max=100, ncores=(detectCores()-1), print.log=TRUE, print.plot=FALSE, write.raster=FALSE)

argument | description 
----- | ----- 
lc | initial land use/cover map 						
suit | either a RasterStack or a list of RasterStacks(for each year/epoche of sceanrio assessment) of the suitabilities for land use classes (ordered by preferences). These are usually the result of a suitability analysis. The data type should be Float (FLT4S). The names of the layers should correspond to the landuse classes, starting with "lc#", for example: "lc7", "lc4", "lc3",.. , only include suitabilities for landuses present in the initial land cover dataset and referenced in the 'demand' file. 						
natural.lc | character string defining land cover classes referring to natural vegetation ordered by succession states. For example: c("lc1", "lc2"). There should not be specific suitability layer for these classes. If suitability layers are provided they need to be defined in the suitability stack ('suit') and refered to in the 'demand' table			
nochange.lc | character string defining land cover/use classes wich are assumed to be stable during the sceanrio assessment. These classes cannot have a suitability layer in the 'suit' stack, neither be defined in the demand table or defined as 'natural.lc'. An example may be 'water' having land cover class 5. In this case you can indicate 'nochange.lc= c("lc5")'.			
spatial | either a RasterLayer or a list of RasterLayers(for each year/epoce of sceanrio assesment) of the locations where no land use change is allowed (i.e. Protected Areas).Definition: 'NA' for areas where conversions are allowed and 1 (or any other values) for areas where conversions are not allowed
demand | data.frame specifying the amount of pixel for each land use class (present in 'suit') for the subsequent modelling steps. Columns refer to the land use classes for which there is a suitability layer (same naming as for suitability layers), number of rows equal the number of modelling steps/epoches. Values should be integer.
elas | matrix of values between 0 and 1 referring to the conversion elasticity of the land use/cover classes. Rows: initial (t)land use/cover (1 to n), Columns: following (t+1) land use/cover (1 to n). Definition 0: no change to the original suitabilities, 0.5: incresed likelyness for the class or conversion (0.5 added to suitabilities at the specific location), 1: very high likelyness for the class or conversion (1 added to suitabilities at the specific location).
traj | matrix describing the temporal trajectories of land use/cover. Rows: initial (t) land use/cover (1 to n), Columns: following (t+1) land use/cover (1 to n). Values define the years/epoches of transition, e.g. 0: no transition allowed, 1: transition allowed after first iteration, 10: transition allowed after 10 iterations. must be specified for all land use/cover classes.
init.years | numeric value or RasterLayer to set the initial number of years the pixels are under the specific land use/cover at the beginning of the modelling.   
method | either "competitive" or "hierarchical" see description (so far only #competitive is avalable)
rule.mw | optional moving window algorithm. applies a moving window algorithm (circular) on the defined land use class(es) and weight the respective suitability layer accordingly (example: urban is more likely to expand around urban areas). Suitability layer will be multiplied with the neighborhood weights and 0 set to NA . Definition: data.frame containing name of land use class and radius of moving window. Example data.frame(name="lc7",radius=500)
stop.crit | (only applicable if method='competitive') stoping criteria defined as a vector with two values: first one refers to max percent difference from the land use changes 'demand'ed, second to the maximum pixel difference allowed. If the first & second is reached  or the second the allocation stops and saves the sceanario land cover
iter.max | (only applies for method='competitive') integer number specifying the maximum number of iteration until the allocation of land use/cover is stopped if the 'stop.crit' was not reached before. In that case the best out of the available allocation is returned)
ncores | (only applies for method="competitive")integer number specifying the number of cores to use during processing
print.log | TRUE/FALSE if tail of log file is printed during processing 
print.plot | TRUE/FALSE if iter and the final raster are plotted during model execution
write.raster | TRUE/FALSE if scenario output raster should be written to the working directory during iteration names 'scenario...tif'



