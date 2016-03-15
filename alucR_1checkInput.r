#Date 15.3.2016

#checks for projection, extent  and origine of raster files as well as naming conventions. 
#prints a warning if difficulties are encountered. 


#check input data for major assumptions 

#lc | initial land use/cover map 
#suit | either a RasterStack or a list of RasterStacks(for each year/epoche of sceanrio assessment) of the suitabilities for land use classes (ordered by preferences). These are usually the result of a suitability analysis. The data type should be Float (FLT4S). The names of the layers should correspond to the landuse classes, starting with "lc#", for example: "lc7", "lc4", "lc3",.. , only include suitabilities for landuses present in the initial land cover dataset and referenced in the 'demand' file. 
#spatial | either a RasterLayer or a list of RasterLayers(for each year/epoce of sceanrio assesment) of the locations where no land use change is allowed (i.e. Protected Areas).Definition: 'NA' for areas where conversions are allowed and 1 (or any other values) for areas where conversions are not allowed
#init.years | numeric value or RasterLayer to set the initial number of years the pixels are under the specific land use/cover at the beginning of the modelling.  
#demand | data.frame specifying the amount of pixel for each land use class (present in 'suit') for the subsequent modelling steps. Columns refer to the land use classes for which there is a suitability layer (same naming as for suitability layers), number of rows equal the number of modelling steps/epoches. Values should be integer.
#nochange.lc | character string defining land cover/use classes wich are assumed to be stable during the sceanrio assessment. These classes cannot have a suitability layer in the 'suit' stack, neither be defined in the demand table or defined as 'natural.lc'. An example may be 'water' having land cover class 5. In this case you can indicate 'nochange.lc= c("lc5")'.	
#natural.lc | character string defining land cover classes referring to natural vegetation ordered by succession states. For example: c("lc1", "lc2"). There should not be specific suitability layer for these classes. If suitability layers are provided they need to be defined in the suitability stack ('suit') and refered to in the 'demand' table	

alucR_check_input <- function (lc, suit, spatial, init.years, demand, nochange.lc, natural.lc){
  
  test <- all(origin(lc) ==origin(suit) & extent (lc)==extent (suit) & projection(lc)==projection(suit))
  if (test==FALSE){
    print ("lc and suit do not have the same projection, extend, or dimension. Use 'resample' in the raster package, method='bilinear' to aligne")
  }
  if (length (spatial)> 0 ){
    test <- all(origin(lc) ==origin(spatial) & extent (lc)==extent (spatial) & projection(lc)==projection(spatial))
    if (test==FALSE){
      print ("Warninglc and spatial do not have the same projection, extend, or dimension. Consider to use 'resample' in the raster package, method='ngb' to aligne")
    }
  }
  if (class(init.years)=="RasterLayer" ){
    test <- all(origin(lc) ==origin(init.years) & extent (lc)==extent (init.years) & projection(lc)==projection(init.years))
    if (test==FALSE){
      print ("Warning: lc and init.years do not have the same projection, extend, or dimension. Consider to use 'resample' in the raster package, method='ngb' to aligne")
    }
  }
  
  if(any(is.element(tolower(names(demand)), tolower(nochange.lc)))){
    print("Warning: nochange.lc should not be included in the demand table")
  }
  
  if(any(is.element(tolower(names(demand)), tolower(natural.lc)))){
    print("Warning: natural.lc should not be included in the demand table")
  }
}