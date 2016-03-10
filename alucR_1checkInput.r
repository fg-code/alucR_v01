#check input data for major assumptions 

#lc
#suit
#spatial
#init.years
#demand
#nochange.lc
#natural.lc

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