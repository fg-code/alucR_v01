#Date: 15.3.2026
# florian.gollnow@geo.hu-berlin.de

#prepares the demand file. adds a columne for the amount of natral vegetation to be modelled, substracts land use in protected areas form the demand, etc.

# demand | data.frame with demand for each land use class, columnes refer to lu, i.e. names ("lc7", "lc4", "lc3"), while sorting sould be similar to land use suitabilities rastestack
# lc | raster layer, integer values
# spatial | raster layer defining protected areas 
# var.list
# epoche

# OUT: list, combining the 
#[[1]] adjusted demand, including natural land cover and reduced for the spatial restrictions 
#[[2]] change of demand from one epoche to the next

alucR_demand.prep <- function (demand , lc, spatial, varl.list, epoche) {
  
  # extract variables from var.list
  lc_freq <- var.list [[1]]
  lc_n <- var.list [[2]][["lc_n"]]
  lc_unique <- var.list [[3]][["lc_unique"]]
  lc_suit <-   var.list [[4]][["lc_suit"]]
  nochange <- var.list [[5]][["nochange"]]
  nochangeN <-var.list [[6]] [["nochangeN"]]
  natural <- var.list [[7]][["natural"]]
  lc.N <- var.list [[9]][["lc.N"]]
  
  # check if lc_suit matches names of demand
  demandNames <- paste ("lc",lc_suit, sep="") # sorting according to suitaby rastestack
  
  names(demand) <- tolower(names(demand))
  
  indSort <- match (as.character(demandNames),as.character(names(demand)))
  if (all (is.na(indSort))){print("Names of 'demand' and names from 'suitability stack' do not match: both should start with lc followed by the class number ")}
  demandE <- demand[epoche,indSort] # sortet according to lc_suit RasterLayer
  
  #change of demand
  if (epoche==1){
    indSuit <- match(lc_suit, lc_freq[,"value"])
    lc_suitn <- lc_freq[indSuit, "count"]
    demandchange <- demandE - lc_suitn
  }else{
    demandchange <- demandE - demand[epoche-1,indSort] # indSort from above
  }
  
  ###########
  # amount of natural land cover to allocate accounting for no change classes and total amount of pixel in 
  if (length(natural)> 0){
    if (length(nochange) > 0){
      natural.d <- lc_n - nochangeN - sum(demandE) 
    } else {
      natural.d <- lc_n - sum(demandE) 
    }
    print (paste("Area for natural land cover:", natural.d))
    if (sign (natural.d) == -1) {
      natural.d <- 0
      print("Warning: the demand cannot be allocated due to spatial (no change classes). Natural vegetation as defined is set to 0")
    } 
  }  
  
  # adjusting demand for protected areas
  if (length (spatial) > 0){
    lc_spatial <- mask (lc, spatial, maskvalue=NA) # mask those values that are protected from the last
    lc_spatial.freq <- as.data.frame (freq (lc_spatial, useNA="no")) # calculate the number of pixels in the protected areas
    
    lc_spatial.tab <- data.frame(value = lc_unique, count= 0) # table with all unique values
    ind <- match(lc_spatial.tab[,"value"],lc_spatial.freq[,"value"])
    lc_spatial.tab[ind, "count"] <- lc_spatial.freq[ind,"count"] 
    
    indSpatial <- match (lc_suit, lc_spatial.tab[,"value"])
    lc_suitSpatial <- lc_spatial.tab[indSpatial, "count"]
    
    demandE <- demandE -  lc_suitSpatial
    
    if (length(natural)> 0 & natural.d !=0){
      indSpatial <- match (natural, lc_spatial.tab[,"value"])
      natural.d <- natural.d - sum (lc_spatial.tab[indSpatial,"count"], na.rm=TRUE)
      print (paste("Area for natural vegetation outside protected areas:", natural.d, sep=""))
    }
  }
  # combining demand for land use classes and natural land cover
  if (length(natural) > 0){
    demandE_N <- cbind (demandE, natural.d)
    names(demandE_N) <- c(names(demandE), "lcN")
  } else {
    demandE_N <- demandE
  }
  
  out <- list(demandE_N, demandchange)
  return(out)
}