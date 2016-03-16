# Date 15.3.2015 florian.gollnow@geo.hu-berlin.de alucR preperation of descriptive variables from input - only calculated once at the beginning of the
# allocation procedure

# lc | initial land cover categories.  suit | RasterStack of suitabilities for the specific land use classes to model. Layer names 'lc1', 'lc2'... with the
# numbers refering to the land cover classes in the initial land cover map lc nochange.lc | (optional) vector of charachter naming the land cover classes
# which do not change during the modelling experiment example c('lc5','lc6') when class 5 refers for example to water and 5 and 6 refer to water. Nochange
# classes should not be included in the suit RasterStack (will be dropped if any) natural.lc | (optional) vector of charachter naming the land cover classes
# which refer to natural vegetation. for example c('lc1','lc2') when landcover 1 refers to Forest and land cover 2 to secondary vegetation

alucR_prep1 <- function(lc, suit, nochange.lc, natural.lc)
{
    
    var.list <- list()
    # number of pixel for all land use/cover classes within the initial land use/cover raster (excluding NA)
    freqTable <- freq(lc, digits = 0, value = NULL, useNA = "no")
    var.list[[1]] <- data.frame(freqTable)  # uses freq instead of tabulate
    
    # total amount of pixels (excl. NAs) - based on frequency calculation
    lc_n <- sum(var.list[[1]][["count"]])
    var.list[[2]] <- data.frame(lc_n = lc_n)
    
    # unique classes land use/cover classes - based on frequency calculation
    lc_unique <- var.list[[1]][["value"]]
    lc_lookup <- 1:length(var.list[[1]][["value"]])
    var.list[[3]] <- data.frame(lc_unique = lc_unique, lc_lookup = lc_lookup)
    
    # names of land use classes (those of the suitability stack) to be modelled
    lc_suit <- as.numeric(gsub("lc", "", tolower(names(suit))))
    lc_slookup <- var.list[[3]][["lc_lookup"]][match(lc_suit, lc_unique)]  # in case classification starts at 0 
    var.list[[4]] <- data.frame(lc_suit = lc_suit, lc_slookup = lc_slookup)
    
    # no change classes
    if (length(nochange.lc) > 0)
    {
        nochange <- as.numeric(gsub("lc", "", tolower(nochange.lc)))
        var.list[[5]] <- data.frame(nochange = nochange)
        
        nochangeN <- sum(freqTable[match(nochange, freqTable[, "value"]), "count"])
        var.list[[6]] <- data.frame(nochangeN = nochangeN)
    } else
    {
        var.list[[5]] <- data.frame(nochange = c())
        var.list[[6]] <- data.frame(nochangeN = c())
    }
    
    # natural land cover classes (as.numeric)
    if (length(natural.lc) > 0)
    {
        natural <- as.numeric(gsub("lc", "", tolower(natural.lc)))
        naturallookup <- var.list[[3]][["lc_lookup"]][is.element(lc_unique, natural)]
        var.list[[7]] <- data.frame(natural = natural, naturallookup = naturallookup)
        
        # +1 pseudo natural layer for allocation algorithm
        pseudo.N <- max(var.list[[3]][["lc_unique"]]) + 1
        var.list[[8]] <- data.frame(pseudo.N = pseudo.N)
        
        # combine lc_suit and pseudo natural class
        var.list[[9]] <- data.frame(lc.N = c(var.list[[4]][["lc_suit"]], var.list[[8]][["pseudo.N"]]))  # class numbers  of all classes to be modelled (incl. pseudo natural class)   
    } else
    {
        var.list[[7]] <- data.frame(natural = c())
        var.list[[8]] <- data.frame(pseudo.N = c())
        var.list[[9]] <- data.frame(lc.N = c(var.list[[4]][["lc_suit"]]))  # in case no natural lc is defined 
    }
    return(var.list)
} 