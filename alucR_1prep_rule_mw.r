# Description: optional moving window algorithm. applies a moving window algorithm (circular) on the defined land use class(es) to and weight the respective
# suitability layer accordingly (example: urban is more likely to expand around urban areas). Suitability layer will be multiplied with the neighborhood
# weights and 0 set to NA. Definition: data.frame containing name of land use class and radius of moving window. Example data.frame(name='lc7',radius=500)


# lc | raster layer, integer values suit | raster stack, rule.mw | moving window rule example: data.frame(name='lc7',radius=500)


alucR_rule.mw <- function(lc, suit, rule.mw)
{
    for (f in 1:nrow(rule.mw))
    {
        # moving window rule.
        
        suitNames <- names(suit)
        if (is.element(as.character(rule.mw[f, 1]), suitNames))
        {
            mat <- focalWeight(lc, rule.mw[f, 2], "circle")  # lc class and radius of moving window
            focalC <- lc
            rclM <- cbind(sort(unique(focalC)), 0)
            rclM[which(rclM[, 1] == as.numeric(gsub("lc", "", rule.mw[f, 1]))), 2] <- 1  # binary classification for the class of interest
            focalC <- reclassify(focalC, rcl = rclM)
            focalW <- focal(x = focalC, w = mat, fun = sum, na.rm = TRUE)
            suitMW <- subset(suit, as.character(rule.mw[f, 1]))
            suitMWf <- suitMW * focalW
            suitMWf[Which(suitMWf) == 0] <- NA
            names(suitMWf) <- as.character(rule.mw[f, 1])
            suit <- dropLayer(suit, which(names(suit) == rule.mw[f, 1]))
            suit <- addLayer(suit, suitMWf)
            suit <- subset(suit, subset = match(suitNames, names(suit)))  # order to original order
            rm(suitMW)
            rm(suitMWf)
            rm(focalC)
            rm(focalW)
        } else
        {
          cat("\n", "rule.mw was not applied:", as.character(rule.mw[f, 1]), "is not an element of the suitability rasters names:", suitNames)  
          #print(paste("rule.mw was not applied:", as.character(rule.mw[f, 1]), "is not an element of the suitability rasters names:", suitNames, sep = ""))
        }
    }
    return(suit)
} 



