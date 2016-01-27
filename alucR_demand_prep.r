# demandE | data.frame with demand for each land use class, columnes refer to lu, i.e. names ("lc7", "lc4", "lc3"), while sorting to hierarchy between lu classes
# lc | raster layer, integer values
# spatial | raster layer defining protected areas
# natural.lc
# nochange.lc

# OUT: adjusted demand for natural land cover and spatial restrictions. 

demand.prep <- function (demandE = demand [epoche,], lc, spatial, natural.lc, nochange.lc) {
                           lu_classes <- as.numeric(gsub("lu", "", names(demandE)))
                           if (length (natural.lc) > 0) { natural <- as.numeric(gsub("lc", "", natural.lc))}
                           if (length(nochange.lc) > 0 ){ no.change <- as.numeric(gsub("lc", "", nochange.lc))}
                           lc_unique <- unique(lc)
                           lc_n <- sum (tabulate (getValues (lc)), nbins=max(lc_unique) ,na.rm=TRUE)
                           
                           #definition of natural land cover (demandE/amount) 	
                           if (length(no.change) > 0 ){
                             lc_pix <- tabulate (getValues(lc), nbins=max(lc_unique)) 
                             nochange.n <- sum (lc_pix[no.change ])
                           }else {nochange.n <- c(0)}
                           natural.d <- lc_n - nochange.n - sum(demandE) 
                           print (paste("Area for natural land cover:", natural.d))
                           
                           #adjust for protected land cover
                           if (length (spatial) > 0){
                             lc_spatial <- lc
                             lc_spatial[is.na(spatial)] <- NA
                             lc_s.tab <- tabulate (getValues(lc_spatial), nbins=max(lc_unique))
                             demandE.adj <- demandE - lc_s.tab[lu_classes]
                             natural.adj <- natural.d -  sum(lc_s.tab[natural])
                           } else {
                             demandE.adj <- demandE
                             natural.adj <- natural.data
                           }
                           if (sign(natural.adj)== -1) {print("land use cannot be allocated due to spatial restrictions")}	
                           demandE.new <- as.integer(cbind(demandE.adj, natural.adj))
                           out <- demandE.new
                           
                           return(out)
}
