# demandE | data.frame with demand for each land use class, columnes refer to lu, i.e. names ("lc7", "lc4", "lc3"), while sorting to hierarchy between lu classes
# lc | raster layer, integer values
# spatial | raster layer defining protected areas
# natural.lc
# nochange.lc

# OUT: adjusted demand for natural land cover and spatial restrictions. 

demand.prep <- function (demandE = demand [epoche,], lc, spatial, varl.list) {
	# extract variables from var.list
	lu_suit <-   var.list [[1]][["lu_suit"]]
	nochange <- var.list [[2]][["nochange"]]
	natural <- var.list [[]][["natural"]]
	lc_unique <- var.list [[7]][["lc_unique"]]
	lc_n <- var.list [[2]][["lc_n"]]
	nochangeN <- var.list[[]][["nochangeN"]]
	
	
	# check if lu_suit matches names demand
	lud_classes <- as.numeric(gsub("lu", "", names(demandE)))
	if (all (match (lud_classes, lu_suit)==FALSE){ print("Names of dmemand and names from suitability stack do not match")}

	#definition of natural land cover (demandE/amount) 	
    if (length(nochange) > 0 ){
		natural.d <- lc_n - nochangeN - sum(demandE) 
		print (paste("Area for natural land cover:", natural.d ,"(if negative the demand for land use classes exceeds the amount of pixel available in the land use dataset)"))
		if (sign (natural.d) == -1) {natural.d <- 0} 
	}
	
	# adjusting for protected areas
	if (length (spatial) > 0){
	lc_spatial <- mask (lc, spatial, maskvalue=NA)
	lc_spatial.tab <- freq (lc_spatial, useNA="no")
	demand.adj <- demandE - 
	
	
    lc_pixN <- tabulate (getValues(lc), nbins=max(lc_unique)) 
                             nochange.n <- sum (lc_pix[no.change ])
                           }else {nochange.n <- c(0)}
                           natural.d <- 
                           print (paste("Area for natural land cover:", natural.d))
                           
                           #adjust for protected land cover
                           if (length (spatial) > 0){
                             lc_spatial <- lc
                             lc_spatial[is.na(spatial)] <- NA
                             lc_s.tab <- tabulate (getValues(lc_spatial), nbins=max(lc_unique))
                             demandE.adj <- demandE - lc_s.tab[lud_classes]
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
