# code. florian.gollnow@geo.hu-berlin.de
# alucR preperation of descriptive variables from input - only calculated once at the beginning of the allocation procedure 


alucR_prep0 <- function (lc, suit,nochange.lc, natural.lc ){ 

			var.list <- list()
			# number of pixel for all land use/cover classes within the initial land use/cover raster (excluding NA)
			var.list [[1]] <- data.frame (freq(lc, digits=0, value=NULL, useNA='no')) # uses freq instead of tabulate
			
			# total amount of pixels (excl. NAs) - based on frequency calculation
			var.list [[2]] <- data.frame (lc_n = sum(var.list [[1]][["count"]])
			
			# unique classes land use/cover classes - based on frequency calculation
			var.list [[3] <- data.frame (lc_unique =var.list [[1]][["value"]], lc_lookup = 1:length (var.list [[1]][["value"]]))
			
			#names of land use classes (those of the suitability stack) to be modelled
			lu_suit <- as.numeric(gsub("lc","",tolower(names(suit))))
			lu_slookup <- var.list [[3]][[]"lc_lookup"][lu_suit] # in case classification starts at 0 
			var.list [[4]] <- data.frame (lu_suit= lu_suit, lu_slookup= lu_slookup)
			
			# no change classes
			var.list [[5]] <- data.frame (no.change= ifelse(length(nochange.lc )> 0 , as.numeric(gsub("lc","",tolower(nochange.lc))), c()))
			
			#natural land cover classes (as.numeric)
			var.list [[6]] <- data.frame (natural= ifelse(length(natural.lc)>0 ,as.numeric(gsub("lc","",tolower(natural.lc))), c()))
						
			if (length (natural.lc)> 0) {
			# +1 pseudo natural layer for allocation algorithm
			var.list [[7]] <- data.frame (pseudo.N= max(var.list[[6]][["lc_unique"]]+1)
			
			# combine lu_suit and pseudo natural class
			var.list [[8]] <- data.frame (lu.N= c(var.list [[1]][[lu_suit]], var.list [[7]][[pseudo.N]]))# class numbers  of all classes to be modelled (incl. pseudo natural class)   
			}
			return (var.list)
			}