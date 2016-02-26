
#suit | either a RasterStack or a list of RasterStacks(for each year) of the suitabilities for land cover classes (ordered by preferences) resulting from the suitability analysis (see above). The data type should be Float (FLT4S). The names of the layers should correspond to the landuse classes, starting with "lc#", for example: "lc7", "lc4", "lc3",..  

alucR_prep1 <- function (suit) {
	#define p_raster from suitabilities depending on definition type(rasterstack or character string)
		if(class(suit)=="RasterStack" | class(suit)=="RasterBrick"){
			p_raster <- suit
		}else if (class(suit)=="character"){
			p_raster <- get(suit[epoche]) # in case different stacks for each episode are specified - possibly useful if  for example new roads are build
		return(p_raster)
	}