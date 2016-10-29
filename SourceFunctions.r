

setwd("C:/Users/geo_flgo/Documents/GitHub/alucR_v01")
listfiles <- dir(patter=".r$")
listfiles
source("alucR_0wrapper.r")
source("alucR_1checkInput.r")
source("alucR_1prep_rule_mw.r")
source("alucR_1prep_varlist.r")
source("alucR_2prep_raster.r" )
source("alucR_3prep_demand.r")
source("alucR_4competitive_function_in_prep.r")
source("alucR_5postprocess.r")
source("alucR_hierachical_function.r")

