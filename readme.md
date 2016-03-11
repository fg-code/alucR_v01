## alucR_v01 - allocation of land use change Version 01

alucR - Project is a first step to implement a Land Use Change Model in R (http://www.r-project.org). We have been following the basic framework provided by Verburg et al. (2002). Land use is spatially allocated following the suitability of a certain cell for the specific land use. The suitability might be assessed using statistical methods (for example logistic regression), machine learning algorithms (for example boosted regression trees) or other modelling techniques (for example Multi Criteria Analysis). The amount of future land use demands for the scenario assessment has to be estimated for the total study area and provided as numbers of pixels. Natural land cover and possible succession stages can be modelled based on the temporal trajectories of succession stages defined before in the trajectories matrix. The code uses basic R-language and packages. This makes it possible to easily adapt the code to the users specific needs.

#Differece to alucR
The version alucR_v01 takes an modular approach following a set of function with specified in and output. This approach makes it easier to add new submodules as for example nessesary when you suitability layers change in response to the sceanrio calculation.

Submodules are structure into:
* initializing
* preprocessing
* allocation of change
* postprocessin
* saving results

.... more to come....

