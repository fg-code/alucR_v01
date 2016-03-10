


### iteratively adjust propabilities according to newst sceanrio lannd cover
#vectorcontaining the model object names
modelList <- list(modelCrop,modelPasture,modelSecondary)

suitupdate <- (modelList = , Function='distance', to_classes=c(), varNames=c() )

for (b in 1:length(to_classes) ){
  tab <- reclassify(lc , rcl=matric(to_classes[b], 1))
  tabDist <- distance(tab)
  assign (varNames[b], tabDist)
  }

for (c in 1:length(modelList) ){
  variables <- stack (get(names(modelListe[[c]]$data)))
  tab <- clusterR(modelList[[c]]), predict, args=list(variables))
  assign (names(suit), tab)
 }

p_raster <- stack (get(names(suit)))
