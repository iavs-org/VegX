setMethod("summary", signature=c("VegX"), definition = function(object, ...) {
  cat(paste0("================================================================\n"))
  cat(paste0("                     VegX object (ver 1.5.3)                    \n"))
  cat(paste0("----------------------------------------------------------------\n"))
  cat(paste0("\n"))
  cat(paste0("   Projects: ", length(object@projects),"\n"))
  if(length(object@projects)>0) {
    for(i in 1:length(object@projects)){
      cat(paste0("      ",i,". ", object@projects[[i]]$title,"\n"))
    }
  }
  cat(paste0("\n"))
  nplots = length(object@plots)
  nsubplots = .getNumberOfSubPlots(object)
  cat(paste0("   Plots: ", nplots,"  [Parent: ", nplots - nsubplots," Subplots: ", nsubplots,"]\n"))
  cat(paste0("\n"))
  cat(paste0("   Individual organisms: ", length(object@individualOrganisms),"\n"))
  cat(paste0("\n"))
  # cat(paste0("   Taxon names: ", length(object@taxonNames),"\n"))
  cat(paste0("   Taxon name usage concepts: ", length(object@taxonNameUsageConcepts),"\n"))
  cat(paste0("\n"))
  cat(paste0("   Vegetation strata: ", length(object@strata),"\n"))
  if(length(object@strata)>0) {
    for(i in 1:length(object@strata)){
      cat(paste0("      ",i,". ", object@strata[[i]]$stratumName," [",object@strata[[i]]$stratumSequence,"/",object@methods[[object@strata[[i]]$methodID]]$name,"]\n"))
    }
  }
  cat(paste0("\n"))
  cat(paste0("   Methods: ", length(object@methods),"\n"))
  if(length(object@methods)>0) {
    for(i in 1:length(object@methods)){
      attIDs = .getAttributeIDsByMethodID(object, names(object@methods)[i])
      cat(paste0("      ",i,". ", object@methods[[i]]$name," [",object@methods[[i]]$attributeClass," / ",length(attIDs), " ", object@methods[[i]]$attributeType," atts.]\n"))
    }
  }
  cat(paste0("\n"))
  npobs = length(object@plotObservations)
  nsubpobs = .getNumberOfPlotObservationsInSubPlots(object)
  cat(paste0("   Plot observations: ", npobs,"  [in parent: ", npobs - nsubpobs," in subplots: ", nsubpobs,"]\n"))
  cat(paste0("\n"))
  cat(paste0("   Individual organism observations: ", length(object@individualObservations),"\n"))
  cat(paste0("\n"))
  cat(paste0("   Aggregated organism observations: ", length(object@aggregateObservations),"\n"))
  cat(paste0("\n"))
  cat(paste0("   Stratum observations: ", length(object@stratumObservations),"\n"))
  cat(paste0("\n"))
  cat(paste0("   Abiotic observations: ", length(object@abioticObservations),"\n"))
  cat(paste0("\n"))
  cat(paste0("================================================================\n"))
})
