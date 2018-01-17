setMethod("summary", signature=c("VegX"), definition = function(object, ...) {
  cat(paste0("============================================================\n"))
  cat(paste0("                  VegX object (ver 1.5.3)                  \n"))
  cat(paste0("------------------------------------------------------------\n"))
  cat(paste0(" ", length(object@plotObservations)," plot observations"))
  cat(paste0(" made in ", length(object@plots)," plots, from ",length(object@projects)," projects.\n"))
  cat(paste0("------------------------------------------------------------\n"))
  cat(paste0("\n"))
  cat(paste0("   Individual organisms: ", length(object@individualOrganisms),"\n"))
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
      cat(paste0("      ",i,". ", object@methods[[i]]$name," [",object@methods[[i]]$attributeClass,"/",object@methods[[i]]$attributeType,"]\n"))
    }
  }
  cat(paste0("\n"))
  cat(paste0("   Individual organism observations: ", length(object@individualObservations),"\n"))
  cat(paste0("   Aggregated organism observations: ", length(object@aggregatedObservations),"\n"))
  cat(paste0("   Stratum observations: ", length(object@stratumObservations),"\n"))
  cat(paste0("============================================================\n"))
})
