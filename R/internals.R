.nextProjectID<-function(target) {
  if(length(target@projects)==0) return("1")
  return(as.character(as.numeric(names(target@projects)[length(target@projects)])+1))
}
.nextPlotID<-function(target) {
  if(length(target@plots)==0) return("1")
  return(as.character(as.numeric(names(target@plots)[length(target@plots)])+1))
}
.nextPlotObservationID<-function(target) {
  if(length(target@plotObservations)==0) return("1")
  return(as.character(as.numeric(names(target@plotObservations)[length(target@plotObservations)])+1))
}
.nextStratumID<-function(target) {
  if(length(target@strata)==0) return("1")
  return(as.character(as.numeric(names(target@strata)[length(target@strata)])+1))
}
.nextSurfaceTypeID<-function(target) {
  if(length(target@surfaceTypes)==0) return("1")
  return(as.character(as.numeric(names(target@surfaceTypes)[length(target@surfaceTypes)])+1))
}
.nextStratumObservationID<-function(target) {
  if(length(target@stratumObservations)==0) return("1")
  return(as.character(as.numeric(names(target@stratumObservations)[length(target@stratumObservations)])+1))
}
.nextSurfaceCoverObservationID<-function(target) {
  if(length(target@surfaceCoverObservations)==0) return("1")
  return(as.character(as.numeric(names(target@surfaceCoverObservations)[length(target@surfaceCoverObservations)])+1))
}
.nextAggregateOrganismObservationID<-function(target) {
  if(length(target@aggregateObservations)==0) return("1")
  return(as.character(as.numeric(names(target@aggregateObservations)[length(target@aggregateObservations)])+1))
}
.nextIndividualOrganismID<-function(target) {
  if(length(target@individualOrganisms)==0) return("1")
  return(as.character(as.numeric(names(target@individualOrganisms)[length(target@individualOrganisms)])+1))
}
.nextIndividualOrganismObservationID<-function(target) {
  if(length(target@individualObservations)==0) return("1")
  return(as.character(as.numeric(names(target@individualObservations)[length(target@individualObservations)])+1))
}
.nextSiteObservationID<-function(target) {
  if(length(target@siteObservations)==0) return("1")
  return(as.character(as.numeric(names(target@siteObservations)[length(target@siteObservations)])+1))
}
.nextMethodID<-function(target) {
  if(length(target@methods)==0) return("1")
  return(as.character(as.numeric(names(target@methods)[length(target@methods)])+1))
}
.nextAttributeID<-function(target) {
  if(length(target@attributes)==0) return("1")
  return(as.character(as.numeric(names(target@attributes)[length(target@attributes)])+1))
}
.nextTNUCID<-function(target) {
  if(length(target@taxonNameUsageConcepts)==0) return("1")
  return(as.character(as.numeric(names(target@taxonNameUsageConcepts)[length(target@taxonNameUsageConcepts)])+1))
}



# Returns the projectID for a new project in the data set or the ID of an existing project with the same name
.newProjectIDByTitle<-function(target, projectTitle) {
  if(length(target@projects)==0) return(list(id="1", new = TRUE))
  for(i in 1:length(target@projects)) {
    if(target@projects[[i]]$title==projectTitle) return(list(id=names(target@projects)[i], new = FALSE))
  }
  return(list(id = .nextProjectID(target), new = TRUE))
}
# Returns the plotID for a new plot in the data set or the ID of an existing plot with the same name
.newPlotIDByName<-function(target, plotName) {
  if(length(target@plots)==0) return(list(id="1", new = TRUE))
  for(i in 1:length(target@plots)) {
    if(target@plots[[i]]$plotName==plotName) return(list(id = names(target@plots)[i], new = FALSE))
  }
  return(list(id = .nextPlotID(target), new = TRUE))
}
# Returns the ID for a new plot observation in the data set or the ID of an existing plot observation
.newPlotObsIDByDate<-function(target, plotID, obsStartDate) {
  obsStartDate = as.Date(obsStartDate)
  if(length(target@plotObservations)==0) return(list(id="1", new = TRUE))
  for(i in 1:length(target@plotObservations)) {
    if((target@plotObservations[[i]]$plotID==plotID) && (target@plotObservations[[i]]$obsStartDate==obsStartDate)) return(list(id = names(target@plotObservations)[i], new = FALSE))
  }
  return(list(id = .nextPlotObservationID(target), new = TRUE))
}
# Returns the ID for a new stratum in the data set or the ID of an existing stratum
.newStratumIDByName<-function(target, methodID, stratumName) {
  if(length(target@strata)==0) return(list(id="1", new = TRUE))
  for(i in 1:length(target@strata)) {
    if((target@strata[[i]]$methodID==methodID) && (target@strata[[i]]$stratumName==stratumName)) return(list(id = names(target@strata)[i], new = FALSE))
  }
  return(list(id = .nextStratumID(target), new = TRUE))
}
# Returns the ID for a new stratum observation in the data set or the ID of an existing stratum observation
.newStratumObsIDByIDs<-function(target, plotObservationID, stratumID) {
  if(length(target@stratumObservations)==0) return(list(id="1", new = TRUE))
  for(i in 1:length(target@stratumObservations)) {
    if((target@stratumObservations[[i]]$plotObservationID==plotObservationID) && (target@stratumObservations[[i]]$stratumID==stratumID)) return(list(id = names(target@stratumObservations)[i], new = FALSE))
  }
  return(list(id = .nextStratumObservationID(target), new = TRUE))
}
# Returns the ID for a new surface type in the data set or the ID of an existing surface type
.newSurfaceTypeIDByName<-function(target, methodID, surfaceName) {
  if(length(target@surfaceTypes)==0) return(list(id="1", new = TRUE))
  for(i in 1:length(target@surfaceTypes)) {
    if((target@surfaceTypes[[i]]$methodID==methodID) && (target@surfaceTypes[[i]]$surfaceName==surfaceName)) return(list(id = names(target@surfaceTypes)[i], new = FALSE))
  }
  return(list(id = .nextSurfaceTypeID(target), new = TRUE))
}
# Returns the ID for a new surface cover observation in the data set or the ID of an existing one
.newSurfaceCoverObsIDByIDs<-function(target, plotObservationID, surfaceTypeID) {
  if(length(target@surfaceCoverObservations)==0) return(list(id="1", new = TRUE))
  for(i in 1:length(target@surfaceCoverObservations)) {
    if((target@surfaceCoverObservations[[i]]$plotObservationID==plotObservationID) && (target@surfaceCoverObservations[[i]]$surfaceTypeID==surfaceTypeID)) return(list(id = names(target@surfaceCoverObservations)[i], new = FALSE))
  }
  return(list(id = .nextSurfaceCoverObservationID(target), new = TRUE))
}
# Returns the ID for a new aggregate organism observation in the data set or the ID of an existing aggregate organism observation
.newAggregateOrganismObservationIDByTaxonID<-function(target, plotObservationID, stratumObservationID, tnucID) {
  if(length(target@aggregateObservations)==0) return(list(id="1", new = TRUE))
  for(i in 1:length(target@aggregateObservations)) {
    if((target@aggregateObservations[[i]]$plotObservationID==plotObservationID) &&
       (target@aggregateObservations[[i]]$stratumObservationID==stratumObservationID) &&
       (target@aggregateObservations[[i]]$taxonNameUsageConcept==tnucID))
      return(list(id = names(target@aggregateObservations)[i], new = FALSE))
  }
  return(list(id = .nextAggregateOrganismObservationID(target), new = TRUE))
}
# Returns the ID for a new individual organism in the data set or the ID of an existing organism
.newIndividualOrganismIDByIdentificationLabel<-function(target, plotID, identificationLabel) {
  if(length(target@individualOrganisms)==0) return(list(id="1", new = TRUE))
  for(i in 1:length(target@individualOrganisms)) {
    if((target@individualOrganisms[[i]]$plotID==plotID) && (target@individualOrganisms[[i]]$identificationLabel==identificationLabel)) return(list(id = names(target@individualOrganisms)[i], new = FALSE))
  }
  return(list(id = .nextIndividualOrganismID(target), new = TRUE))
}
# Returns the ID for a new individual organism observation in the data set or the ID of an existing organism observation
.newIndividualOrganismObservationIDByIndividualID<-function(target, plotObservationID, individualOrganismID) {
  if(length(target@individualObservations)==0) return(list(id="1", new = TRUE))
  for(i in 1:length(target@individualObservations)) {
    if((target@individualObservations[[i]]$plotObservationID==plotObservationID) && (target@individualObservations[[i]]$individualOrganismID==individualOrganismID)) return(list(id = names(target@individualObservations)[i], new = FALSE))
  }
  return(list(id = .nextIndividualOrganismObservationID(target), new = TRUE))
}
# Returns the ID for a new site observation in the data set or the ID of an existing site observation
.newSiteObservationIDByID<-function(target, plotObservationID) {
  if(length(target@siteObservations)==0) return(list(id="1", new = TRUE))
  for(i in 1:length(target@siteObservations)) {
    if(target@siteObservations[[i]]$plotObservationID==plotObservationID) return(list(id = names(target@siteObservations)[i], new = FALSE))
  }
  return(list(id = .nextSiteObservationID(target), new = TRUE))
}
# Returns the ID for a new method in the data set or the ID of an existing method with the same name
.newMethodIDByName<-function(target, methodName) {
  if(length(target@methods)==0) return(list(id="1", new = TRUE))
  for(i in 1:length(target@methods)) {
    if(target@methods[[i]]$name==methodName) return(list(id = names(target@methods)[i], new = FALSE))
  }
  return(list(id = .nextMethodID(target), new = TRUE))
}
# Returns the ID for a new taxon name usage concept in the data set or the ID of an existing taxon name usage concept with the same name
.newTaxonNameUsageConceptIDByName<-function(target, authorTaxonName) {
  if(length(target@taxonNameUsageConcepts)==0) return(list(id="1", new = TRUE))
  for(i in 1:length(target@taxonNameUsageConcepts)) {
    if(target@taxonNameUsageConcepts[[i]]$authorTaxonName==authorTaxonName) return(list(id = names(target@taxonNameUsageConcepts)[i], new = FALSE))
  }
  return(list(id = .nextTNUCID(target), new = TRUE))
}



# Returns strata names corresponding to the input method
.getAttributeCodesByMethodID<-function(target, methodID) {
  attVec = character(0)
  if(length(target@attributes)>0) {
    cnt = 1
    for(i in 1:length(target@attributes)) {
      if(target@attributes[[i]]$methodID==methodID) {
        if(target@attributes[[i]]$type != "quantitative") {
          attVec[cnt] = target@attributes[[i]]$code
          cnt = cnt + 1
        }
      }
    }
  }
  return(attVec)
}
# Returns strata names corresponding to the input method
.getAttributeIDsByMethodID<-function(target, methodID) {
  attVec = character(0)
  if(length(target@attributes)>0) {
    cnt = 1
    for(i in 1:length(target@attributes)) {
      if(target@attributes[[i]]$methodID==methodID) {
        attVec[cnt] = names(target@attributes)[i]
        cnt = cnt + 1
      }
    }
  }
  return(attVec)
}


# Returns strata names corresponding to the input method
.getStratumNamesByMethodID<-function(target, methodID) {
  strVec = character(0)
  if(length(target@strata)>0) {
    cnt = 1
    for(i in 1:length(target@strata)) {
      if(target@strata[[i]]$methodID==methodID) {
        strVec[cnt] = target@strata[[i]]$stratumName
        cnt = cnt + 1
      }
    }
  }
  return(strVec)
}

# Returns strata IDs corresponding to the input method
.getStratumIDsByMethodID<-function(target, methodID) {
  strVec = character(0)
  if(length(target@strata)>0) {
    cnt = 1
    for(i in 1:length(target@strata)) {
      if(target@strata[[i]]$methodID==methodID) {
        strVec[cnt] = names(target@strata)[i]
        cnt = cnt + 1
      }
    }
  }
  return(strVec)
}



# Returns surface type IDs corresponding to the input method
.getSurfaceTypeIDsByMethodID<-function(target, methodID) {
  stVec = character(0)
  if(length(target@surfaceTypes)>0) {
    cnt = 1
    for(i in 1:length(target@surfaceTypes)) {
      if(target@surfaceTypes[[i]]$methodID==methodID) {
        stVec[cnt] = names(target@surfaceTypes)[i]
        cnt = cnt + 1
      }
    }
  }
  return(stVec)
}

.getNumberOfSubPlots<-function(target) {
  cnt = 0
   if(length(target@plots)>0) {
     for(i in 1:length(target@plots)) {
       if("parentPlotID" %in% names(target@plots[[i]])) {
         cnt = cnt + 1
       }
     }
   }
   return(cnt)
}

.getNumberOfPlotObservationsInSubPlots<-function(target) {
  cnt = 0
  if(length(target@plotObservations)>0) {
    for(i in 1:length(target@plotObservations)) {
      plot = target@plots[[target@plotObservations[[i]]$plotID]]
      if("parentPlotID" %in% names(plot)) {
        cnt = cnt + 1
      }
    }
  }
  return(cnt)
}


#Translate attributes of measurements in a plot element
.applyAttributeMappingToPlot<-function(plot, attIDmap) {
  for(n in names(plot)) {
    # Update attribute codes
    if(n %in% c("topography")) {
      for(m in names(plot[[n]])) {
        if(m %in% c("slope", "aspect"))
          plot[[n]][[m]]$attributeID = attIDmap[[plot[[n]][[m]]$attributeID]]
      }
    }
  }
  return(plot)
}
.applyAttributeMappingToAggregatePlotObservations <-function(aggObs, attIDmap) {
  for(n in names(aggObs)) {
    # Update attribute codes
    if(n %in% c("aggregateOrganismMeasurements")) {
      for(i in 1:length(aggObs[["aggregateOrganismMeasurements"]])) {
        aggObs$aggregateOrganismMeasurements[[i]]$attributeID = attIDmap[[aggObs$aggregateOrganismMeasurements[[i]]$attributeID]]
      }
    }
    else if(n %in% c("heightMeasurement")) {
      aggObs[[n]]$attributeID = attIDmap[[aggObs[[n]]$attributeID]]
    }
  }
  return(aggObs)
}
.applyAttributeMappingToSiteObservations<-function(siteobs, attIDmap) {
  # Update attribute codes
  for(n in names(siteobs)) {
    if(n %in% c("soilMeasurements", "climateMeasurements", "waterMassMeasurements")) {
      for(i in 1:length(siteobs[[n]])) {
        siteobs[[n]][[i]]$attributeID = attIDmap[[siteobs[[n]][[i]]$attributeID]]
      }
    }
  }
  return(siteobs)
}
.applyAttributeMappingToIndividualOrganismObservations<-function(indObs, attIDmap) {
  for(n in names(indObs)) {
    # Update attribute codes
    if(n %in% c("individualOrganismMeasurements")) {
      for(i in 1:length(indObs[["individualOrganismMeasurements"]])) {
        indObs$individualOrganismMeasurements[[i]]$attributeID = attIDmap[[indObs$individualOrganismMeasurements[[i]]$attributeID]]
      }
    }
    else if(n %in% c("heightMeasurement", "diameterMeasurement")) {
      indObs[[n]]$attributeID = attIDmap[[indObs[[n]]$attributeID]]
    }
  }
  return(indObs)
}
.applyAttributeMappingToStratumObservations<-function(strobs, attIDmap) {
  for(n in names(strobs)) {
    if(n %in% c("stratumMeasurements")) {
      for(i in 1:length(strobs[["stratumMeasurements"]])) {
        strobs$stratumMeasurements[[i]]$attributeID = attIDmap[[strobs$stratumMeasurements[[i]]$attributeID]]
      }
    }
    else if(n %in% c("lowerLimitMeasurement", "upperLimitMeasurement")) {
      strobs[[n]]$attributeID = attIDmap[[strobs[[n]]$attributeID]]
    }
  }
  return(strobs)
}
.applyAttributeMappingToSurfaceCoverObservations<-function(scobs, attIDmap) {
  for(n in names(scobs)) {
    if(n %in% c("coverMeasurement")) {
      scobs[[n]]$attributeID = attIDmap[[scobs[[n]]$attributeID]]
    }
  }
  return(scobs)
}

#Pools the information of two plots
.mergePlots<-function(plot1, plot2, attIDmap) {
   n1 = names(plot1)
   n2 = names(plot2)
   plot2 = .applyAttributeMappingToPlot(plot2, attIDmap)
   npool = unique(c(n1,n2))
   res = list()
   for(n in npool) {
     if((n %in% n1) && (n %in% n2)) {
       if(plot1[[n]]!=plot2[[n]]) stop(paste0("Plots have different data for '", n, "'. Cannot merge."))
       res[[n]] = plot1[[n]]
     } else if(n %in% n1) {
       res[[n]] = plot1[[n]]
     } else if(n %in% n2) {
       res[[n]] = plot2[[n]]
     }
   }
   return(res)
}

#Pools the information of two plot observationss
.mergePlotObservations<-function(plotObservation1, plotObservation2, attIDmap) {
  n1 = names(plotObservation1)
  n2 = names(plotObservation2)
  npool = unique(c(n1,n2))
  res = list()
  for(n in npool) {
    if((n %in% n1) && (n %in% n2)) {
      if(plotObservation1[[n]]!=plotObservation2[[n]]) stop(paste0("Plot observations have different data for '", n, "'. Cannot merge."))
      res[[n]] = plotObservation1[[n]]
    } else if(n %in% n1) {
      res[[n]] = plotObservation1[[n]]
    } else if(n %in% n2) {
      res[[n]] = plotObservation2[[n]]
    }
  }
  return(res)
}

#Pools the information of two taxon name usage concepts
.mergeTaxonNameUsageConcepts<-function(tnuc1, tnuc2) {
  n1 = names(tnuc1)
  n2 = names(tnuc2)
  npool = unique(c(n1,n2))
  res = list()
  for(n in npool) {
    if((n %in% n1) && (n %in% n2)) {
      if(tnuc1[[n]]!=tnuc2[[n]]) stop(paste0("Taxon name usage concepts have different data for '", n, "'. Cannot merge."))
      res[[n]] = tnuc1[[n]]
    } else if(n %in% n1) {
      res[[n]] = tnuc1[[n]]
    } else if(n %in% n2) {
      res[[n]] = tnuc2[[n]]
    }
  }
  return(res)
}

#Pools the information of two methods
.mergeMethods<-function(met1, met2) {
  n1 = names(met1)
  n2 = names(met2)
  npool = unique(c(n1,n2))
  res = list()
  for(n in npool) {
    if((n %in% n1) && (n %in% n2)) {
      if(met1[[n]]!=met2[[n]]) stop(paste0("Methods have different data for '", n, "'. Cannot merge."))
      res[[n]] = met1[[n]]
    } else if(n %in% n1) {
      res[[n]] = met1[[n]]
    } else if(n %in% n2) {
      res[[n]] = met2[[n]]
    }
  }
  return(res)
}

#Pools the information of two strata
.mergeStrata<-function(str1, str2) {
  n1 = names(str1)
  n2 = names(str2)
  npool = unique(c(n1,n2))
  res = list()
  for(n in npool) {
    if((n %in% n1) && (n %in% n2)) {
      if(str1[[n]]!=str2[[n]]) stop(paste0("Strata have different data for '", n, "'. Cannot merge."))
      res[[n]] = str1[[n]]
    } else if(n %in% n1) {
      res[[n]] = str1[[n]]
    } else if(n %in% n2) {
      res[[n]] = str2[[n]]
    }
  }
  return(res)
}

#Pools the information of two stratum observations
.mergeStratumObservations<-function(strobs1, strobs2, attIDmap) {
  n1 = names(strobs1)
  n2 = names(strobs2)
  strObs2 = .applyAttributeMappingToStratumObservations(strobs2, attIDmap)
  npool = unique(c(n1,n2))
  res = list()
  for(n in npool) {
    if((n %in% n1) && (n %in% n2)) {
      if(strobs1[[n]]!=strobs2[[n]]) stop(paste0("Stratum observations have different data for '", n, "'. Cannot merge."))
      res[[n]] = strobs1[[n]]
    } else if(n %in% n1) {
      res[[n]] = strobs1[[n]]
    } else if(n %in% n2) {
      res[[n]] = strobs2[[n]]
    }
  }
  return(res)
}

#Pools the information of two surface cover observations
.mergeSurfaceCoverObservations<-function(scobs1, scobs2, attIDmap) {
  n1 = names(scobs1)
  n2 = names(scobs2)
  scobs2 = .applyAttributeMappingToStratumObservations(scobs2, attIDmap)
  npool = unique(c(n1,n2))
  res = list()
  for(n in npool) {
    if((n %in% n1) && (n %in% n2)) {
      if(scobs1[[n]]!=scobs2[[n]]) stop(paste0("Surface cover observations have different data for '", n, "'. Cannot merge."))
      res[[n]] = strobs1[[n]]
    } else if(n %in% n1) {
      res[[n]] = scobs1[[n]]
    } else if(n %in% n2) {
      res[[n]] = scobs2[[n]]
    }
  }
  return(res)
}

#Pools the information of two aggregate organism observations
.mergeAggregateOrganismObservations<-function(aggobs1, aggobs2, attIDmap) {
  n1 = names(aggobs1)
  n2 = names(aggobs2)
  aggobs2 = .applyAttributeMappingToAggregatePlotObservations(aggobs2, attIDmap)
  npool = unique(c(n1,n2))
  res = list()
  for(n in npool) {
    if((n %in% n1) && (n %in% n2)) {
      if(aggobs1[[n]]!=aggobs2[[n]]) stop(paste0("Aggregate organism observations have different data for '", n, "'. Cannot merge."))
      res[[n]] = aggobs1[[n]]
    } else if(n %in% n1) {
      res[[n]] = aggobs1[[n]]
    } else if(n %in% n2) {
      res[[n]] = aggobs2[[n]]
    }
  }
  return(res)
}
#Pools the information of two individual organisms
.mergeIndividualOrganisms<-function(ind1, ind2) {
  n1 = names(ind1)
  n2 = names(ind2)
  npool = unique(c(n1,n2))
  res = list()
  for(n in npool) {
    if((n %in% n1) && (n %in% n2)) {
      if(ind1[[n]]!=ind2[[n]]) stop(paste0("Individual organisms have different data for '", n, "'. Cannot merge."))
      res[[n]] = ind1[[n]]
    } else if(n %in% n1) {
      res[[n]] = ind1[[n]]
    } else if(n %in% n2) {
      res[[n]] = ind2[[n]]
    }
  }
  return(res)
}

#Pools the information of two individual organism observations
.mergeIndividualOrganismObservations<-function(indobs1, indobs2, attIDmap) {
  n1 = names(indobs1)
  n2 = names(indobs2)
  indobs2 = .applyAttributeMappingToIndividualOrganismObservations(indobs2, attIDmap)
  npool = unique(c(n1,n2))
  res = list()
  for(n in npool) {
    if((n %in% n1) && (n %in% n2)) {
      if(indobs1[[n]]!=indobs2[[n]]) stop(paste0("Individual organism observations have different data for '", n, "'. Cannot merge."))
      res[[n]] = indobs1[[n]]
    } else if(n %in% n1) {
      res[[n]] = indobs1[[n]]
    } else if(n %in% n2) {
      res[[n]] = indobs2[[n]]
    }
  }
  return(res)
}

# Pools the information of two site observations
# Measurements
.mergeSiteObservations<-function(siteobs1, siteobs2, attIDmap) {
  n1 = names(siteobs1)
  n2 = names(siteobs2)
  siteobs2 = .applyAttributeMappingToSiteObservations(siteobs2, attIDmap)
  npool = unique(c(n1,n2)) # these are soilMeasurements, climateMeasurements, ...
  res = list()
  for(n in npool) {
    if((n %in% n1) && (n %in% n2)) {
      res[[n]] = c(siteobs1[[n]], siteobs2[[n]])# add both vector elements to the result
    } else if(n %in% n1) {
      res[[n]] = siteobs1[[n]]
    } else if(n %in% n2) {
      res[[n]] = siteobs2[[n]]
    }
  }
  return(res)
}
