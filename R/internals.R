# Returns the projectID for a new project in the data set or the ID of an existing project with the same name
.newProjectIDByTitle<-function(target, projectTitle) {
  if(length(target@projects)==0) return(list(id="1", new = TRUE))
  for(i in 1:length(target@projects)) {
    if(target@projects[[i]]$title==projectTitle) return(list(id=names(target@projects)[i], new = FALSE))
  }
  return(list(id = as.character(length(target@projects)+1), new = TRUE))
}
# Returns the plotID for a new plot in the data set or the ID of an existing plot with the same name
.newPlotIDByName<-function(target, plotName) {
  if(length(target@plots)==0) return(list(id="1", new = TRUE))
  for(i in 1:length(target@plots)) {
    if(target@plots[[i]]$plotName==plotName) return(list(id = names(target@plots)[i], new = FALSE))
  }
  return(list(id = as.character(length(target@plots)+1), new = TRUE))
}
# Returns the ID for a new plot observation in the data set or the ID of an existing plot observation
.newPlotObsIDByDate<-function(target, plotID, obsStartDate) {
  obsStartDate = as.Date(obsStartDate)
  if(length(target@plotObservations)==0) return(list(id="1", new = TRUE))
  for(i in 1:length(target@plotObservations)) {
    if((target@plotObservations[[i]]$plotID==plotID) && (target@plotObservations[[i]]$obsStartDate==obsStartDate)) return(list(id = names(target@plotObservations)[i], new = FALSE))
  }
  return(list(id = as.character(length(target@plotObservations)+1), new = TRUE))
}
# Returns the ID for a new stratum in the data set or the ID of an existing stratum
.newStratumIDByName<-function(target, methodID, stratumName) {
  if(length(target@strata)==0) return(list(id="1", new = TRUE))
  for(i in 1:length(target@strata)) {
    if((target@strata[[i]]$methodID==methodID) && (target@strata[[i]]$stratumName==stratumName)) return(list(id = names(target@strata)[i], new = FALSE))
  }
  return(list(id = as.character(length(target@strata)+1), new = TRUE))
}
# Returns the ID for a new stratum observation in the data set or the ID of an existing stratum observation
.newStratumObsIDByIDs<-function(target, plotObservationID, stratumID) {
  if(length(target@stratumObservations)==0) return(list(id="1", new = TRUE))
  for(i in 1:length(target@stratumObservations)) {
    if((target@stratumObservations[[i]]$plotObservationID==plotObservationID) && (target@stratumObservations[[i]]$stratumID==stratumID)) return(list(id = names(target@stratumObservations)[i], new = FALSE))
  }
  return(list(id = as.character(length(target@stratumObservations)+1), new = TRUE))
}

# Returns the ID for a new method in the data set or the ID of an existing method with the same name
.newMethodIDByName<-function(target, methodName) {
  if(length(target@methods)==0) return(list(id="1", new = TRUE))
  for(i in 1:length(target@methods)) {
    if(target@methods[[i]]$name==methodName) return(list(id = names(target@methods)[i], new = FALSE))
  }
  return(list(id = as.character(length(target@methods)+1), new = TRUE))
}
# Returns the ID for a new taxon name usage concept in the data set or the ID of an existing taxon name usage concept with the same name
.newTaxonNameUsageConceptIDByName<-function(target, authorName) {
  if(length(target@taxonNameUsageConcepts)==0) return(list(id="1", new = TRUE))
  for(i in 1:length(target@taxonNameUsageConcepts)) {
    if(target@taxonNameUsageConcepts[[i]]$authorName==authorName) return(list(id = names(target@taxonNameUsageConcepts)[i], new = FALSE))
  }
  return(list(id = as.character(length(target@taxonNameUsageConcepts)+1), new = TRUE))
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



.getStratumIDByName<-function(target, stratumName) {
  if(length(target@strata)>0) {
    for(i in 1:length(target@strata)) {
      if(target@strata[[i]]$stratumName==stratumName) return(names(target@strata)[i])
    }
  }
  return(NULL)
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

#Pools the information of two plots
.mergePlots<-function(plot1, plot2) {
   n1 = names(plot1)
   n2 = names(plot2)
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
.mergePlotObservations<-function(plotObservation1, plotObservation2) {
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
