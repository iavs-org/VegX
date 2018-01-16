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
        attVec[cnt] = names(target@attributes[[i]])
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
