.nextPartyID<-function(target) {
  if(length(target@parties)==0) return("1")
  return(as.character(as.numeric(names(target@parties)[length(target@parties)])+1))
}
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
.nextCommunityObservationID<-function(target) {
  if(length(target@communityObservations)==0) return("1")
  return(as.character(as.numeric(names(target@communityObservations)[length(target@communityObservations)])+1))
}
.nextSiteObservationID<-function(target) {
  if(length(target@siteObservations)==0) return("1")
  return(as.character(as.numeric(names(target@siteObservations)[length(target@siteObservations)])+1))
}
.nextLiteratureCitationID<-function(target) {
  if(length(target@literatureCitations)==0) return("1")
  return(as.character(as.numeric(names(target@literatureCitations)[length(target@literatureCitations)])+1))
}
.nextMethodID<-function(target) {
  if(length(target@methods)==0) return("1")
  return(as.character(as.numeric(names(target@methods)[length(target@methods)])+1))
}
.nextAttributeID<-function(target) {
  if(length(target@attributes)==0) return("1")
  return(as.character(as.numeric(names(target@attributes)[length(target@attributes)])+1))
}
.nextOrganismNameID<-function(target) {
  if(length(target@organismNames)==0) return("1")
  return(as.character(as.numeric(names(target@organismNames)[length(target@organismNames)])+1))
}
.nextTaxonConceptID<-function(target) {
  if(length(target@taxonConcepts)==0) return("1")
  return(as.character(as.numeric(names(target@taxonConcepts)[length(target@taxonConcepts)])+1))
}
.nextOrganismIdentityID<-function(target) {
  if(length(target@organismIdentities)==0) return("1")
  return(as.character(as.numeric(names(target@organismIdentities)[length(target@organismIdentities)])+1))
}
.nextIndividualOrganismLabelForPlot<-function(target, plotID) {
  return(paste0("ind", .getNumberOfOrganismsByPlotID(target, plotID)+1))
}


# Returns the partyID for a new party in the data set or the ID of an existing party with the same name
.newPartyIDByName<-function(target, partyName) {
  if(length(target@parties)==0) return(list(id="1", new = TRUE))
  for(i in 1:length(target@parties)) {
    if(target@parties[[i]]$name==partyName) return(list(id=names(target@parties)[i], new = FALSE))
  }
  return(list(id = .nextPartyID(target), new = TRUE))
}
# Returns the projectID for a new project in the data set or the ID of an existing project with the same title
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
# Returns the plotID for a new plot in the data set or the ID of an existing plot with the same name
.newPlotIDByNameAndUniqueIdentifier<-function(target, plotName, plotUniqueIdentifier) {
  if(length(target@plots)==0) return(list(id="1", new = TRUE))
  for(i in 1:length(target@plots)) {
    if(!("plotUniqueIdentifier" %in% names(target@plots[[i]]))) {
      if(target@plots[[i]]$plotName==plotName) return(list(id = names(target@plots)[i], new = FALSE))
    } else if(plotUniqueIdentifier=="") { #If unique identifier is missing in both plots, compare names only
      if((target@plots[[i]]$plotName==plotName) &&  (target@plots[[i]]$plotUniqueIdentifier==plotUniqueIdentifier)) return(list(id = names(target@plots)[i], new = FALSE))
    }
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
.newAggregateOrganismObservationIDByOrganismIdentityID<-function(target, plotObservationID, stratumObservationID, oiID) {
  if(length(target@aggregateObservations)==0) return(list(id="1", new = TRUE))
  for(i in 1:length(target@aggregateObservations)) {
    if((target@aggregateObservations[[i]]$plotObservationID==plotObservationID) &&
       (target@aggregateObservations[[i]]$stratumObservationID==stratumObservationID) &&
       (target@aggregateObservations[[i]]$organismIdentity==oiID))
      return(list(id = names(target@aggregateObservations)[i], new = FALSE))
  }
  return(list(id = .nextAggregateOrganismObservationID(target), new = TRUE))
}
# Returns the ID for a new individual organism in the data set or the ID of an existing organism
.newIndividualOrganismIDByIndividualOrganismLabel<-function(target, plotID, individualOrganismLabel) {
  if(length(target@individualOrganisms)==0) return(list(id="1", new = TRUE))
  for(i in 1:length(target@individualOrganisms)) {
    if((target@individualOrganisms[[i]]$plotID==plotID) && (target@individualOrganisms[[i]]$individualOrganismLabel==individualOrganismLabel)) return(list(id = names(target@individualOrganisms)[i], new = FALSE))
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
# Returns the ID for a new community observation in the data set or the ID of an existing community observation
.newCommunityObservationIDByID<-function(target, plotObservationID) {
  if(length(target@communityObservations)==0) return(list(id="1", new = TRUE))
  for(i in 1:length(target@communityObservations)) {
    if(target@communityObservations[[i]]$plotObservationID==plotObservationID) return(list(id = names(target@communityObservations)[i], new = FALSE))
  }
  return(list(id = .nextCommunityObservationID(target), new = TRUE))
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
# Returns the ID for a new literature citation in the data set or the ID of an existing one with the same string
.newLiteratureCitationIDByCitationString<-function(target, citationString) {
  if(length(target@literatureCitations)==0) return(list(id="1", new = TRUE))
  for(i in 1:length(target@literatureCitations)) {
    if(target@literatureCitations[[i]]$citationString==citationString) return(list(id = names(target@literatureCitations)[i], new = FALSE))
  }
  return(list(id = .nextLiteratureCitationID(target), new = TRUE))
}
# Returns the ID for a new organism name in the data set or the ID of an existing organismName with the same name
.newOrganismNameIDByName<-function(target, organismName, taxon) {
  if(length(target@organismNames)==0) return(list(id="1", new = TRUE))
  for(i in 1:length(target@organismNames)) {
    if((target@organismNames[[i]]$name==organismName) && (target@organismNames[[i]]$taxon==taxon)) return(list(id = names(target@organismNames)[i], new = FALSE))
  }
  return(list(id = .nextOrganismNameID(target), new = TRUE))
}
.newTaxonConceptIDByNameCitation<-function(target, organismName, citationString) {
  if(length(target@taxonConcepts)==0) return(list(id="1", new = TRUE))
  for(i in 1:length(target@taxonConcepts)) {
    on  = target@organismNames[[target@taxonConcepts[[i]]$organismNameID]]$name
    cs = target@literatureCitations[[target@taxonConcepts[[i]]$accordingToCitationID]]$citationString
    if(length(organismName)>1) stop("Organism name has length > 1.\n")

    if((on==organismName) && (cs==citationString)) {
      return(list(id = names(target@taxonConcepts)[i], new = FALSE))
    }
  }
  return(list(id = .nextTaxonConceptID(target), new = TRUE))

}

# Returns the ID for a new organism identity in the data set or the ID of an existing organism identity with the same taxon concept
.newOrganismIdentityIDByTaxonConcept<-function(target, organismName, citationString) {
  if(length(target@organismIdentities)==0) return(list(id="1", new = TRUE))
  for(i in 1:length(target@organismIdentities)) {
    on = .getOrganismIdentityName(target, i)
    citString = .getOrganismIdentityCitationString(target, i)
    if((on==organismName) && (citString == citationString)) {
       return(list(id = names(target@organismIdentities)[i], new = FALSE))
    }
  }
  return(list(id = .nextOrganismIdentityID(target), new = TRUE))
}


.getOrganismIdentityName<-function(target,  identityID) {
  oi = target@organismIdentities[[identityID]]
  if("preferredTaxonNomenclature" %in% names(oi)) {
    oriName = target@organismNames[[oi$preferredTaxonNomenclature$preferredTaxonNameID]]$name
  } else {
    oriName = target@organismNames[[oi$originalOrganismNameID]]$name
  }
  return(oriName)
}
.getOrganismIdentityCitationString<-function(target,  identityID) {
  citationString = ""
  oi = target@organismIdentities[[identityID]]
  if("originalConceptIdentification" %in% names(oi)) {
    tc = target@taxonConcepts[[oi$originalConceptIdentification$taxonConceptID]]
    citationString = target@literatureCitations[[tc$citationID]]$citationString
  }
  return(citationString)
}
.getIndividualOrganismIdentityName <-function(target, individualID){
  return(.getOrganismIdentityName(target, target@individualOrganisms[[individualID]]$organismIdentityID))
}
# Counts the number of organisms by plot id
.getNumberOfOrganismsByPlotID<-function(target, plotID) {
  indCount = 0
  if(length(target@individualOrganisms)>0) {
    for(i in 1:length(target@individualOrganisms)) {
      if(target@individualOrganisms[[i]]$plotID==plotID) {
          indCount = indCount + 1
      }
    }
  }
  return(indCount)
}
# Returns organism identities having the corresponding original organism name ID
.getOrganismIdentityIDsByOriginalOrganismNameID<-function(target, originalOrganismNameID) {
  orgIdVec = character(0)
  if(length(target@organismIdentities)>0) {
    cnt = 1
    for(i in 1:length(target@organismIdentities)) {
      if(target@organismIdentities[[i]]$originalOrganismNameID==originalOrganismNameID) {
        orgIdVec[cnt] = names(target@organismIdentities)[i]
        cnt = cnt + 1
      }
    }
  }
  return(orgIdVec)
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

# Returns surafce names corresponding to the input method
.getSurfaceTypeNamesByMethodID<-function(target, methodID) {
  sVec = character(0)
  if(length(target@surfaceTypes)>0) {
    cnt = 1
    for(i in 1:length(target@surfaceTypes)) {
      if(target@surfaceTypes[[i]]$methodID==methodID) {
        sVec[cnt] = target@surfaceTypes[[i]]$surfaceName
        cnt = cnt + 1
      }
    }
  }
  return(sVec)
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


.applyMappingsToMethod<-function(method, litIDmap){
  if("citationID" %in% names(method)) {
    method$citationID = litIDmap[[method$citationID]]
  }
  return(method)
}
.applyMappingsToTaxonConcept<-function(taxonConcept, onIDmap, litIDmap){
  if("organismNameID" %in% names(taxonConcept)) {
    taxonConcept$organismNameID = onIDmap[[taxonConcept$organismNameID]]
  }
  if("accordingToCitationID" %in% names(taxonConcept)) {
    taxonConcept$accordingToCitationID = litIDmap[[taxonConcept$accordingToCitationID]]
  }
  return(taxonConcept)
}

.applyMappingsToOrganismIdentity<-function(organismIdentity, onIDmap, tcIDmap){
  if("originalOrganismNameID" %in% names(organismIdentity)) {
    organismIdentity$originalOrganismNameID = onIDmap[[organismIdentity$originalOrganismNameID]]
  }
  if("originalConceptIdentification" %in% names(organismIdentity)) {
    organismIdentity$originalConceptIdentification$taxonConceptID = tcIDmap[[organismIdentity$originalConceptIdentification$taxonConceptID]]
  }
  return(organismIdentity)
}
#Translate IDs in a project element
.applyMappingsToProject<-function(project, partyIDmap, litIDmap) {
  n = names(project)
  for(i in 1:length(n)) {
    # Update party codes
    if(n[[i]]=="personnel")  project[[i]][[1]] = partyIDmap[[project[[i]][[1]]]]
    if(n[[i]]=="documentCitationID")  project[[i]] = litIDmap[[project[[i]]]]
  }
  return(project)
}

#Translate IDs in a plot element
.applyMappingsToPlot<-function(plot, partyIDmap, attIDmap) {
  for(n in names(plot)) {
    # Update party codes
    if(n=="placementPartyID")  plot[[n]] = partyIDmap[[plot[[n]]]]
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
.applyMappingsToIndividualOrganism<-function(indOrg, plotIDmap, oiIDmap) {
  indOrg$plotID = plotIDmap[[indOrg$plotID]]
  indOrg$organismIdentityID = oiIDmap[[indOrg$organismIdentityID]]
  return(indOrg)
}
.applyMappingsToPlotObservation<-function(plotObs, plotIDmap, projectIDmap) {
  plotObs$plotID = plotIDmap[[plotObs$plotID]]
  if("projectID" %in% names(plotObs)) plotObs$projectID = projectIDmap[[plotObs[["projectID"]]]]
  return(plotObs)
}

.applyMappingsToStratumObservation<-function(strobs, strIDmap, plotObsIDmap, attIDmap) {
  strobs$stratumID = strIDmap[[strobs$stratumID]]
  strobs$plotObservationID = plotObsIDmap[[strobs$plotObservationID]]
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

.applyMappingsToIndividualOrganismObservation<-function(indObs, plotObsIDmap, strObsIDmap, indIDmap, attIDmap) {
  indObs$plotObservationID = plotObsIDmap[[indObs$plotObservationID]]
  indObs$individualOrganismID = indIDmap[[indObs$individualOrganismID]]
  if("stratumObservationID" %in% names(indObs)) {
    indObs$stratumObservationID = strObsIDmap[[indObs$stratumObservationID]]
  }
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

.applyMappingsToAggregateOrganismObservation <-function(aggObs, plotObsIDmap, oiIDmap, strObsIDmap, attIDmap) {
    if("plotObservationID" %in% names(aggObs)) {
      if(aggObs$plotObservationID!="") {
        aggObs$plotObservationID = plotObsIDmap[[aggObs$plotObservationID]]
      }
    } else {
      aggObs$plotObservationID = ""
    }
    if("organismIdentityID" %in% names(aggObs)) {
      if(aggObs$organismIdentityID!="") {
        aggObs$organismIdentityID = oiIDmap[[aggObs$organismIdentityID]]
      }
    } else {
      aggObs$organismIdentityID = ""
    }
    if("stratumObservationID" %in% names(aggObs)) {
      if(aggObs$stratumObservationID!="") {
        aggObs$stratumObservationID = strObsIDmap[[aggObs$stratumObservationID]]
      }
    } else {
      aggObs$stratumObservationID = ""
    }
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
.applyMappingsToCommunityObservation<-function(commobs, plotObsIDmap, attIDmap) {
  commobs$plotObservationID = plotObsIDmap[[commobs$plotObservationID]]
  # Update attribute codes
  for(n in names(commobs)) {
    if(n %in% c("communityMeasurements")) {
      for(i in 1:length(commobs[[n]])) {
        commobs[[n]][[i]]$attributeID = attIDmap[[commobs[[n]][[i]]$attributeID]]
      }
    }
  }
  return(commobs)
}

.applyMappingsToSiteObservation<-function(siteobs, plotObsIDmap, attIDmap) {
  siteobs$plotObservationID = plotObsIDmap[[siteobs$plotObservationID]]
  # Update attribute codes
  for(n in names(siteobs)) {
    if(n %in% c("soilMeasurements", "climateMeasurements", "waterBodyMeasurements")) {
      for(i in 1:length(siteobs[[n]])) {
        siteobs[[n]][[i]]$attributeID = attIDmap[[siteobs[[n]][[i]]$attributeID]]
      }
    }
  }
  return(siteobs)
}
.applyMappingsToSurfaceCoverObservation<-function(scobs, stIDmap, plotObsIDmap, attIDmap) {
  scobs$surfaceTypeID = stIDmap[[scobs$surfaceTypeID]]
  scobs$plotObservationID = plotObsIDmap[[scobs$plotObservationID]]
  for(n in names(scobs)) {
    if(n %in% c("coverMeasurement")) {
      scobs[[n]]$attributeID = attIDmap[[scobs[[n]]$attributeID]]
    }
  }
  return(scobs)
}

#Pools the information of two parties
.mergeParties<-function(par1, par2) {
  n1 = names(par1)
  n2 = names(par2)
  npool = unique(c(n1,n2))
  res = list()
  for(n in npool) {
    if((n %in% n1) && (n %in% n2)) {
      if(par1[[n]]!=par2[[n]]) stop(paste0("Parties have different data for '", n, "'. Cannot merge."))
      res[[n]] = par1[[n]]
    } else if(n %in% n1) {
      res[[n]] = par1[[n]]
    } else if(n %in% n2) {
      res[[n]] = par2[[n]]
    }
  }
  return(res)
}

#Pools the information of two literature citations
.mergeLiteratureCitations<-function(cit1, cit2) {
  n1 = names(cit1)
  n2 = names(cit2)
  npool = unique(c(n1,n2))
  res = list()
  for(n in npool) {
    if((n %in% n1) && (n %in% n2)) {
      if(cit1[[n]]!=cit2[[n]]) stop(paste0("Literature citations have different data for '", n, "'. Cannot merge."))
      res[[n]] = cit1[[n]]
    } else if(n %in% n1) {
      res[[n]] = cit1[[n]]
    } else if(n %in% n2) {
      res[[n]] = cit2[[n]]
    }
  }
  return(res)
}

#Pools the information of two methods
.mergeMethods<-function(met1, met2, litIDmap) {
  n1 = names(met1)
  n2 = names(met2)
  met2 = .applyMappingsToMethod(met2, litIDmap)
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

#Pools the information of two organism names
.mergeOrganismNames<-function(on1, on2) {
  n1 = names(on1)
  n2 = names(on2)
  npool = unique(c(n1,n2))
  res = list()
  for(n in npool) {
    if((n %in% n1) && (n %in% n2)) {
      if(on1[[n]]!=on2[[n]]) stop(paste0("Organism names have different data for '", n, "'. Cannot merge."))
      res[[n]] = on1[[n]]
    } else if(n %in% n1) {
      res[[n]] = on1[[n]]
    } else if(n %in% n2) {
      res[[n]] = on2[[n]]
    }
  }
  return(res)
}

#Pools the information of two taxon concepts
.mergeTaxonConcepts<-function(tc1, tc2, onIDmap, litIDmap) {
  n1 = names(tc1)
  n2 = names(tc2)
  tc2 = .applyMappingsToTaxonConcept(tc2, onIDmap, litIDmap)
  npool = unique(c(n1,n2))
  res = list()
  for(n in npool) {
    if((n %in% n1) && (n %in% n2)) {
      if(tc1[[n]]!=tc2[[n]]) stop(paste0("Organism names have different data for '", n, "'. Cannot merge."))
      res[[n]] = tc1[[n]]
    } else if(n %in% n1) {
      res[[n]] = tc1[[n]]
    } else if(n %in% n2) {
      res[[n]] = tc2[[n]]
    }
  }
  return(res)
}

#Pools the information of two organism identities
.mergeOrganismIdentities<-function(oi1, oi2, onIDmap, tcIDmap) {
  n1 = names(oi1)
  n2 = names(oi2)
  oi2 = .applyMappingsToOrganismIdentity(oi2, onIDmap, tcIDmap)
  npool = unique(c(n1,n2))
  res = list()
  for(n in npool) {
    if((n %in% n1) && (n %in% n2)) {
      if(oi1[[n]]!=oi2[[n]]) stop(paste0("Organism identities have different data for '", n, "'. Cannot merge."))
      res[[n]] = oi1[[n]]
    } else if(n %in% n1) {
      res[[n]] = oi1[[n]]
    } else if(n %in% n2) {
      res[[n]] = oi2[[n]]
    }
  }
  return(res)
}

#Pools the information of two projects
.mergeProjects<-function(prj1, prj2) {
  n1 = names(prj1)
  n2 = names(prj2)
  npool = unique(c(n1,n2))
  res = list()
  for(n in npool) {
    if((n %in% n1) && (n %in% n2)) {
      if(prj1[[n]]!=prj2[[n]]) stop(paste0("Projects have different data for '", n, "'. Cannot merge."))
      res[[n]] = prj1[[n]]
    } else if(n %in% n1) {
      res[[n]] = prj1[[n]]
    } else if(n %in% n2) {
      res[[n]] = prj2[[n]]
    }
  }
  return(res)
}

#Pools the information of two plots
.mergePlots<-function(plot1, plot2, partyIDmap, attIDmap) {
   n1 = names(plot1)
   n2 = names(plot2)
   plot2 = .applyMappingsToPlot(plot2, partyIDmap, attIDmap)
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

#Pools the information of two plot observations
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

#Pools the information of two organism identitys
.mergeOrganismIdentities<-function(oi1, oi2) {
  n1 = names(oi1)
  n2 = names(oi2)
  npool = unique(c(n1,n2))
  res = list()
  for(n in npool) {
    if((n %in% n1) && (n %in% n2)) {
      if(oi1[[n]]!=oi2[[n]]) stop(paste0("Taxon name usage concepts have different data for '", n, "'. Cannot merge."))
      res[[n]] = oi1[[n]]
    } else if(n %in% n1) {
      res[[n]] = oi1[[n]]
    } else if(n %in% n2) {
      res[[n]] = oi2[[n]]
    }
  }
  return(res)
}



#Pools the information of two stratum observations
.mergeStratumObservations<-function(strobs1, strobs2) {
  n1 = names(strobs1)
  n2 = names(strobs2)
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
.mergeSurfaceCoverObservations<-function(scobs1, scobs2) {
  n1 = names(scobs1)
  n2 = names(scobs2)
  npool = unique(c(n1,n2))
  res = list()
  for(n in npool) {
    if((n %in% n1) && (n %in% n2)) {
      if(scobs1[[n]]!=scobs2[[n]]) stop(paste0("Surface cover observations have different data for '", n, "'. Cannot merge."))
      res[[n]] = scobs1[[n]]
    } else if(n %in% n1) {
      res[[n]] = scobs1[[n]]
    } else if(n %in% n2) {
      res[[n]] = scobs2[[n]]
    }
  }
  return(res)
}

#Pools the information of two aggregate organism observations
.mergeAggregateOrganismObservations<-function(aggobs1, aggobs2) {
  n1 = names(aggobs1)
  n2 = names(aggobs2)
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
.mergeIndividualOrganismObservations<-function(indobs1, indobs2) {
  n1 = names(indobs1)
  n2 = names(indobs2)
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
.mergeCommunityObservations<-function(commobs1, commobs2) {
  n1 = names(commobs1)
  n2 = names(commobs2)
  npool = unique(c(n1,n2)) # these are soilMeasurements, climateMeasurements, ...
  res = list()
  for(n in npool) {
    if((n %in% n1) && (n %in% n2)) {
      res[[n]] = c(commobs1[[n]], commobs2[[n]])# add both vector elements to the result
    } else if(n %in% n1) {
      res[[n]] = commobs1[[n]]
    } else if(n %in% n2) {
      res[[n]] = commobs2[[n]]
    }
  }
  return(res)
}

# Pools the information of two site observations
# Measurements
.mergeSiteObservations<-function(siteobs1, siteobs2) {
  n1 = names(siteobs1)
  n2 = names(siteobs2)
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

# Title: Argument Verification Using Partial or Fuzzy Matching
#
# Description These function adapts the base function `match.arg()`, so that
#   matching can be case insesitive and performed using partial or fuzzy
#   matching.
#
# @param arg a character
# @param choices a character vector of candidate values
# @param several.ok logical specifying if arg should be allowed to have more
#   than one element.
# @param ignore.case logical specifying if matching should be case sensitive?
# @param method character specifying the type of matching desired: 'partial' or 'fuzzy'.
# @param max.distance Maximum distance allowed for fuzzy matching. Expressed
#   either as integer, or as a fraction of the pattern length times the maximal
#   transformation cost.
#   
#
.matchArgExtended <- function (arg, choices, 
                                several.ok = FALSE, ignore.case = FALSE,
                                method = "partial", max.distance = 0.1) {
  
  if (missing(choices)) {
    formal.args <- formals(sys.function(sysP <- sys.parent()))
    choices <- eval(formal.args[[as.character(substitute(arg))]], 
                    envir = sys.frame(sysP))
  }
  
  if (is.null(arg)) {
    return(NA_character_)
    # return(choices[1L])
  } else {
    if (!is.character(arg)) 
      stop("'arg' must be NULL or a character vector")
  }  
  
  if (ignore.case) {
    choices.orig <- choices
    choices <- tolower(choices)
    arg.orig <- arg
    arg <- tolower(arg)
  } else {
    choices.orig <- choices
    arg.orig <- arg
  }
  
  if (!several.ok) {
    if (identical(arg, choices)) 
      return(arg.orig[1L])
    if (length(arg) > 1L) 
      stop("'arg' must be of length 1")
  } else {
    if (length(arg) == 0L) 
      stop("'arg' must be of length >= 1")
  }  
  
  if (method == "partial")
    i <- pmatch(arg, choices, nomatch = 0L, duplicates.ok = TRUE)
  if (method == "fuzzy")
    i <- agrep(arg, choices, max.distance = max.distance)
  
  if (all(i == 0L)) 
    stop(gettextf("'arg' should be one of %s", 
                  paste(dQuote(choices), collapse = ", ")), domain = NA)
  i <- i[i > 0L]
  if (!several.ok && length(i) > 1) 
    stop("there is more than one match in 'match.arg'")
  choices.orig[i]
}