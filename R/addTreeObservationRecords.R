#' Add tree observation records
#'
#' Adds tree observation records to a VegX object from a data table,
#' using a mapping to identify columns: plot, observation date, taxon name and diameter.
#' Additional mappings can be used to specify a stratum where the tree is located.
#'
#' @param target The initial object of class \code{\linkS4class{VegX}} to be modified
#' @param x A data frame where each row corresponds to one tree observation. Columns can be varied.
#' @param projectTitle A string to identify the project title, which can be the same as one of the currently defined in \code{target}.
#' @param mapping A list with element names 'plotName', 'obsStartDate', 'taxonAuthorName' and 'diameter', used to specify the mapping of data columns (specified using strings for column names) onto these variables.
#'                Additional optional mappings are: 'subPlotName', 'obsEndDate', 'individual', 'height' and 'stratumName'.
#' @param diameterMethod
#' @param heightMethod
#' @param stratumDefinition An object of class \code{\linkS4class{VegXStrata}} indicating the definition of strata.
#' @param missing.values A vector of diameter/height values that should be considered as missing observations/measurements.
#' @param verbose A boolean flag to indicate console output of the data integration process.
#'
#' @return The modified object of class \code{\linkS4class{VegX}}.
#' @export
#'
#' @seealso \code{\link{addTaxonObservationRecords}}.
#'
#' @examples
addTreeObservationRecords<-function(target, x, projectTitle,
                                    mapping,
                                    diameterMethod,
                                    heightMethod = NULL,
                                    stratumDefinition = NULL,
                                    missing.values = c(NA, 0),
                                    verbose = TRUE) {
  x = as.data.frame(x)
  nrecords = nrow(x)
  nmissing = 0

  #Check columns exist
  for(i in 1:length(mapping)) {
    if(!(mapping[i] %in% names(x))) stop(paste0("Variable '", mapping[i],"' not found in column names. Revise mapping or data."))
  }
  plotNames = as.character(x[[mapping[["plotName"]]]])
  obsStartDates = as.Date(x[[mapping[["obsStartDate"]]]])
  taxonAuthorNames = as.character(x[[mapping[["taxonAuthorName"]]]])
  diameters = as.character(x[[mapping[["diameter"]]]])

  #Optional mappings
  stratumFlag = ("stratumName" %in% names(mapping))
  if(stratumFlag) {
    stratumNames = as.character(x[[mapping[["stratumName"]]]])
    if(is.null(stratumDefinition)) stop("Stratum definition must be supplied to map stratum observations.\n  Revise mapping or provide a stratum definition.")
  } else {
    if(!is.null(stratumDefinition)) stop("You need to include a mapping for 'stratumName' in order to map stratum observations.")
  }
  obsEndFlag = ("obsEndDate" %in% names(mapping))
  if(obsEndFlag) {
    obsEndDates = as.Date(x[[mapping[["obsEndDate"]]]])
  }
  subPlotFlag = ("subPlotName" %in% names(mapping))
  if(subPlotFlag) {
    subPlotNames = as.character(x[[mapping[["subPlotName"]]]])
  }
  heightFlag = ("height" %in% names(mapping))
  if(stratumFlag) {
    heights = as.character(x[[mapping[["height"]]]])
    if(is.null(heightMethod)) stop("Height method must be supplied to map individual heights. Revise mapping or provide a method.")
  } else {
    if(!is.null(heightMethod)) stop("You need to include a mapping for 'height' in order to map individual heights.")
  }
  individualFlag = ("individual" %in% names(mapping))
  if(stratumFlag) {
    individuals = as.character(x[[mapping[["individual"]]]])
  }
  #get project ID and add new project if necessary
  nprid = .newProjectIDByTitle(target,projectTitle)
  projectID = nprid$id
  if(nprid$new) {
    target@projects[[projectID]] = list("title" = projectTitle)
    if(verbose) cat(paste0(" New project '", projectTitle,"' added.\n"))
  } else {
    if(verbose) cat(paste0(" Data will be added to existing project '", projectTitle,"'.\n"))
  }

  #methods/attributes (WARNING: method match should be made by attributes?)
  nmtid = .newMethodIDByName(target,diameterMethod@name)
  methodID = nmtid$id
  diameterCodes = character(0)
  diamAttIDs = character(0)
  if(nmtid$new) {
    target@methods[[methodID]] = list(name = diameterMethod@name,
                                      description = diameterMethod@description,
                                      attributeClass = diameterMethod@attributeClass,
                                      attributeType = diameterMethod@attributeType)
    if(verbose) cat(paste0(" Diameter measurement method '", diameterMethod@name,"' added.\n"))
    # add attributes if necessary
    cnt = length(target@attributes)+1
    for(i in 1:length(diameterMethod@attributes)) {
      attid = as.character(cnt)
      target@attributes[[attid]] = diameterMethod@attributes[[i]]
      target@attributes[[attid]]$methodID = methodID
      diamAttIDs[i] = attid
      if(diameterMethod@attributes[[i]]$type != "quantitative") diameterCodes[i] = diameterMethod@attributes[[i]]$code
      cnt = cnt + 1
    }
  } else {
    diameterCodes = .getAttributeCodesByMethodID(methodID)
    diamAttIDs = .getAttributeIDsByMethodID(methodID)
    if(verbose) cat(paste0(" Diameter measurement method '", diameterMethod@name,"' already included.\n"))
  }

  if(heightFlag) {
    nmtid = .newMethodIDByName(target,heightMethod@name)
    methodID = nmtid$id
    heightCodes = character(0)
    heightAttIDs = character(0)
    if(nmtid$new) {
      target@methods[[methodID]] = list(name = heightMethod@name,
                                        description = heightMethod@description,
                                        attributeClass = heightMethod@attributeClass,
                                        attributeType = heightMethod@attributeType)
      if(verbose) cat(paste0(" Diameter measurement method '", heightMethod@name,"' added.\n"))
      # add attributes if necessary
      cnt = length(target@attributes)+1
      for(i in 1:length(heightMethod@attributes)) {
        attid = as.character(cnt)
        target@attributes[[attid]] = heightMethod@attributes[[i]]
        target@attributes[[attid]]$methodID = methodID
        heightAttIDs[i] = attid
        if(heightMethod@attributes[[i]]$type != "quantitative") heightCodes[i] = heightMethod@attributes[[i]]$code
        cnt = cnt + 1
      }
    } else {
      heightCodes = .getAttributeCodesByMethodID(methodID)
      heightAttIDs = .getAttributeIDsByMethodID(methodID)
      if(verbose) cat(paste0(" Diameter measurement method '", heightMethod@name,"' already included.\n"))
    }

  }

  # stratum definition
  if(stratumFlag) {
    # stratum definition method (WARNING: method match should be made by attributes?)
    stratumDefMethod = stratumDefinition@method
    snmtid = .newMethodIDByName(target,stratumDefMethod@name)
    strmethodID = snmtid$id
    if(snmtid$new) {
      target@methods[[strmethodID]] = list(name = stratumDefMethod@name,
                                           description = stratumDefMethod@description,
                                           attributeClass = stratumDefMethod@attributeClass,
                                           attributeType = stratumDefMethod@attributeType)
      if(verbose) cat(paste0(" Stratum definition method '", stratumDefMethod@name,"' added.\n"))
      # add attributes if necessary
      cnt = length(target@attributes)+1
      for(i in 1:length(stratumDefMethod@attributes)) {
        attid = as.character(cnt)
        target@attributes[[attid]] = stratumDefMethod@attributes[[i]]
        target@attributes[[attid]]$methodID = strmethodID
        cnt = cnt + 1
      }
      # add strata (beware of new strata)
      orinstrata = length(target@strata)
      nstr = length(stratumDefinition@strata)
      stratumIDs = character(0)
      cnt = length(target@strata)+1
      for(i in 1:nstr) {
        strid = as.character(cnt)
        stratumIDs[i] = strid
        target@strata[[strid]] = stratumDefinition@strata[[i]]
        target@strata[[strid]]$methodID = strmethodID
        cnt = cnt + 1
      }
      finnstrata = length(target@strata)
      if(verbose) {
        cat(paste0(" ", finnstrata-orinstrata, " new stratum definitions added.\n"))
      }
    } else { #Read stratum IDs and stratum names from selected method
      stratumIDs = .getStratumIDsByMethodID(strmethodID)
      if(verbose) cat(paste0(" Stratum definition '", stratumDefMethod@name,"' already included.\n"))
    }
  }


  orinplots = length(target@plots)
  orinplotobs = length(target@plotObservations)
  orinstrobs = length(target@stratumObservations)
  orintuc = length(target@taxonNameUsageConcepts)
  orininds = length(target@individualOrganisms)
  orinindobs = length(target@individualObservations)
  parsedPlots = character(0)
  parsedPlotIDs = character(0)
  parsedPlotObs = character(0)
  parsedPlotObsIDs = character(0)
  parsedTNUCs = character(0)
  parsedTNUCIDs = character(0)
  parsedStrObs = character(0)
  parsedStrObsIDs = character(0)
  parsedInds = character(0)
  parsedIndIDs = character(0)
  indObsCounter = orinindobs+1 #counter
  #Record parsing loop
  for(i in 1:nrecords) {
    #plot
    if(!(plotNames[i] %in% parsedPlots)) {
      npid = .newPlotIDByName(target, plotNames[i]) # Get the new plot ID (internal code)
      plotID = npid$id
      if(npid$new) target@plots[[plotID]] = list("plotName" = plotNames[i])
      parsedPlots = c(parsedPlots, plotNames[i])
      parsedPlotIDs = c(parsedPlotIDs, plotID)
    } else { #this access should be faster
      plotID = parsedPlotIDs[which(parsedPlots==plotNames[i])]
    }
    #subplot (if defined)
    if(subPlotFlag){
      if(!is.na(subPlotNames[i])) {
        subPlotCompleteName = paste0(plotNames[i],"_", subPlotNames[i])
        if(!(subPlotCompleteName %in% parsedPlots)) {
          parentPlotID = plotID
          npid = .newPlotIDByName(target, subPlotCompleteName) # Get the new subplot ID (internal code)
          plotID = npid$id
          if(npid$new) target@plots[[plotID]] = list("plotName" = subPlotCompleteName,
                                                     "parentPlotID" = parentPlotID)
          parsedPlots = c(parsedPlots, subPlotCompleteName)
          parsedPlotIDs = c(parsedPlotIDs, plotID)
        } else { #this access should be faster
          plotID = parsedPlotIDs[which(parsedPlots==subPlotCompleteName)]
        }
      }
    }
    #plot observation
    pObsString = paste(plotID, obsStartDates[i]) # plotID+Date
    if(!(pObsString %in% parsedPlotObs)) {
      npoid = .newPlotObsIDByDate(target, plotID, obsStartDates[i]) # Get the new plot observation ID (internal code)
      plotObsID = npoid$id
      if(npoid$new) {
        target@plotObservations[[plotObsID]] = list("plotID" = plotID,
                                                    "projectID" = projectID,
                                                    "obsStartDate" = obsStartDates[i])
        if(obsEndFlag) target@plotObservations[[plotObsID]]$obsEndDate = obsEndDates[i]
      }
      parsedPlotObs = c(parsedPlotObs, pObsString)
      parsedPlotObsIDs = c(parsedPlotObsIDs, plotObsID)
    } else {
      plotObsID = parsedPlotIDs[which(parsedPlotObs==pObsString)]
    }
    # taxon name
    if(!(taxonAuthorNames[i] %in% parsedTNUCs)) {
      ntnucid = .newTaxonNameUsageConceptIDByName(target, taxonAuthorNames[i]) # Get the new taxon name usage ID (internal code)
      tnucID = ntnucid$id
      if(ntnucid$new) target@taxonNameUsageConcepts[[tnucID]] = list("authorName" = taxonAuthorNames[i])
      parsedTNUCs = c(parsedTNUCs, taxonAuthorNames[i])
      parsedTNUCIDs = c(parsedTNUCIDs, tnucID)
    } else {
      tnucID = parsedTNUCIDs[which(parsedTNUCs==taxonAuthorNames[i])]
    }

    # strata
    if(stratumFlag) {
      strID = .getStratumIDByName(target, stratumNames[i])
      if(is.null(strID)) stop(paste0(stratumNames[i]," not found within stratum names. Revise stratum definition or data."))
      strObsString = paste(plotObsID, strID) # plotObsID+stratumID
      if(!(strObsString %in% parsedStrObs)) {
        nstroid = .newStratumObsIDByIDs(target, plotObsID, strID) # Get the new stratum observation ID (internal code)
        strObsID = nstroid$id
        if(nstroid$new) target@stratumObservations[[strObsID]] = list("plotObservationID" = plotObsID,
                                                                      "stratumID" = strID)
        parsedStrObs = c(parsedStrObs, strObsString)
        parsedStrObsIDs = c(parsedStrObsIDs, strObsID)
      } else {
        strObsID = parsedStrObsIDs[which(parsedStrObs==stratumNames[i])]
      }
    }

    # individual organisms
    if(individualFlag) { # Allow for repeated observations on the same individuals
      if(!(individuals[i] %in% parsedInds)) {
        nindid = .newIndividualByIdentificationLabel(target, individuals[i]) # Get the new individual ID (internal code)
        indID = nindid$id
        if(nindid$new) target@individualOrganisms[[indID]] = list("taxonNameUsageConceptID" = tnucID,
                                                                  "identificationLabel" = individuals[i])
        parsedInds = c(parsedInds, individuals[i])
        parsedIndIDs = c(parsedIndIDs, indID)
      } else {
        indID = parsedIndIDs[which(parsedInds==individuals[i])]
      }
    } else { # Add a new individual for each individual observation record
      indID = as.character(length(target@individualOrganisms)+1)
      target@individualOrganisms[[indID]] = list("taxonNameUsageConceptID" = tnucID)
    }

    # agg org observations
    if(!(diameters[i] %in% as.character(missing.values))) {
      if(diameterMethod@attributeType== "quantitative") {
        value = as.numeric(diameters[i])
        if(value> diameterMethod@attributes[[1]]$upperBound) {
          stop(paste0("Diameter '", value,"' larger than upper bound of diameter measurement definition. Please revise scale or data."))
        }
        else if(value < diameterMethod@attributes[[1]]$lowerBound) {
          stop(paste0("Diameter '", value,"' smaller than lower bound of diameter measurement definition. Please revise scale or data."))
        }
        target@individualObservations[[as.character(indObsCounter)]] = list("plotObservationID" = plotObsID,
                                                                            "individualOrganismID" = indID,
                                                                            "diameterID" = diamAttIDs[1],
                                                                            "diameterValue" = value)
        if(stratumFlag) target@individualObservations[[as.character(indObsCounter)]]$stratumObservationID = strObsID
        indObsCounter = indObsCounter + 1
      } else {
        ind = which(diameterCodes==as.character(diameters[i]))
        if(length(ind)==1) {
          target@individualObservations[[as.character(indObsCounter)]] = list("plotObservationID" = plotObsID,
                                                                              "individualOrganismID" = indID,
                                                                              "attributeID" = diamAttIDs[ind],
                                                                              "diameterValue" = diameters[i])
          if(stratumFlag) target@individualObservations[[as.character(indObsCounter)]]$stratumObservationID = strObsID
          indObsCounter = indObsCounter + 1
        }
        else stop(paste0("Diameter '", diameters[i],"' not found in diameter measurement definition. Please revise diameter classes or data."))
      }
    } else {
      nmissing = nmissing + 1
    }
  }
  finnplots = length(target@plots)
  finnplotobs = length(target@plotObservations)
  finnstrobs = length(target@stratumObservations)
  finntuc = length(target@taxonNameUsageConcepts)
  finninds = length(target@individualOrganisms)
  finnindobs = length(target@individualObservations)
  if(verbose) {
    cat(paste0(" ", finnplots-orinplots, " new plot(s) added.\n"))
    cat(paste0(" ", finnplotobs-orinplotobs, " new plot observation(s) added.\n"))
    cat(paste0(" ", finntuc-orintuc, " new taxon name usage concept(s) added.\n"))
    if(stratumFlag) cat(paste0(" ", finnstrobs-orinstrobs, " new stratum observation(s) added.\n"))
    cat(paste0(" ", finninds-orininds, " new individual organism(s) added.\n"))
    cat(paste0(" ", finnindobs-orinindobs, " new individual organism observation(s) added.\n"))
    if(nmissing>0) cat(paste0(" ", nmissing, " individual organism observation(s) with missing diameter value(s) not added.\n"))
  }

  return(target)
}
