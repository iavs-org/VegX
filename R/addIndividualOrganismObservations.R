#' Add individual organism observation records
#'
#' Adds individual organism observation records to a VegX object from a data table.
#'
#' @param target The initial object of class \code{\linkS4class{VegX}} to be modified
#' @param x A data frame where each row corresponds to one tree observation. Columns can be varied.
#' @param mapping A list with element names 'plotName', 'obsStartDate' used to specify the mapping of data columns (specified using strings for column names) onto these variables.
#'                Additional optional mappings are: 'subPlotName', 'individual', 'authorTaxonName', 'stratumName', 'diameterMeasurement', 'heightMeasurement', and names to identify additional specific measurements.
#' @param methods A named list of objects of class \code{\linkS4class{VegXMethod}} indicating the definition of 'diameterMeasurement', 'heightMeasurement' and any additional individual organism measurement defined in \code{mapping}.
#' @param stratumDefinition An object of class \code{\linkS4class{VegXStrata}} indicating the definition of strata.
#' @param missing.values A character vector of values that should be considered as missing observations/measurements.
#' @param verbose A boolean flag to indicate console output of the data integration process.
#'
#' @return The modified object of class \code{\linkS4class{VegX}}.
#' @export
#'
#' @references Wiser SK, Spencer N, De Caceres M, Kleikamp M, Boyle B & Peet RK (2011). Veg-X - an exchange standard for plot-based vegetation data
#'
#' @family add functions
#'
#' @examples
#' # Load source data
#' data(mokihinui)
#'
#'
#' # Define mapping
#' mapping = list(plotName = "Plot", subPlotName = "Subplot", obsStartDate = "PlotObsStartDate",
#'                authorTaxonName = "PreferredSpeciesName", diameterMeasurement = "Diameter")
#'
#' # Define diameter measurement method
#' diamMeth = predefinedMeasurementMethod("DBH/cm")
#'
#'
#' # Create new Veg-X document with individual organism observations
#' x = addIndividualOrganismObservations(newVegX(), moki_dia, mapping = mapping,
#'                                       methods = c(diameterMeasurement = diamMeth))
#'
#' # Inspect the result
#' summary(x)
#' head(showElementTable(x, "individualOrganismObservation"))
#'
addIndividualOrganismObservations<-function(target, x, mapping,
                                            methods = list(),
                                            stratumDefinition = NULL,
                                            missing.values = c(NA, "0", ""),
                                            verbose = TRUE) {
  x = as.data.frame(x)
  nrecords = nrow(x)
  nmissing = 0

  indObservationMapping = c("plotName", "obsStartDate", "subPlotName", "stratumName", "authorTaxonName", "individual")

  #Check columns exist
  for(i in 1:length(mapping)) {
    if(!(mapping[i] %in% names(x))) stop(paste0("Variable '", mapping[i],"' not found in column names. Revise mapping or data."))
  }
  plotNames = as.character(x[[mapping[["plotName"]]]])
  obsStartDates = as.Date(as.character(x[[mapping[["obsStartDate"]]]]))
  authorTaxonNames = as.character(x[[mapping[["authorTaxonName"]]]])

  #Optional mappings
  taxonFlag = ("authorTaxonName" %in% names(mapping))
  if(taxonFlag) {
    authorTaxonNames = as.character(x[[mapping[["authorTaxonName"]]]])
  }
  stratumFlag = ("stratumName" %in% names(mapping))
  if(stratumFlag) {
    stratumNames = as.character(x[[mapping[["stratumName"]]]])
    if(is.null(stratumDefinition)) stop("Stratum definition must be supplied to map stratum observations.\n  Revise mapping or provide a stratum definition.")
  } else {
    if(!is.null(stratumDefinition)) stop("You need to include a mapping for 'stratumName' in order to map stratum observations.")
  }
  subPlotFlag = ("subPlotName" %in% names(mapping))
  if(subPlotFlag) {
    subPlotNames = as.character(x[[mapping[["subPlotName"]]]])
  }
  individualFlag = ("individual" %in% names(mapping))
  if(individualFlag) {
    individuals = as.Date(as.character(x[[mapping[["individual"]]]]))
  }


  indMeasurementValues = list()
  #diametermeasurement
  diameterMeasurementFlag = ("diameterMeasurement" %in% names(mapping))
  if(diameterMeasurementFlag) {
    if(!("diameterMeasurement" %in% names(methods))) stop("Method definition must be provided for 'diameterMeasurement'.")
    indMeasurementValues[["diameterMeasurement"]] = as.character(x[[mapping[["diameterMeasurement"]]]])
  }
  #heightmeasurement
  heightMeasurementFlag = ("heightMeasurement" %in% names(mapping))
  if(heightMeasurementFlag) {
    if(!("heightMeasurement" %in% names(methods))) stop("Method definition must be provided for 'heightMeasurement'.")
    indMeasurementValues[["heightMeasurement"]] = as.character(x[[mapping[["heightMeasurement"]]]])
  }

  indmesmapping = mapping[!(names(mapping) %in% c(indObservationMapping, "diameterMeasurement", "heightMeasurement"))]
  if(verbose) cat(paste0(" ", length(indmesmapping)," additional individual organism measurement variables found.\n"))
  if(length(indmesmapping)>0) {
    for(i in 1:length(indmesmapping)){
      if(!(names(indmesmapping)[[i]] %in% names(methods))) stop("Method definition must be provided for '",names(indmesmapping)[[i]],"'.")
      indMeasurementValues[[names(indmesmapping)[i]]] = as.character(x[[indmesmapping[[i]]]])
    }
  }


  #add methods
  methodIDs = character(0)
  methodCodes = list()
  methodAttIDs = list()
  for(m in names(methods)) {
    method = methods[[m]]
    nmtid = .newMethodIDByName(target,method@name)
    methodID = nmtid$id
    methodIDs[[m]] = methodID
    methodCodes[[m]] = character(0)
    methodAttIDs[[m]] = character(0)
    if(nmtid$new) {
      target@methods[[methodID]] = list(name = method@name,
                                        description = method@description,
                                        subject = method@subject,
                                        attributeType = method@attributeType)
      if(verbose) cat(paste0(" Measurement method '", method@name,"' added for '",m,"'.\n"))
      # add attributes if necessary
      methodAttIDs[[m]] = character(length(method@attributes))
      methodCodes[[m]] = character(length(method@attributes))
      for(i in 1:length(method@attributes)) {
        attid = .nextAttributeID(target)
        target@attributes[[attid]] = method@attributes[[i]]
        target@attributes[[attid]]$methodID = methodID
        methodAttIDs[[m]][i] = attid
        if(method@attributes[[i]]$type != "quantitative") methodCodes[[m]][i] = method@attributes[[i]]$code
      }
    } else {
      methodCodes[[m]] = .getAttributeCodesByMethodID(target,methodID)
      methodAttIDs[[m]] = .getAttributeIDsByMethodID(target,methodID)
      if(verbose) cat(paste0(" Measurement method '", method@name,"' for '",m,"' already included.\n"))
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
                                           subject = stratumDefMethod@subject,
                                           attributeType = stratumDefMethod@attributeType)
      if(verbose) cat(paste0(" Stratum definition method '", stratumDefMethod@name,"' added.\n"))
      # add attributes if necessary
      if(length(stratumDefMethod@attributes)>0) {
        for(i in 1:length(stratumDefMethod@attributes)) {
          attid = .nextAttributeID(target)
          target@attributes[[attid]] = stratumDefMethod@attributes[[i]]
          target@attributes[[attid]]$methodID = strmethodID
        }
      }
      # add strata (beware of new strata)
      orinstrata = length(target@strata)
      nstr = length(stratumDefinition@strata)
      stratumIDs = character(0)
      for(i in 1:nstr) {
        strid = .nextStratumID(target)
        stratumIDs[i] = strid
        target@strata[[strid]] = stratumDefinition@strata[[i]]
        target@strata[[strid]]$methodID = strmethodID
      }
      finnstrata = length(target@strata)
      if(verbose) {
        cat(paste0(" ", finnstrata-orinstrata, " new stratum definitions added.\n"))
      }
    } else { #Read stratum IDs and stratum names from selected method
      stratumIDs = .getStratumIDsByMethodID(target,strmethodID)
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
    if(!(plotNames[i] %in% missing.values)) {
      if(!(plotNames[i] %in% parsedPlots)) {
        npid = .newPlotIDByName(target, plotNames[i]) # Get the new plot ID (internal code)
        plotID = npid$id
        if(npid$new) target@plots[[plotID]] = list("plotName" = plotNames[i])
        parsedPlots = c(parsedPlots, plotNames[i])
        parsedPlotIDs = c(parsedPlotIDs, plotID)
      } else { #this access should be faster
        plotID = parsedPlotIDs[which(parsedPlots==plotNames[i])]
      }
    }
    #subplot (if defined)
    if(subPlotFlag){
      if(!(subPlotNames[i] %in% missing.values)) {
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
    if(!(as.character(obsStartDates[i]) %in% missing.values)) {
      pObsString = paste(plotID, obsStartDates[i]) # plotID+Date
      if(!(pObsString %in% parsedPlotObs)) {
        npoid = .newPlotObsIDByDate(target, plotID, obsStartDates[i]) # Get the new plot observation ID (internal code)
        plotObsID = npoid$id
        if(npoid$new) {
          target@plotObservations[[plotObsID]] = list("plotID" = plotID,
                                                      "obsStartDate" = obsStartDates[i])
        }
        parsedPlotObs = c(parsedPlotObs, pObsString)
        parsedPlotObsIDs = c(parsedPlotObsIDs, plotObsID)
      }
      else {
        plotObsID = parsedPlotIDs[which(parsedPlotObs==pObsString)]
      }
    }
    # taxon name
    if(taxonFlag) {
      if(!(authorTaxonNames[i] %in% missing.values)) {
        if(!(authorTaxonNames[i] %in% parsedTNUCs)) {
          ntnucid = .newTaxonNameUsageConceptIDByName(target, authorTaxonNames[i]) # Get the new taxon name usage ID (internal code)
          tnucID = ntnucid$id
          if(ntnucid$new) target@taxonNameUsageConcepts[[tnucID]] = list("authorTaxonName" = authorTaxonNames[i])
          parsedTNUCs = c(parsedTNUCs, authorTaxonNames[i])
          parsedTNUCIDs = c(parsedTNUCIDs, tnucID)
        }
        else {
          tnucID = parsedTNUCIDs[which(parsedTNUCs==authorTaxonNames[i])]
        }
      }
    }

    # strata
    if(stratumFlag) {
      if(!(stratumNames[i] %in% missing.values)) {
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
    }

    # individual organisms
    if(individualFlag) { # Allow for repeated observations on the same individuals
      if(!(individuals[i] %in% missing.values)) {
        if(!(individuals[i] %in% parsedInds)) {
          nindid = .newIndividualOrganismIDByIdentificationLabel(target, plotID, individuals[i]) # Get the new individual ID (internal code)
          indID = nindid$id
          if(nindid$new) target@individualOrganisms[[indID]] = list("plotID"= plotID,
                                                                    "identificationLabel" = individuals[i])
          parsedInds = c(parsedInds, individuals[i])
          parsedIndIDs = c(parsedIndIDs, indID)
        }
        else {
          indID = parsedIndIDs[which(parsedInds==individuals[i])]
        }
      }
      # else keep current individual
    }
    else { # Add a new individual for each individual observation record
      indID = .nextIndividualOrganismID(target)
      target@individualOrganisms[[indID]] = list("plotID"= plotID,
                                                 "identificationLabel" = paste0("ind",indID))
    }
    if(taxonFlag) target@individualOrganisms[[indID]]$taxonNameUsageConceptID = tnucID

    # ind org observations
    nioID = .newIndividualOrganismObservationIDByIndividualID(target, plotObsID, indID)
    indObsID = nioID$id
    if(nioID$new) {
      indObs = list("plotObservationID" = plotObsID,
                    "individualOrganismID" = indID)
    }
    else {
      indObs = target@individualObservations[[indObsID]]
    }
    if(stratumFlag) indObs$stratumObservationID = strObsID


    # diameter measurements
    for(m in c("diameterMeasurement")) {
      if(m %in% names(mapping)) {
        method = methods[[m]]
        attIDs = methodAttIDs[[m]]
        codes = methodCodes[[m]]
        value = as.character(indMeasurementValues[[m]][i])
        if(!(value %in% as.character(missing.values))) {
          if(method@attributeType== "quantitative") {
            value = as.numeric(value)
            if(value> method@attributes[[1]]$upperLimit) {
              stop(paste0("Diameter '", value,"' larger than upper limit of measurement definition. Please revise scale or data."))
            }
            else if(value < method@attributes[[1]]$lowerLimit) {
              stop(paste0("Diameter '", value,"' smaller than lower limit of measurement definition. Please revise scale or data."))
            }
            indObs[[m]] = list("attributeID" = attIDs[1], "value" = value)
          } else {
            ind = which(codes==as.character(value))
            if(length(ind)==1) {
              indObs[[m]] = list("attributeID" = attIDs[ind], "value" = value)
            }
            else stop(paste0("Value '", value,"' not found in diameter measurement definition. Please revise height classes or data."))
          }
        } else {
          nmissing = nmissing + 1
        }
      }
    }
    # height measurements
    for(m in c("heightMeasurement")) {
      if(m %in% names(mapping)) {
        method = methods[[m]]
        attIDs = methodAttIDs[[m]]
        codes = methodCodes[[m]]
        value = as.character(indMeasurementValues[[m]][i])
        if(!(value %in% as.character(missing.values))) {
          if(method@attributeType== "quantitative") {
            value = as.numeric(value)
            if(value> method@attributes[[1]]$upperLimit) {
              stop(paste0("Height '", value,"' larger than upper limit of measurement definition. Please revise scale or data."))
            }
            else if(value < method@attributes[[1]]$lowerLimit) {
              stop(paste0("Height '", value,"' smaller than lower limit of measurement definition. Please revise scale or data."))
            }
            indObs[[m]] = list("attributeID" = attIDs[1], "value" = value)
          } else {
            ind = which(codes==as.character(value))
            if(length(ind)==1) {
              indObs[[m]] = list("attributeID" = attIDs[ind], "value" = value)
            }
            else stop(paste0("Value '", value,"' not found in height measurement definition. Please revise height classes or data."))
          }
        } else {
          nmissing = nmissing + 1
        }
      }
    }
    # individual organism measurements
    for(m in names(indmesmapping)) {
      method = methods[[m]]
      attIDs = methodAttIDs[[m]]
      codes = methodCodes[[m]]
      value = as.character(indMeasurementValues[[m]][i])
      if(!(value %in% as.character(missing.values))) {
        if(!("individualOrganismMeasurements" %in% names(indObs))) indObs$individualOrganismMeasurements = list()
        mesID = as.character(length(indObs$individualOrganismMeasurements)+1)
        if(method@attributeType== "quantitative") {
          value = as.numeric(value)
          if(value> method@attributes[[1]]$upperLimit) {
            stop(paste0("Value '", value,"' larger than upper limit of measurement definition for '",m,"'. Please revise scale or data."))
          }
          else if(value < method@attributes[[1]]$lowerLimit) {
            stop(paste0("Value '", value,"' smaller than lower limit of measurement definition for '",m,"'. Please revise scale or data."))
          }
          indObs$individualOrganismMeasurements[[mesID]] = list("attributeID" = attIDs[1], "value" = value)
        } else {
          ind = which(codes==value)
          if(length(ind)==1) {
            indObs$individualOrganismMeasurements[[mesID]] = list("attributeID" = attIDs[ind], "value" = value)
          }
          else stop(paste0("Value '", value,"' not found in measurement definition for '",m,"'. Please revise height classes or data."))
        }
      }
      else {
        nmissing = nmissing + 1
      }
    }
    #Store value in target
    target@individualObservations[[indObsID]] = indObs

  }
  finnplots = length(target@plots)
  finnplotobs = length(target@plotObservations)
  finnstrobs = length(target@stratumObservations)
  finntuc = length(target@taxonNameUsageConcepts)
  finninds = length(target@individualOrganisms)
  finnindobs = length(target@individualObservations)
  if(verbose) {
    cat(paste0(" " , length(parsedPlots)," plot(s) parsed, ", finnplots-orinplots, " new added.\n"))
    cat(paste0(" " , length(parsedPlotObs)," plot observation(s) parsed, ", finnplotobs-orinplotobs, " new added.\n"))
    cat(paste0(" " , length(parsedTNUCs)," taxon name usage concept(s) parsed, ", finntuc-orintuc, " new added.\n"))
    if(stratumFlag) cat(paste0(" " , length(parsedStrObs)," stratum observation(s) parsed, ", finnstrobs-orinstrobs, " new added.\n"))
    cat(paste0(" " , length(parsedInds)," individual organism(s) parsed, ", finninds-orininds, " new added.\n"))
    cat(paste0(" ", nrecords," record(s) parsed, ", finnindobs-orinindobs, " new individual organism observation(s) added.\n"))
    if(nmissing>0) cat(paste0(" ", nmissing, " individual organism observation(s) with missing diameter value(s) not added.\n"))
  }

  return(target)
}
