#' Add aggregate organism observation records
#'
#' Adds aggregate organism observation records to a VegX object from a data table
#'
#' @param target The initial object of class \code{\linkS4class{VegX}} to be modified
#' @param x A data frame where each row corresponds to one aggregate organism observation. Columns can be varied.
#' @param projectTitle A character string to identify the project title, which can be the same as one of the currently defined in \code{target}.
#' @param mapping A list with element names 'plotName', 'obsStartDate', 'authorTaxonName' and 'value', used to specify the mapping of data columns (specified using strings for column names) onto these variables.
#'                Additional optional mappings are: 'subPlotName', 'obsEndDate', 'stratumName', 'heightMeasurement' and mappings to other measurements (e.g. taxon abundance).
#' @param methods A list measurement methods for aggregated organism measurements (an object of class \code{\linkS4class{VegXMethod}}).
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
#' data(mokihinui)
#'
#' # Create new Veg-X document
#' target = newVegX()
#'
#' # Define mapping
#' mapping = list(plotName = "Plot", obsStartDate = "obsDate", authorTaxonName = "PreferredSpeciesName",
#'               stratumName = "Tier", cover = "Category")
#'
#' # Define abundance scale
#' coverscale = defineOrdinalScaleMethod(name = "Recce cover scale",
#'                    description = "Recce recording method by Hurst/Allen",
#'                    subject = "plant cover",
#'                    citation = "Hurst, JM and Allen, RB. (2007) The Recce method for describing New Zealand vegetation – Field protocols. Landcare Research, Lincoln.",
#'                    codes = c("P","1","2","3", "4", "5", "6"),
#'                    quantifiableCodes = c("1","2","3", "4", "5", "6"),
#'                    breaks = c(0, 1, 5, 25, 50, 75, 100),
#'                    midPoints = c(0.05, 0.5, 15, 37.5, 62.5, 87.5),
#'                    definitions = c("Presence", "<1%", "1-5%","6-25%", "26-50%", "51-75%", "76-100%"))
#'
#' # Define strata
#' strataDef = defineMixedStrata(name = "Recce strata",
#'                               description = "Standard Recce stratum definition",
#'                               citation = "Hurst, JM and Allen, RB. (2007) The Recce method for describing New Zealand vegetation – Field protocols. Landcare Research, Lincoln.",
#'                               heightStrataBreaks = c(0, 0.3,2.0,5, 12, 25, 50),
#'                               heightStrataNames = paste0("Tier ",1:6),
#'                               categoryStrataNames = "Tier 7",
#'                               categoryStrataDefinition = "Epiphytes")
#'
#' # Mapping process
#' x = addAggregateOrganismObservations(target, tcv, "Mokihinui",
#'                         mapping = mapping,
#'                         methods = c(cover=coverscale),
#'                         stratumDefinition = strataDef)
#'
#' summary(x)
#'
addAggregateOrganismObservations<-function(target, x, projectTitle,
                                     mapping,
                                     methods = list(),
                                     stratumDefinition = NULL,
                                     missing.values = c(NA, "0", ""),
                                     verbose = TRUE) {

  x = as.data.frame(x)
  nrecords = nrow(x)
  nmissing = 0

  aggregatedObservationMappingsAvailable = c("plotName", "obsStartDate", "obsEndDate", "subPlotName", "stratumName", "authorTaxonName")

  #Check columns exist
  for(i in 1:length(mapping)) {
    if(!(mapping[i] %in% names(x))) stop(paste0("Variable '", mapping[i],"' not found in column names. Revise mapping or data."))
  }
  plotNames = as.character(x[[mapping[["plotName"]]]])
  obsStartDates = as.Date(as.character(x[[mapping[["obsStartDate"]]]]))
  authorTaxonNames = as.character(x[[mapping[["authorTaxonName"]]]])

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
    obsEndDates = as.Date(as.character(x[[mapping[["obsEndDate"]]]]))
  }
  subPlotFlag = ("subPlotName" %in% names(mapping))
  if(subPlotFlag) {
    subPlotNames = as.character(x[[mapping[["subPlotName"]]]])
  }

  #heightmeasurement
  aggMeasurementValues = list()
  heightMeasurementFlag = ("heightMeasurement" %in% names(mapping))
  if(heightMeasurementFlag) {
    if(!("heightMeasurement" %in% names(methods))) stop("Method definition must be provided for 'heightMeasurement'.")
    aggMeasurementValues[["heightMeasurement"]] = as.character(x[[mapping[["heightMeasurement"]]]])
  }

  aggmesmapping = mapping[!(names(mapping) %in% c(aggregatedObservationMappingsAvailable, "heightMeasurement"))]
  if(verbose) cat(paste0(" ", length(aggmesmapping)," additional aggregated organism measurement variables found.\n"))
  if(length(aggmesmapping)>0) {
    for(i in 1:length(aggmesmapping)){
      if(!(names(aggmesmapping)[[i]] %in% names(methods))) stop("Method definition must be provided for '",names(aggmesmapping)[[i]],"'.")
      aggMeasurementValues[[names(aggmesmapping)[i]]] = as.character(x[[aggmesmapping[[i]]]])
    }
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
      # cnt = length(target@strata)+1
      for(i in 1:nstr) {
        # strid = as.character(cnt)
        strid = .nextStratumID(target)
        stratumIDs[i] = strid
        target@strata[[strid]] = stratumDefinition@strata[[i]]
        target@strata[[strid]]$methodID = strmethodID
        # cnt = cnt + 1
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
  orinaggobs = length(target@aggregateObservations)
  parsedPlots = character(0)
  parsedPlotIDs = character(0)
  parsedPlotObs = character(0)
  parsedPlotObsIDs = character(0)
  parsedTNUCs = character(0)
  parsedTNUCIDs = character(0)
  parsedStrObs = character(0)
  parsedStrObsIDs = character(0)
  aggObsCounter = orinaggobs+1 #counter
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
    if(!(authorTaxonNames[i] %in% parsedTNUCs)) {
      ntnucid = .newTaxonNameUsageConceptIDByName(target, authorTaxonNames[i]) # Get the new taxon name usage ID (internal code)
      tnucID = ntnucid$id
      if(ntnucid$new) target@taxonNameUsageConcepts[[tnucID]] = list("authorTaxonName" = authorTaxonNames[i])
      parsedTNUCs = c(parsedTNUCs, authorTaxonNames[i])
      parsedTNUCIDs = c(parsedTNUCIDs, tnucID)
    } else {
      tnucID = parsedTNUCIDs[which(parsedTNUCs==authorTaxonNames[i])]
    }

    # stratum observations
    strObsID = NULL
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
        strObsID = parsedStrObsIDs[which(parsedStrObs==strObsString)]
      }
    }

    # agg org observations
    naoID = .newAggregateOrganismObservationIDByTaxonID(target, plotObsID, strObsID, tnucID)
    aggObsID = naoID$id
    if(naoID$new) {
      aggObs = list("plotObservationID" = plotObsID,
                    "taxonNameUsageConceptID" = tnucID,
                    "stratumObservationID" = "")
      if(stratumFlag) aggObs$stratumObservationID = strObsID
    }
    else {
      aggObs = target@stratumObservations[[aggObsID]]
    }

    # height limit measurements
    for(m in c("heightMeasurement")) {
      if(m %in% names(mapping)) {
        method = methods[[m]]
        attIDs = methodAttIDs[[m]]
        codes = methodCodes[[m]]
        value = as.character(aggMeasurementValues[[m]][i])
        if(!(value %in% as.character(missing.values))) {
          if(method@attributeType== "quantitative") {
            value = as.numeric(value)
            if(value> method@attributes[[1]]$upperLimit) {
              stop(paste0("Height '", value,"' larger than upper limit of measurement definition. Please revise scale or data."))
            }
            else if(value < method@attributes[[1]]$lowerLimit) {
              stop(paste0("Height '", value,"' smaller than lower limit of measurement definition. Please revise scale or data."))
            }
            aggObs[[m]] = list("attributeID" = attIDs[1], "value" = value)
          } else {
            ind = which(codes==as.character(value))
            if(length(ind)==1) {
              aggObs[[m]] = list("attributeID" = attIDs[ind], "value" = value)
            }
            else stop(paste0("Value '", value,"' not found in height measurement definition. Please revise height classes or data."))
          }
        } else {
          nmissing = nmissing + 1
        }
      }
    }
    # stratum measurements
    for(m in names(aggmesmapping)) {
      method = methods[[m]]
      attIDs = methodAttIDs[[m]]
      codes = methodCodes[[m]]
      value = as.character(aggMeasurementValues[[m]][i])
      if(!(value %in% as.character(missing.values))) {
        if(!("aggregateOrganismMeasurements" %in% names(aggObs))) aggObs$aggregateOrganismMeasurements = list()
        mesID = as.character(length(aggObs$aggregateOrganismMeasurements)+1)
        if(method@attributeType== "quantitative") {
          value = as.numeric(value)
          if(value> method@attributes[[1]]$upperLimit) {
            stop(paste0("Value '", value,"' larger than upper limit of measurement definition for '",m,"'. Please revise scale or data."))
          }
          else if(value < method@attributes[[1]]$lowerLimit) {
            stop(paste0("Value '", value,"' smaller than lower limit of measurement definition for '",m,"'. Please revise scale or data."))
          }
          aggObs$aggregateOrganismMeasurements[[mesID]] = list("attributeID" = attIDs[1], "value" = value)
        } else {
          ind = which(codes==value)
          if(length(ind)==1) {
            aggObs$aggregateOrganismMeasurements[[mesID]] = list("attributeID" = attIDs[ind], "value" = value)
          }
          else stop(paste0("Value '", value,"' not found in measurement definition for '",m,"'. Please revise height classes or data."))
        }
      }
      else {
        nmissing = nmissing + 1
      }
    }
    #Store value in target
    target@aggregateObservations[[aggObsID]] = aggObs

  }
  finnplots = length(target@plots)
  finnplotobs = length(target@plotObservations)
  finnstrobs = length(target@stratumObservations)
  finntuc = length(target@taxonNameUsageConcepts)
  finnaggobs = length(target@aggregateObservations)
  if(verbose) {
    cat(paste0(" " , length(parsedPlots)," plot(s) parsed, ", finnplots-orinplots, " new added.\n"))
    cat(paste0(" " , length(parsedPlotObs)," plot observation(s) parsed, ", finnplotobs-orinplotobs, " new added.\n"))
    cat(paste0(" " , length(parsedTNUCs)," taxon name usage concept(s) parsed, ", finntuc-orintuc, " new added.\n"))
    if(stratumFlag) cat(paste0(" " , length(parsedStrObs)," stratum observation(s) parsed, ", finnstrobs-orinstrobs, " new added.\n"))
    cat(paste0(" ", nrecords," record(s) parsed, ", finnaggobs-orinaggobs, " new aggregate organism observation(s) added.\n"))
    if(nmissing>0) cat(paste0(" ", nmissing, " aggregate organism observation(s) with missing abundance value(s) not added.\n"))
  }


  return(target)
}
