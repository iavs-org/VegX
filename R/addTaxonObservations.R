#' Add taxon observation records
#'
#' Adds aggregate taxon observation records to a VegX object from a data table
#' using a mapping to identify columns: plot, observation date, stratum, taxon name and value.
#'
#' @param target The initial object of class \code{\linkS4class{VegX}} to be modified
#' @param x A data frame where each row corresponds to one aggregate taxon observation. Columns can be varied.
#' @param projectTitle A character string to identify the project title, which can be the same as one of the currently defined in \code{target}.
#' @param mapping A list with element names 'plotName', 'obsStartDate', 'taxonAuthorName' and 'value', used to specify the mapping of data columns (specified using strings for column names) onto these variables.
#'                Additional optional mappings are: 'subPlotName', 'obsEndDate' and 'stratumName'.
#' @param abundanceMethod Measurement method for aggregate plant abundance (an object of class \code{\linkS4class{VegXMethod}}).
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
#' mapping = list(plotName = "Plot", obsStartDate = "obsDate", taxonAuthorName = "PreferredSpeciesName",
#'               stratumName = "Tier", value = "Category")
#'
#' # Define abundance scale
#' scale = definePlantCoverScale(name = "Recce cover scale", description = "Recce recording method by Allen",
#'                          citation = "Hurst, JM and Allen, RB. (2007) The Recce method for describing New Zealand vegetation – Field protocols. Landcare Research, Lincoln.",
#'                          breaks = c(0, 0.1, 1, 5, 25, 50, 75, 100),
#'                          midPoints = c(0.01, 0.05, 0.5, 15, 37.5, 62.5, 87.5),
#'                          values = c("P","1","2","3", "4", "5", "6"))
#'
#' # Define strata
#' strataDef = defineMixedStrata(name = "Recce strata",
#'                               description = "Standard Recce stratum definition",
#'                               citation = "Hurst, JM and Allen, RB. (2007) The Recce method for describing New Zealand vegetation – Field protocols. Landcare Research, Lincoln.",
#'                               heightStrataBreaks = c(0, 0.3,2.0,5, 12, 25, 50),
#'                               heightStrataNames = paste0("Tier ",1:6),
#'                               categoryStrataNames = "Tier 7")
#'
#' # Mapping process
#' x = addTaxonObservations(target, tcv, "Mokihinui",
#'                         mapping = mapping,
#'                         abundanceMethod = scale,
#'                         stratumDefinition = strataDef)
#'
#' summary(x)
#'
addTaxonObservations<-function(target, x, projectTitle,
                                     mapping,
                                     abundanceMethod,
                                     stratumDefinition = NULL,
                                     missing.values = c(NA, "0", ""),
                                     verbose = TRUE) {

  x = as.data.frame(x)
  nrecords = nrow(x)
  nmissing = 0

  #Check columns exist
  for(i in 1:length(mapping)) {
    if(!(mapping[i] %in% names(x))) stop(paste0("Variable '", mapping[i],"' not found in column names. Revise mapping or data."))
  }
  plotNames = as.character(x[[mapping[["plotName"]]]])
  obsStartDates = as.Date(as.character(x[[mapping[["obsStartDate"]]]]))
  taxonAuthorNames = as.character(x[[mapping[["taxonAuthorName"]]]])
  values = as.character(x[[mapping[["value"]]]])

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
  nmtid = .newMethodIDByName(target,abundanceMethod@name)
  methodID = nmtid$id
  abundanceCodes = character(0)
  attIDs = character(0)
  if(nmtid$new) {
    target@methods[[methodID]] = list(name = abundanceMethod@name,
                                      description = abundanceMethod@description,
                                      subject = abundanceMethod@subject,
                                      attributeType = abundanceMethod@attributeType)
    if(verbose) cat(paste0(" Abundance measurement method '", abundanceMethod@name,"' added.\n"))
    # add attributes if necessary
    cnt = length(target@attributes)+1
    for(i in 1:length(abundanceMethod@attributes)) {
      attid = as.character(length(target@attributes)+1)
      target@attributes[[attid]] = abundanceMethod@attributes[[i]]
      target@attributes[[attid]]$methodID = methodID
      attIDs[i] = attid
      if(abundanceMethod@attributes[[i]]$type != "quantitative") abundanceCodes[i] = abundanceMethod@attributes[[i]]$code
      cnt = cnt + 1
    }
  } else {
    abundanceCodes = .getAttributeCodesByMethodID(methodID)
    attIDs = .getAttributeIDsByMethodID(methodID)
    if(verbose) cat(paste0(" Abundance measurement method '", abundanceMethod@name,"' already included.\n"))
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
      cnt = length(target@attributes)+1
      for(i in 1:length(stratumDefMethod@attributes)) {
        attid = as.character(length(target@attributes)+1)
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
    if(!(taxonAuthorNames[i] %in% parsedTNUCs)) {
      ntnucid = .newTaxonNameUsageConceptIDByName(target, taxonAuthorNames[i]) # Get the new taxon name usage ID (internal code)
      tnucID = ntnucid$id
      if(ntnucid$new) target@taxonNameUsageConcepts[[tnucID]] = list("authorName" = taxonAuthorNames[i])
      parsedTNUCs = c(parsedTNUCs, taxonAuthorNames[i])
      parsedTNUCIDs = c(parsedTNUCIDs, tnucID)
    } else {
      tnucID = parsedTNUCIDs[which(parsedTNUCs==taxonAuthorNames[i])]
    }

    # stratum observations
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
    # TO BE DONE: CHECK that the agg org observation is new
    if(!(values[i] %in% as.character(missing.values))) {
      if(abundanceMethod@attributeType== "quantitative") {
        if(values[i]> abundanceMethod@attributes[[1]]$upperLimit) {
          stop(paste0("Value '", values[i],"' larger than upper limit of measurement definition. Please revise scale or data."))
        }
        else if(values[i] < abundanceMethod@attributes[[1]]$lowerLimit) {
          stop(paste0("Value '", values[i],"' smaller than lower limit of measurement definition. Please revise scale or data."))
        }
        target@aggregateObservations[[as.character(aggObsCounter)]] = list("plotObservationID" = plotObsID,
                                                                            "taxonNameUsageConceptID" = tnucID,
                                                                            "attributeID" = attID[1],
                                                                            "stratumObservationID" = "",
                                                                            "value" = values[i])
        if(stratumFlag) target@aggregateObservations[[as.character(aggObsCounter)]]$stratumObservationID = strObsID
        aggObsCounter = aggObsCounter + 1
      } else {
        ind = which(abundanceCodes==as.character(values[i]))
        if(length(ind)==1) {
          target@aggregateObservations[[as.character(aggObsCounter)]] = list("plotObservationID" = plotObsID,
                                                                              "taxonNameUsageConceptID" = tnucID,
                                                                              "attributeID" = attIDs[ind],
                                                                              "stratumObservationID" = "",
                                                                              "value" = values[i])
          if(stratumFlag) target@aggregateObservations[[as.character(aggObsCounter)]]$stratumObservationID = strObsID
          aggObsCounter = aggObsCounter + 1
        }
        else stop(paste0("Value '", values[i],"' not found in measurement definition. Please revise scale or data."))
      }
    } else {
      nmissing = nmissing + 1
    }
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
