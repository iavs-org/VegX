#' Add a taxon observation records
#'
#' Adds aggregated taxon observation records to a VegX object from a data table
#' using a mapping to identify columns: plot, observation date, stratum, taxon name and value.
#'
#' @param target The original object of class \code{\linkS4class{VegX}} to be modified
#' @param x A data frame where each row corresponds to one aggregated taxon observation. Columns can be varied.
#' @param projectTitle A character string to identify the project title, which can be the same as one of the currently defined in \code{target}.
#' @param mapping A list with element names 'plotName', 'obsStartDate', 'taxonAuthorName' and 'value', used to specify the mapping of data columns (specified using strings for column names) onto these variables.
#'                Additional optional mappings are: 'subPlot', 'obsEndDate' and 'stratumName'.
#' @param abundanceMethod Measurement method for aggregated plant abundance (an object of class \code{\linkS4class{VegXMethod}}).
#' @param stratumDefinition An object of class \code{\linkS4class{VegXStrata}} indicating the definition of strata.
#' @param verbose A boolean flag to indicate console output of the data integration process.
#'
#' @return The modified object of class \code{\linkS4class{VegX}}
#' @export
#'
#' @examples
addTaxonObservationRecords<-function(target, x, projectTitle,
                                     mapping,
                                     abundanceMethod = defaultPercentCoverMethod(),
                                     stratumDefinition = stratumDefinitionByHeight(),
                                     verbose = TRUE) {

  x = as.data.frame(x)
  nrecords = nrow(x)

  plotNames = as.character(x[[mapping[["plotName"]]]])
  obsStartDates = as.Date(x[[mapping[["obsStartDate"]]]])
  taxonAuthorNames = as.character(x[[mapping[["taxonAuthorName"]]]])
  values = as.character(x[[mapping[["value"]]]])

  stratumFlag = ("stratumName" %in% names(mapping))
  if(stratumFlag) {
    stratumNames = as.character(x[[mapping[["stratumName"]]]])
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
  if(nmtid$new) {
    target@methods[[methodID]] = list(name = abundanceMethod@name,
                                      description = abundanceMethod@description,
                                      attributeClass = abundanceMethod@attributeClass,
                                      attributeType = abundanceMethod@attributeType)
    if(verbose) cat(paste0(" Abundance measurement method '", abundanceMethod@name,"' added.\n"))
    # add attributes if necessary
    cnt = length(target@attributes)+1
    for(i in 1:length(abundanceMethod@attributes)) {
      attid = as.character(cnt)
      target@attributes[[attid]] = abundanceMethod@attributes[[i]]
      target@attributes[[attid]]$methodID = methodID
      cnt = cnt + 1
    }
  }

  if(abundanceMethod@attributeType!= "quantitative") {
    nattr = length(abundanceMethod@attributes)
    abundanceCodes = character(nattr)
    abundanceIDs = names(abundanceMethod@attributes)
    for(i in 1:nattr) abundanceCodes[i] = as.character(abundanceMethod@attributes[[i]]$code)
  }

  # stratum definition
  if(!is.null(stratumDefinition)) {
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
    }
  }

  orinplots = length(target@plots)
  orinplotobs = length(target@plotObservations)
  orinstrobs = length(target@stratumObservations)
  parsedPlots = character(0)
  parsedPlotObs = character(0)
  parsedStrObs = character(0)
  #Record parsing loop
  for(i in 1:nrecords) {
    #plot
    if(!(plotNames[i] %in% parsedPlots)) {
      npid = .newPlotIDByName(target, plotNames[i]) # Get the new plot ID (internal code)
      if(npid$new) target@plots[[npid$id]] = list("plotName" = plotNames[i])
      parsedPlots = c(parsedPlots, plotNames[i])
    }
    pObsString = paste(npid$id, obsStartDates[i]) # plotID+Date
    if(!(pObsString %in% parsedPlotObs)) {
      npoid = .newPlotObsIDByDate(target, npid$id, obsStartDates[i]) # Get the new plot observation ID (internal code)
      if(npoid$new) target@plotObservations[[npoid$id]] = list("plotID" = npid$id,
                                                    "obsStartDate" = obsStartDates[i],
                                                    "projectID" = projectID)
      parsedPlotObs = c(parsedPlotObs, pObsString)
    }

    if(stratumFlag) {
      strID = .getStratumIDByName(target, stratumNames[i])
      if(is.null(strID)) stop(paste0(stratumNames[i]," not found within stratum names. Revise stratum definition or data."))
      strObsString = paste(npoid$id, strID) # plotObsID+stratumID
      if(!(strObsString %in% parsedStrObs)) {
        nstroid = .newStratumObsIDByIDs(target, npoid$id, strID) # Get the new stratum observation ID (internal code)
        if(nstroid$new) target@stratumObservations[[nstroid$id]] = list("plotObservationID" = npoid$id,
                                                                        "stratumID" = strID)
        parsedStrObs = c(parsedStrObs, strObsString)
      }
    }
  }
  finnplots = length(target@plots)
  finnplotobs = length(target@plotObservations)
  finnstrobs = length(target@stratumObservations)
  if(verbose) {
    cat(paste0(" ", finnplots-orinplots, " new plots added.\n"))
    cat(paste0(" ", finnplotobs-orinplotobs, " new plot observations added.\n"))
    if(stratumFlag) cat(paste0(" ", finnstrobs-orinstrobs, " new stratum observations added.\n"))
  }


  return(target)
}
