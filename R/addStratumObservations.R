#' Add stratum observation records
#'
#' Adds stratum observation records to a VegX object from a data table
#'
#' @param target The initial object of class \code{\linkS4class{VegX}} to be modified
#' @param x A data frame where each row corresponds to one aggregate taxon observation. Columns can be varied.
#' @param projectTitle A character string to identify the project title, which can be the same as one of the currently defined in \code{target}.
#' @param mapping A list with element names 'plotName', 'obsStartDate', and 'stratumName' used to specify the mapping of data columns (specified using strings for column names) onto these variables.
#' Additional optional mappings are: 'obsEndDate', 'subPlotName', 'lowerLimitMeasurement' and 'lowerLimitMeasurement'.
#' @param methods A list measurement methods for stratum measurements (an object of class \code{\linkS4class{VegXMethod}}).
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
#' mapping = list(plotName = "Plot", obsStartDate = "obsDate", stratumName = "Tier",
#'                lowerLimitMeasurement = "TierLower", upperLimitMeasurement = "TierUpper",
#'                cover = "CoverClass")
#'
#'
#' # Define cover scale
#' coverscale = definePlantCoverScale(name = "Recce cover scale", description = "Recce recording method by Allen",
#'                          citation = "Hurst, JM and Allen, RB. (2007) The Recce method for describing New Zealand vegetation – Field protocols. Landcare Research, Lincoln.",
#'                          breaks = c(0, 0.1, 1, 5, 25, 50, 75, 100),
#'                          midPoints = c(0.01, 0.05, 0.5, 15, 37.5, 62.5, 87.5),
#'                          values = c("P","1","2","3", "4", "5", "6"))
#'
#' # Define height measurement methods
#' heightMethod = predefinedMeasurementMethod("Stratum height")
#'
#' # Define strata
#' strataDef = defineCategoricalStrata(name = "Category strata",
#'                                  description = "Strata with boundaries undefined",
#'                                  strataNames = paste0("Tier ",1:7))
#'
#' # Mapping process
#' x = addStratumObservations(target, tier, "Mokihinui",
#'                         mapping = mapping,
#'                         methods = list(lowerLimitMeasurement = heightMethod,
#'                                        upperLimitMeasurement = heightMethod,
#'                                        cover=coverscale),
#'                         stratumDefinition = strataDef)
#'
#' summary(x)
#'
addStratumObservations<-function(target, x, projectTitle,
                                 mapping,
                                 methods,
                                 stratumDefinition,
                                 missing.values = c(NA, ""),
                                 verbose = TRUE) {

  if(is.null(stratumDefinition)) stop("Stratum definition must be supplied to map stratum observations.")

  x = as.data.frame(x)
  nrecords = nrow(x)
  nmissing = 0

  stratumObservationMappingsAvailable = c("plotName", "obsStartDate", "obsEndDate", "subPlotName", "stratumName")

  #Check columns exist
  for(i in 1:length(mapping)) {
    if(!(mapping[i] %in% names(x))) stop(paste0("Variable '", mapping[i],"' not found in column names. Revise mapping or data."))
  }
  plotNames = as.character(x[[mapping[["plotName"]]]])
  obsStartDates = as.Date(as.character(x[[mapping[["obsStartDate"]]]]))
  stratumNames = as.character(x[[mapping[["stratumName"]]]])

  #Optional mappings
  obsEndFlag = ("obsEndDate" %in% names(mapping))
  if(obsEndFlag) {
    obsEndDates = as.Date(as.character(x[[mapping[["obsEndDate"]]]]))
  }
  subPlotFlag = ("subPlotName" %in% names(mapping))
  if(subPlotFlag) {
    subPlotNames = as.character(x[[mapping[["subPlotName"]]]])
  }


  #stratummeasurement
  stratumMeasurementValues = list()
  lowerLimitMeasurementFlag = ("lowerLimitMeasurement" %in% names(mapping))
  if(lowerLimitMeasurementFlag) {
    if(!("lowerLimitMeasurement" %in% names(methods))) stop("Method definition must be provided for 'lowerLimitMeasurement'.")
    stratumMeasurementValues[["lowerLimitMeasurement"]] = as.character(x[[mapping[["lowerLimitMeasurement"]]]])
  }
  upperLimitMeasurementFlag = ("upperLimitMeasurement" %in% names(mapping))
  if(upperLimitMeasurementFlag) {
    if(!("upperLimitMeasurement" %in% names(methods))) stop("Method definition must be provided for 'upperLimitMeasurement'.")
    stratumMeasurementValues[["upperLimitMeasurement"]] = as.character(x[[mapping[["upperLimitMeasurement"]]]])
  }

  strmesmapping = mapping[!(names(mapping) %in% c("plotName", "subPlotName", "lowerLimitMeasurement", "upperLimitMeasurement","obsStartDate","obsEndDate", "stratumName"))]
  if(verbose) cat(paste0(" ", length(strmesmapping)," stratum measurement variables found.\n"))
  if(length(strmesmapping)>0) {
    for(i in 1:length(strmesmapping)){
      if(!(names(strmesmapping)[[i]] %in% names(methods))) stop("Method definition must be provided for '",names(strmesmapping)[[i]],"'.")
      stratumMeasurementValues[[names(strmesmapping)[i]]] = as.character(x[[strmesmapping[[i]]]])
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
      cnt = length(target@attributes)+1
      methodAttIDs[[m]] = character(length(method@attributes))
      methodCodes[[m]] = character(length(method@attributes))
      for(i in 1:length(method@attributes)) {
        attid = as.character(length(target@attributes)+1)
        target@attributes[[attid]] = method@attributes[[i]]
        target@attributes[[attid]]$methodID = methodID
        methodAttIDs[[m]][i] = attid
        if(method@attributes[[i]]$type != "quantitative") methodCodes[[m]][i] = method@attributes[[i]]$code
        cnt = cnt + 1
      }
    } else {
      methodCodes[[m]] = .getAttributeCodesByMethodID(target,methodID)
      methodAttIDs[[m]] = .getAttributeIDsByMethodID(target,methodID)
      if(verbose) cat(paste0(" Measurement method '", method@name,"' for '",m,"' already included.\n"))
    }
  }


  # stratum definition
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
      cnt = length(target@attributes)+1
      for(i in 1:length(stratumDefMethod@attributes)) {
        attid = as.character(length(target@attributes)+1)
        target@attributes[[attid]] = stratumDefMethod@attributes[[i]]
        target@attributes[[attid]]$methodID = strmethodID
        cnt = cnt + 1
      }
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
  }
  else { #Read stratum IDs and stratum names from selected method
    if(verbose) cat(paste0(" Stratum definition '", stratumDefMethod@name,"' already included.\n"))
    stratumIDs = .getStratumIDsByMethodID(target,strmethodID)
  }

  orinplots = length(target@plots)
  orinplotobs = length(target@plotObservations)
  orinstrobs = length(target@stratumObservations)
  parsedPlots = character(0)
  parsedPlotIDs = character(0)
  parsedPlotObs = character(0)
  parsedPlotObsIDs = character(0)
  parsedStrObs = character(0)
  parsedStrObsIDs = character(0)
  strObsCounter = orinstrobs+1 #counter
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

    # stratum observations
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
    strObs = target@stratumObservations[[strObsID]]

    # height limit measurements
    for(m in c("lowerLimitMeasurement","upperLimitMeasurement")) {
      method = methods[[m]]
      attIDs = methodAttIDs[[m]]
      codes = methodCodes[[m]]
      value = as.numeric(stratumMeasurementValues[[m]][i])
      if(!(value %in% as.character(missing.values))) {
        if(method@attributeType== "quantitative") {
          if(value> method@attributes[[1]]$upperLimit) {
            stop(paste0("Height '", value,"' larger than upper limit of measurement definition. Please revise scale or data."))
          }
          else if(value < method@attributes[[1]]$lowerLimit) {
            stop(paste0("Height '", value,"' smaller than lower limit of measurement definition. Please revise scale or data."))
          }
          strObs[[m]] = list("attributeID" = attIDs[1], "value" = value)
        } else {
          ind = which(codes==as.character(value))
          if(length(ind)==1) {
            strObs[[m]] = list("attributeID" = attIDs[ind], "value" = value)
          }
          else stop(paste0("Value '", value,"' not found in height measurement definition. Please revise height classes or data."))
        }
      } else {
        nmissing = nmissing + 1
      }
    }
    # stratum measurements
    for(m in names(strmesmapping)) {
      method = methods[[m]]
      attIDs = methodAttIDs[[m]]
      codes = methodCodes[[m]]
      value = as.numeric(stratumMeasurementValues[[m]][i])
      if(!(value %in% as.character(missing.values))) {
        if(!("stratumMeasurements" %in% names(strObs))) strObs$stratumMeasurements = list()
        mesID = as.character(length(strObs$stratumMeasurements)+1)
        if(method@attributeType== "quantitative") {
          if(value> method@attributes[[1]]$upperLimit) {
            stop(paste0("Value '", value,"' larger than upper limit of measurement definition for '",m,"'. Please revise scale or data."))
          }
          else if(value < method@attributes[[1]]$lowerLimit) {
            stop(paste0("Value '", value,"' smaller than lower limit of measurement definition for '",m,"'. Please revise scale or data."))
          }
          strObs$stratumMeasurements[[mesID]] = list("attributeID" = attIDs[1], "value" = value)
        } else {
          ind = which(codes==as.character(value))
          if(length(ind)==1) {
            strObs$stratumMeasurements[[mesID]] = list("attributeID" = attIDs[ind], "value" = value)
          }
          else stop(paste0("Value '", value,"' not found in measurement definition for '",m,"'. Please revise height classes or data."))
        }
      }
      else {
        nmissing = nmissing + 1
      }
    }
    #Store value in target
    target@stratumObservations[[strObsID]] = strObs
  }
  finnplots = length(target@plots)
  finnplotobs = length(target@plotObservations)
  finnstrobs = length(target@stratumObservations)
  if(verbose) {
    cat(paste0(" " , length(parsedPlots)," plot(s) parsed, ", finnplots-orinplots, " new added.\n"))
    cat(paste0(" " , length(parsedPlotObs)," plot observation(s) parsed, ", finnplotobs-orinplotobs, " new added.\n"))
    cat(paste0(" ", nrecords," record(s) parsed, ", finnstrobs-orinstrobs, " new stratum observation(s) added.\n"))
    if(nmissing>0) cat(paste0(" ", nmissing, " measurement(s) with missing value(s) not added.\n"))
  }


  return(target)
}
