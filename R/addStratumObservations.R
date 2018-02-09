#' Add stratum observation records
#'
#' Adds stratum observation records to a VegX object from a data table where rows are stratum observations.
#'
#' @param target The initial object of class \code{\linkS4class{VegX}} to be modified
#' @param x A data frame where each row corresponds to one stratum observation. Columns can be varied.
#' @param mapping A named list with element names 'plotName', 'obsStartDate', and 'stratumName' used to specify the mapping of data columns (specified using strings for column names) onto these variables.
#' Additional optional mappings are: 'subPlotName', 'lowerLimitMeasurement', 'lowerLimitMeasurement', and mappings to other stratum measurements.
#' @param methods A list measurement methods for stratum measurements (an object of class \code{\linkS4class{VegXMethodDefinition}}).
#' @param stratumDefinition An object of class \code{\linkS4class{VegXStrataDefinition}} indicating the definition of strata.
#' @param date.format A character string specifying the input format of dates (see \code{\link{as.Date}}).
#' @param missing.values A character vector of values that should be considered as missing observations/measurements.
#' @param verbose A boolean flag to indicate console output of the data integration process.
#'
#' @return The modified object of class \code{\linkS4class{VegX}}.
#' @export
#'
#' @details Missing value policy:
#'  \itemize{
#'   \item{Missing 'plotName', 'obsStartDate' or 'stratumName' values are interpreted as if the previous non-missing value has to be used to define plot observation.}
#'   \item{Missing 'subPlotName' values are interpreted in that observation refers to the parent plotName.}
#'   \item{Missing measurements are simply not added to the Veg-X document.}
#'  }
#'
#' @references Wiser SK, Spencer N, De Caceres M, Kleikamp M, Boyle B & Peet RK (2011). Veg-X - an exchange standard for plot-based vegetation data
#'
#' @family add functions
#'
#' @examples
#' # Load source data
#' data(mokihinui)
#'
#' # Define mapping
#' mapping = list(plotName = "Plot", obsStartDate = "PlotObsStartDate", stratumName = "Tier",
#'                lowerLimitMeasurement = "TierLower", upperLimitMeasurement = "TierUpper",
#'                cover = "CoverClass")
#'
#'
#' # Define cover scale
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
#' # Define height measurement methods
#' heightMethod = predefinedMeasurementMethod("Stratum height/m")
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
#' # Create new Veg-X document with stratum observations
#' x = addStratumObservations(newVegX(), moki_str, mapping = mapping,
#'                         methods = list(lowerLimitMeasurement = heightMethod,
#'                                        upperLimitMeasurement = heightMethod,
#'                                        cover=coverscale),
#'                         stratumDefinition = strataDef)
#'
#' # Examine results
#' head(showElementTable(x, "stratumObservation"))
#'
addStratumObservations<-function(target, x, mapping,
                                 methods,
                                 stratumDefinition,
                                 date.format = "%Y-%m-%d",
                                 missing.values = c(NA, ""),
                                 verbose = TRUE) {

  if(is.null(stratumDefinition)) stop("Stratum definition must be supplied to map stratum observations.")

  x = as.data.frame(x)
  nrecords = nrow(x)
  nmissing = 0


  #Check columns exist
  for(i in 1:length(mapping)) {
    if(!(mapping[i] %in% names(x))) stop(paste0("Variable '", mapping[i],"' not found in column names. Revise mapping or data."))
  }
  plotNames = as.character(x[[mapping[["plotName"]]]])
  obsStartDates = as.Date(as.character(x[[mapping[["obsStartDate"]]]]), format = date.format)
  stratumNamesData = as.character(x[[mapping[["stratumName"]]]])

  #Optional mappings
  subPlotFlag = ("subPlotName" %in% names(mapping))
  if(subPlotFlag) {
    subPlotNames = as.character(x[[mapping[["subPlotName"]]]])
  }


  #Check duplicate records
  stratumObservationMappingsAvailable = c("plotName", "obsStartDate", "subPlotName", "stratumName")
  mapcols = as.character(mapping[stratumObservationMappingsAvailable[c(T,T,subPlotFlag,T)]])
  xstrings = apply(x[, mapcols],1, paste, collapse=" ")
  us = length(unique(xstrings))
  if(us<nrow(x)) warning(paste0(nrow(x)-us," duplicate records found!"))

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

  strmesmapping = mapping[!(names(mapping) %in% c(stratumObservationMappingsAvailable, "lowerLimitMeasurement", "upperLimitMeasurement"))]
  if(verbose) cat(paste0(" ", length(strmesmapping)," stratum measurement variables found.\n"))
  if(length(strmesmapping)>0) {
    for(i in 1:length(strmesmapping)){
      if(!(names(strmesmapping)[[i]] %in% names(methods))) stop("Method definition must be provided for '",names(strmesmapping)[[i]],"'.")
      stratumMeasurementValues[[names(strmesmapping)[i]]] = as.character(x[[strmesmapping[[i]]]])
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
      # add literature citation if necessary
      if(method@citationString!="") {
        ncitid = .newLiteratureCitationIDByCitationString(target, method@citationString)
        if(ncitid$new) {
          target@literatureCitations[[ncitid$id]] = list(citationString =method@citationString)
          if(stratumDefMethod@DOI!="")  target@literatureCitations[[ncitid$id]]$DOI = method@DOI
        }
        target@methods[[methodID]]$citationID = ncitid$id
      }
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
  stratumDefMethod = stratumDefinition@method
  snmtid = .newMethodIDByName(target,stratumDefMethod@name)
  strmethodID = snmtid$id
  if(snmtid$new) {
    target@methods[[strmethodID]] = list(name = stratumDefMethod@name,
                                         description = stratumDefMethod@description,
                                         subject = stratumDefMethod@subject,
                                         attributeType = stratumDefMethod@attributeType)
    if(verbose) cat(paste0(" Stratum definition method '", stratumDefMethod@name,"' added.\n"))
    # add literature citation if necessary
    if(stratumDefMethod@citationString!="") {
      ncitid = .newLiteratureCitationIDByCitationString(target, stratumDefMethod@citationString)
      if(ncitid$new) {
        target@literatureCitations[[ncitid$id]] = list(citationString =stratumDefMethod@citationString)
        if(method@DOI!="")  target@literatureCitations[[ncitid$id]]$DOI = stratumDefMethod@DOI
      }
      target@methods[[strmethodID]]$citationID = ncitid$id
    }
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
    stratumNames = character(0)
    for(i in 1:nstr) {
      strid = .nextStratumID(target)
      stratumIDs[i] = strid
      stratumNames[i] = stratumDefinition@strata[[i]]$stratumName
      target@strata[[strid]] = stratumDefinition@strata[[i]]
      target@strata[[strid]]$methodID = strmethodID
    }
    finnstrata = length(target@strata)
    if(verbose) {
      cat(paste0(" ", finnstrata-orinstrata, " new stratum definitions added.\n"))
    }
  }
  else { #Read stratum IDs and stratum names from selected method
    if(verbose) cat(paste0(" Stratum definition '", stratumDefMethod@name,"' already included.\n"))
    stratumIDs = .getStratumIDsByMethodID(target,strmethodID)
    stratumNames = .getStratumNamesByMethodID(target,strmethodID)
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
    if(!(plotNames[i] %in% missing.values)) {# If plotName is missing take the previous one
      plotName = plotNames[i]
    }
    if(!(plotName %in% parsedPlots)) {
      npid = .newPlotIDByName(target, plotNames[i]) # Get the new plot ID (internal code)
      plotID = npid$id
      if(npid$new) target@plots[[plotID]] = list("plotName" = plotName)
      parsedPlots = c(parsedPlots, plotName)
      parsedPlotIDs = c(parsedPlotIDs, plotID)
    } else { #this access should be faster
      plotID = parsedPlotIDs[which(parsedPlots==plotName)]
    }
    #subplot (if defined)
    if(subPlotFlag){
      if(!(subPlotNames[i] %in% missing.values)) {# If subPlotName is missing use parent plot ID
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
    if(!(obsStartDates[i] %in% missing.values)) {# If observation date is missing take the previous one
      obsStartDate = obsStartDates[i]
    }
    pObsString = paste(plotID, obsStartDate) # plotID+Date
    if(!(pObsString %in% parsedPlotObs)) {
      npoid = .newPlotObsIDByDate(target, plotID, obsStartDate) # Get the new plot observation ID (internal code)
      plotObsID = npoid$id
      if(npoid$new) {
        target@plotObservations[[plotObsID]] = list("plotID" = plotID,
                                                    "obsStartDate" = obsStartDate)
      }
      parsedPlotObs = c(parsedPlotObs, pObsString)
      parsedPlotObsIDs = c(parsedPlotObsIDs, plotObsID)
    } else {
      plotObsID = parsedPlotObsIDs[which(parsedPlotObs==pObsString)]
    }

    # stratum observations
    if(!(stratumNamesData[i] %in% missing.values)) {# If stratum name is missing take the previous one
      stratumName = stratumNamesData[i]
    }
    if(!(stratumName %in% stratumNames)) stop(paste0(stratumName," not found within stratum names. Revise stratum definition or data."))
    strID = stratumIDs[which(stratumNames==stratumName)]
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
      if(m %in% names(mapping)) {
        method = methods[[m]]
        attIDs = methodAttIDs[[m]]
        codes = methodCodes[[m]]
        value = as.character(stratumMeasurementValues[[m]][i])
        if(!(value %in% missing.values)) {
          if(method@attributeType== "quantitative") {
            value = as.numeric(value)
            if(value> method@attributes[[1]]$upperLimit) {
              stop(paste0("Height '", value,"' larger than upper limit of measurement definition. Please revise scale or data."))
            }
            else if(value < method@attributes[[1]]$lowerLimit) {
              stop(paste0("Height '", value,"' smaller than lower limit of measurement definition. Please revise scale or data."))
            }
            strObs[[m]] = list("attributeID" = attIDs[1], "value" = value)
          } else {
            ind = which(codes==value)
            if(length(ind)==1) {
              strObs[[m]] = list("attributeID" = attIDs[ind], "value" = value)
            }
            else stop(paste0("Value '", value,"' not found in height measurement definition. Please revise height classes or data."))
          }
        } else {
          nmissing = nmissing + 1
        }
      }
    }
    # stratum measurements
    for(m in names(strmesmapping)) {
      method = methods[[m]]
      attIDs = methodAttIDs[[m]]
      codes = methodCodes[[m]]
      value = as.character(stratumMeasurementValues[[m]][i])
      if(!((value %in% missing.values) || (value==""))) {
        if(!("stratumMeasurements" %in% names(strObs))) strObs$stratumMeasurements = list()
        mesID = as.character(length(strObs$stratumMeasurements)+1)
        if(method@attributeType== "quantitative") {
          value = as.numeric(value)
          if(value> method@attributes[[1]]$upperLimit) {
            stop(paste0("Value '", value,"' larger than upper limit of measurement definition for '",m,"'. Please revise scale or data."))
          }
          else if(value < method@attributes[[1]]$lowerLimit) {
            stop(paste0("Value '", value,"' smaller than lower limit of measurement definition for '",m,"'. Please revise scale or data."))
          }
          strObs$stratumMeasurements[[mesID]] = list("attributeID" = attIDs[1], "value" = value)
        } else {
          ind = which(codes==value)
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
