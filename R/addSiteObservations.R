#' Add site observation records
#'
#' Adds site observation records to a VegX object from a data table where rows are plot observations,
#' using a mapping to identify plot observation: plot, subplot (optional) and observation date.
#' Additional mappings are used to map specific site variables.
#'
#' @param target The initial object of class \code{\linkS4class{VegX}} to be modified
#' @param x A data frame where each row corresponds to one plot observation. Columns can be varied.
#' @param plotObservationMapping A list with element names 'plotName', 'obsStartDate', used to specify the mapping of data columns (specified using strings for column names) onto these variables.
#' Additional optional mappings are: 'subPlotName'.
#' @param soilMeasurementMapping A list with element names equal to soil measurement subjects, used to specify the mapping of data columns (specified using strings for column names) onto these variables.
#' @param climateMeasurementMapping A list with element names equal to climate measurement subjects, used to specify the mapping of data columns (specified using strings for column names) onto these variables.
#' @param waterMassMeasurementMapping A list with element names equal to water mass measurement subjects, used to specify the mapping of data columns (specified using strings for column names) onto these variables.
#' @param soilMeasurementMethods A named list of objects of class \code{\linkS4class{VegXMethod}} with the measurement method
#' for each of the soil variables stated in \code{soilMeasurementMapping}. List names should be the same as soil subject measurement variables
#' (e.g. \code{list(pH = pHmeth)} to specify the use of method '\code{pHmeth}' for pH measurements).
#' @param climateMeasurementMethods A named list of objects of class \code{\linkS4class{VegXMethod}} with the measurement method
#' for each of the soil variables stated in \code{soilMeasurementMapping}. List names should be the same as climate subject measurement variables.
#' @param waterMassMeasurementMethods A named list of objects of class \code{\linkS4class{VegXMethod}} with the measurement method
#' for each of the soil variables stated in \code{soilMeasurementMapping}. List names should be the same as water mass subject measurement variables.
#' @param missing.values A character vector of values that should be considered as missing observations/measurements.
#' @param verbose A boolean flag to indicate console output of the data integration process.
#'
#' @return The modified object of class \code{\linkS4class{VegX}}.
#' @export
#'
#' @details Missing plotName or obsStartDate values are interpreted as if the previous non-missing value has to be used to define plot observation.
#' Missing subPlotName values are interpreted in that observation refers to the parent plotName.
#' Missing measurements are simply not added to the Veg-X document.
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
#' mapping = list(plotName = "Plot", subPlotName = "Subplot",
#'                obsStartDate = "PlotObsStartDate")
#' soilmapping = list(pH = "pH")
#'
#' # Define pH method
#' pHMeth = predefinedMeasurementMethod("pH")
#'
#' # Create new Veg-X document with site observations
#' x = addSiteObservations(newVegX(), moki_site,
#'                         plotObservationMapping = mapping,
#'                         soilMeasurementMapping = soilmapping,
#'                         soilMeasurementMethods = list(pH = pHMeth))
#' # Examine results
#' summary(x)
#' head(showElementTable(x, "siteObservation"))
#'
addSiteObservations<-function(target, x,
                              plotObservationMapping,
                              soilMeasurementMapping = list(),
                              climateMeasurementMapping = list(),
                              waterMassMeasurementMapping = list(),
                              soilMeasurementMethods = list(),
                              climateMeasurementMethods = list(),
                              waterMassMeasurementMethods = list(),
                              missing.values = c(NA,""),
                              verbose = TRUE) {
  x = as.data.frame(x)
  nrecords = nrow(x)
  nmissing = 0


  # Get recognized site subjects
  soilVariables = c('pH')
  climateVariables = c()
  waterMassVariables = c()

  #check mappings
  siteVariables = c(soilVariables, climateVariables, waterMassVariables)
  plotObservationMappingsAvailable = c("plotName", "obsStartDate", "subPlotName")
  siteValues = list()
  for(i in 1:length(plotObservationMapping)) {
    if(!(names(plotObservationMapping)[i] %in% plotObservationMappingsAvailable)) stop(paste0("Mapping for '", names(plotObservationMapping)[i], "' cannot be defined."))
  }
  if(length(soilMeasurementMapping)>0) {
    for(i in 1:length(soilMeasurementMapping)) {
      if(!(names(soilMeasurementMapping)[i] %in% soilVariables)) stop(paste0("Mapping for '", names(soilMeasurementMapping)[i], "' cannot be defined."))
      if(!(names(soilMeasurementMapping)[i] %in% names(soilMeasurementMethods))) stop(paste0("Measurement method should be provided corresponding to mapping '", names(soilMeasurementMapping)[i], "'."))
      siteValues[[names(soilMeasurementMapping)[i]]] = as.character(x[[soilMeasurementMapping[[i]]]])
    }
  }
  if(length(climateMeasurementMapping)>0) {
    for(i in 1:length(climateMeasurementMapping)) {
      if(!(names(climateMeasurementMapping)[i] %in% climateVariables)) stop(paste0("Mapping for '", names(climateMeasurementMapping)[i], "' cannot be defined."))
      if(!(names(climateMeasurementMapping)[i] %in% names(climateMeasurementMethods))) stop(paste0("Measurement method should be provided corresponding to mapping '", names(climateMeasurementMapping)[i], "'."))
      siteValues[[names(climateMeasurementMapping)[i]]] = as.character(x[[climateMeasurementMapping[[i]]]])
    }
  }
  if(length(waterMassMeasurementMapping)>0) {
    for(i in 1:length(waterMassMeasurementMapping)) {
      if(!(names(waterMassMeasurementMapping)[i] %in% waterMassVariables)) stop(paste0("Mapping for '", names(waterMassMeasurementMapping)[i], "' cannot be defined."))
      if(!(names(waterMassMeasurementMapping)[i] %in% names(waterMassMeasurementMethods))) stop(paste0("Measurement method should be provided corresponding to mapping '", names(waterMassMeasurementMapping)[i], "'."))
      siteValues[[names(waterMassMeasurementMapping)[i]]] = as.character(x[[waterMassMeasurementMapping[[i]]]])
    }
  }
  #Check columns exist
  for(i in 1:length(plotObservationMapping)) {
    if(!(plotObservationMapping[i] %in% names(x))) stop(paste0("Variable '", plotObservationMapping[i],"' not found in column names. Revise mapping or data."))
  }
  if(length(soilMeasurementMapping)>0) {
    if(length(soilMeasurementMapping)>0) {
      for(i in 1:length(soilMeasurementMapping)) {
        if(!(soilMeasurementMapping[i] %in% names(x))) stop(paste0("Variable '", soilMeasurementMapping[i],"' not found in column names. Revise mapping or data."))
      }
    }
  }
  if(length(climateMeasurementMapping)>0) {
    if(length(climateMeasurementMapping)>0) {
      for(i in 1:length(climateMeasurementMapping)) {
        if(!(climateMeasurementMapping[i] %in% names(x))) stop(paste0("Variable '", climateMeasurementMapping[i],"' not found in column names. Revise mapping or data."))
      }
    }
  }
  if(length(waterMassMeasurementMapping)>0) {
    if(length(waterMassMeasurementMapping)>0) {
      for(i in 1:length(waterMassMeasurementMapping)) {
        if(!(waterMassMeasurementMapping[i] %in% names(x))) stop(paste0("Variable '", waterMassMeasurementMapping[i],"' not found in column names. Revise mapping or data."))
      }
    }
  }
  plotNames = as.character(x[[plotObservationMapping[["plotName"]]]])
  obsStartDates = as.Date(as.character(x[[plotObservationMapping[["obsStartDate"]]]]))

  #Optional mappings
  subPlotFlag = ("subPlotName" %in% names(plotObservationMapping))
  if(subPlotFlag) {
    subPlotNames = as.character(x[[plotObservationMapping[["subPlotName"]]]])
  }

  #check methods for site variables
  if(length(soilMeasurementMethods)>0) {
    for(i in 1:length(soilMeasurementMethods)) {
      if(!(names(soilMeasurementMethods)[i] %in% soilVariables)) stop(paste0("Method for '", names(soilMeasurementMethods)[i], "' cannot be applied."))
      if(!(names(soilMeasurementMethods)[i] %in% names(soilMeasurementMapping))) stop(paste0("Mapping should be defined corresponding to measurement method '", names(soilMeasurementMethods)[i], "'."))
    }
  }
  if(length(climateMeasurementMethods)>0) {
    for(i in 1:length(climateMeasurementMethods)) {
      if(!(names(climateMeasurementMethods)[i] %in% climateVariables)) stop(paste0("Method for '", names(climateMeasurementMethods)[i], "' cannot be applied."))
      if(!(names(climateMeasurementMethods)[i] %in% names(climateMeasurementMapping))) stop(paste0("Mapping should be defined corresponding to measurement method '", names(climateMeasurementMethods)[i], "'."))
    }
  }
  if(length(waterMassMeasurementMethods)>0) {
    for(i in 1:length(waterMassMeasurementMethods)) {
      if(!(names(waterMassMeasurementMethods)[i] %in% waterMassVariables)) stop(paste0("Method for '", names(waterMassMeasurementMethods)[i], "' cannot be applied."))
      if(!(names(waterMassMeasurementMethods)[i] %in% names(waterMassMeasurementMapping))) stop(paste0("Mapping should be defined corresponding to measurement method '", names(waterMassMeasurementMethods)[i], "'."))
    }
  }


  #add methods
  methodIDs = character(0)
  methodCodes = list()
  methodAttIDs = list()
  measurementMethods = c(soilMeasurementMethods, climateMeasurementMethods, waterMassMeasurementMethods)
  for(m in names(measurementMethods)) {
    method = measurementMethods[[m]]
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


  orinplots = length(target@plots)
  orinplotobs = length(target@plotObservations)
  orinabioobs = length(target@siteObservations)
  parsedPlots = character(0)
  parsedPlotIDs = character(0)
  parsedPlotObs = character(0)
  parsedPlotObsIDs = character(0)
  abioObsCounter = orinabioobs+1 #counter
  #Record parsing loop
  for(i in 1:nrecords) {
    #plot
    if(!(plotNames[i] %in% missing.values)) plotName = plotNames[i] # Missing plot means we keep with plot of the previous record
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
      if(!(subPlotNames[i] %in% missing.values)) { # Missing subplot means that we add information to the plot
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
    if(!(obsStartDates[i] %in% missing.values)) obsStartDate = obsStartDates[i] # Missing date means we keep with date of the previous record
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
      plotObsID = parsedPlotIDs[which(parsedPlotObs==pObsString)]
    }

    #site observations
    soid = .newSiteObservationIDByID(target, plotObsID)
    siteObsID = soid$id
    if(soid$new) {
      siteObs = list("plotObservationID" = plotObsID)
      target@plotObservations[[plotObsID]]$siteObservationID = siteObsID # Set a one-to-one link
    } else {
      siteObs = target@siteObservations[[siteObsID]]
    }
    for(m in names(measurementMethods)) {
      value = siteValues[[m]][i]
      method = measurementMethods[[m]]
      attIDs = methodAttIDs[[m]]
      codes = methodCodes[[m]]
      if(!(value %in% as.character(missing.values))) {
        if(m %in% soilVariables) {
          if(!("soilMeasurements" %in% names(siteObs))) siteObs$soilMeasurements = list()
          mesID = as.character(length(siteObs$soilMeasurements)+1)
          siteObs$soilMeasurements[[mesID]] = list()
          if(method@attributeType== "quantitative") {
            value = as.numeric(value)
            if(value > method@attributes[[1]]$upperLimit) {
              stop(paste0("Value '", value,"' for '", m, "' larger than upper limit of measurement definition. Please revise scale or data."))
            }
            else if(value < method@attributes[[1]]$lowerLimit) {
              stop(paste0("Value '", value,"' for '", m, "' smaller than lower limit of measurement definition. Please revise scale or data."))
            }
            siteObs$soilMeasurements[[mesID]] = list("attributeID" = attIDs[[1]],
                                                          "value" = value)
          } else {
            ind = which(codes==as.character(value))
            if(length(ind)==1) {
              siteObs$soilMeasurements[[mesID]] = list("attributeID" = attIDs[[ind]],
                                  "value" = value)
            }
            else stop(paste0("Value '", value,"' for '", m, "' not found in measurement definition. Please revise classes or data."))
          }
        }
        if(m %in% climateVariables) {
          if(!("climateMeasurements" %in% names(siteObs))) siteObs$climateMeasurements = list()
          mesID = as.character(length(siteObs$climateMeasurements)+1)
          siteObs$climateMeasurements[[mesID]] = list()
          if(method@attributeType== "quantitative") {
            value = as.numeric(value)
            if(value > method@attributes[[1]]$upperLimit) {
              stop(paste0("Value '", value,"' for '", m, "' larger than upper limit of measurement definition. Please revise scale or data."))
            }
            else if(value < method@attributes[[1]]$lowerLimit) {
              stop(paste0("Value '", value,"' for '", m, "' smaller than lower limit of measurement definition. Please revise scale or data."))
            }
            siteObs$climateMeasurements[[mesID]] = list("attributeID" = attIDs[[1]],
                                                     "value" = value)
          } else {
            ind = which(codes==as.character(value))
            if(length(ind)==1) {
              siteObs$climateMeasurements[[mesID]] = list("attributeID" = attIDs[[ind]],
                                                       "value" = value)
            }
            else stop(paste0("Value '", value,"' for '", m, "' not found in measurement definition. Please revise classes or data."))
          }
        }
        if(m %in% waterMassVariables) {
          if(!("waterMassMeasurements" %in% names(siteObs))) siteObs$waterMassMeasurements = list()
          mesID = as.character(length(siteObs$waterMassMeasurements)+1)
          siteObs$waterMassMeasurements[[mesID]] = list()
          if(method@attributeType== "quantitative") {
            value = as.numeric(value)
            if(value > method@attributes[[1]]$upperLimit) {
              stop(paste0("Value '", value,"' for '", m, "' larger than upper limit of measurement definition. Please revise scale or data."))
            }
            else if(value < method@attributes[[1]]$lowerLimit) {
              stop(paste0("Value '", value,"' for '", m, "' smaller than lower limit of measurement definition. Please revise scale or data."))
            }
            siteObs$waterMassMeasurements[[mesID]] = list("attributeID" = attIDs[[1]],
                                                        "value" = value)
          } else {
            ind = which(codes==as.character(value))
            if(length(ind)==1) {
              siteObs$waterMassMeasurements[[mesID]] = list("attributeID" = attIDs[[ind]],
                                                          "value" = value)
            }
            else stop(paste0("Value '", value,"' for '", m, "' not found in measurement definition. Please revise classes or data."))
          }
        }
      } else {
        nmissing = nmissing + 1
      }

    }
    target@siteObservations[[siteObsID]] = siteObs
  }
  finnplots = length(target@plots)
  finnplotobs = length(target@plotObservations)
  finnabioobs = length(target@siteObservations)
  if(verbose) {
    cat(paste0(" " , length(parsedPlots)," plot(s) parsed, ", finnplots-orinplots, " new plot(s) added.\n"))
    cat(paste0(" " , length(parsedPlotObs)," plot observation(s) parsed, ", finnplotobs-orinplotobs, " new plot observation(s) added.\n"))
    cat(paste0(" ", nrecords," record(s) parsed, ", finnabioobs-orinabioobs, " new site observation(s) added.\n"))
    if(nmissing>0) cat(paste0(" ", nmissing, " site measurement(s) with missing value(s) not added.\n"))
  }

  return(target)
}
