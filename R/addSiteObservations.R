#' Add site observation records
#'
#' Adds site observation records to a VegX object from a data table where rows are plot observations,
#' using a mapping to identify plot observation: plot, subplot (optional) and observation date.
#' Additional mappings are used to map specific site variables.
#'
#' @param target The initial object of class \code{\linkS4class{VegX}} to be modified
#' @param x A data frame where each row corresponds to one plot observation. Columns can be varied.
#' @param projectTitle A string to identify the project title, which can be the same as one of the currently defined in \code{target}.
#' @param mapping A list with element names 'plotName', 'obsStartDate', used to specify the mapping of data columns (specified using strings for column names) onto these variables.
#' Site variables that can be mapped are: 'phosphorus', 'pottasium', 'magnesium', 'nitrogen' and 'pH'.
#' Additional optional mappings are: 'subPlotName' and 'obsEndDate'.
#' @param measurementMethods A named list of objects of class \code{\linkS4class{VegXMethod}} with the measurement method
#' for each of the site variables stated in \code{mapping}. List names should be the same as site variables
#' (e.g. \code{list(pH = pHmeth)} to specify the use of method '\code{pHmeth}' for pH measurements).
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
#' abiomapping = list(plotName = "Plot", subPlotName = "Subplot",
#'                    obsStartDate = "obsDate", obsEndDate = "obsEndDate",
#'                    pH = "pH")
#' # Define pH method
#' pHMeth = predefinedMeasurementMethod("pH")
#'
#' # Mapping process
#' z = addSiteObservations(target, site, "Mokihinui",
#'                            mapping = abiomapping,
#'                            measurementMethods = list(pH = pHMeth))
#'
#' summary(z)
#'
addSiteObservations<-function(target, x, projectTitle,
                              mapping,
                              measurementMethods = list(),
                              missing.values = c(NA,""),
                              verbose = TRUE) {
  x = as.data.frame(x)
  nrecords = nrow(x)
  nmissing = 0


  #check mappings
  soilVariables = c('phosphorus', 'potassium', 'magnesium', 'nitrogen','pH')
  siteVariables = c(soilVariables)
  mappingsAvailable = c("plotName", "obsStartDate", "obsEndDate", "subPlotName", siteVariables)
  siteValues = list()
  for(i in 1:length(mapping)) {
    if(!(names(mapping)[i] %in% mappingsAvailable)) stop(paste0("Mapping for '", names(mapping)[i], "' cannot be defined."))
    if(names(mapping)[i] %in% siteVariables) {
      if(!(names(mapping)[i] %in% names(measurementMethods))) stop(paste0("Measurement method should be provided corresponding to mapping '", names(mapping)[i], "'."))
      siteValues[[names(mapping)[i]]] = as.character(x[[mapping[[i]]]])
    }
  }

  #Check columns exist
  for(i in 1:length(mapping)) {
    if(!(mapping[i] %in% names(x))) stop(paste0("Variable '", mapping[i],"' not found in column names. Revise mapping or data."))
  }
  plotNames = as.character(x[[mapping[["plotName"]]]])
  obsStartDates = as.Date(as.character(x[[mapping[["obsStartDate"]]]]))

  #Optional mappings
  obsEndFlag = ("obsEndDate" %in% names(mapping))
  if(obsEndFlag) {
    obsEndDates = as.Date(as.character(x[[mapping[["obsEndDate"]]]]))
  }
  subPlotFlag = ("subPlotName" %in% names(mapping))
  if(subPlotFlag) {
    subPlotNames = as.character(x[[mapping[["subPlotName"]]]])
  }

  #check methods for site variables
  for(i in 1:length(measurementMethods)) {
    if(!(names(measurementMethods)[i] %in% siteVariables)) stop(paste0("Method for '", names(measurementMethods)[i], "' cannot be applied."))
    if(!(names(measurementMethods)[i] %in% names(mapping))) stop(paste0("Mapping should be defined corresponding to measurement method '", names(measurementMethods)[i], "'."))
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
      methodCodes[[m]] = .getAttributeCodesByMethodID(methodID)
      methodAttIDs[[m]] = .getAttributeIDsByMethodID(methodID)
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
    #site observations
    # TO BE DONE: CHECK that the site observation is new
    abioObsID = as.character(length(target@siteObservations)+1)
    abioObs = list("plotObservationID" = plotObsID)
    target@plotObservations[[plotObsID]]$siteObservationID = abioObsID # Set a one-to-one link
    for(m in names(measurementMethods)) {
      value = siteValues[[m]][i]
      method = measurementMethods[[m]]
      attIDs = methodAttIDs[[m]]
      codes = methodCodes[[m]]
      if(!(value %in% as.character(missing.values))) {
        if(m %in% soilVariables) {
          if(!("soil" %in% names(abioObs))) abioObs$soil = list()
          if(method@attributeType== "quantitative") {
            value = as.numeric(value)
            if(value > method@attributes[[1]]$upperBound) {
              stop(paste0("Value '", value,"' for '", m, "' larger than upper bound of measurement definition. Please revise scale or data."))
            }
            else if(value < method@attributes[[1]]$lowerBound) {
              stop(paste0("Value '", value,"' for '", m, "' smaller than lower bound of measurement definition. Please revise scale or data."))
            }
            abioObs$soil[[m]] = list("attributeID" = attIDs[[1]],
                                "value" = value)
          } else {
            ind = which(codes==as.character(value))
            if(length(ind)==1) {
              abioObs$soil[[m]] = list("attributeID" = attIDs[[ind]],
                                  "value" = value)
            }
            else stop(paste0("Value '", value,"' for '", m, "' not found in measurement definition. Please revise classes or data."))
          }
        }
      } else {
        nmissing = nmissing + 1
      }

    }
    target@siteObservations[[abioObsID]] = abioObs
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
