#' Add site observation records
#'
#' Adds site observation records to a VegX object from a data table where rows
#' are plot observations.
#'
#' @param target The initial object of class \code{\linkS4class{VegX}} to be
#'   modified
#' @param x A data frame where each row corresponds to one plot observation.
#'   Columns can be varied.
#' @param plotObservationMapping A list with element names 'plotName',
#'   'obsStartDate', used to specify the mapping of data columns (specified
#'   using strings for column names) onto these variables. Additional optional
#'   mappings are: 'subPlotName'.
#' @param soilMeasurementMapping A named list used to specify the mapping of
#'   data columns to soil variables (e.g. a = "pH" to map variable "pH" of the
#'   data frame). List names should be the same as in
#'   \code{soilMeasurementMethods}.
#' @param climateMeasurementMapping A named list used to specify the mapping of
#'   data columns to climate variables. List names should be the same as in
#'   \code{climateMeasurementMethods}.
#' @param waterBodyMeasurementMapping A named list used to specify the mapping
#'   of data columns to water body variables. List names should be the same as
#'   in \code{waterBodyMeasurementMethods}.
#' @param soilMeasurementMethods A named list of objects of class
#'   \code{\linkS4class{VegXMethodDefinition}} with the measurement method for
#'   each of the element names stated in \code{soilMeasurementMapping} (e.g.
#'   \code{list(a = pHmeth)} to specify the use of method '\code{pHmeth}' for
#'   soil1). Alternatively, methods can be specified using strings if predefined
#'   methods exist (e.g. \code{list(a = "pH/0-14")} to use the predefined method
#'   "pH/0-14"), see \code{\link{predefinedMeasurementMethod}}.
#' @param climateMeasurementMethods A named list of objects of class
#'   \code{\linkS4class{VegXMethodDefinition}} with the measurement method for
#'   each of the element names stated in \code{climateMeasurementMapping}. List
#'   names should be the same as climate subject measurement variables.
#'   Alternatively, methods can be specified using strings if predefined methods
#'   exist, see \code{\link{predefinedMeasurementMethod}}.
#' @param waterBodyMeasurementMethods A named list of objects of class
#'   \code{\linkS4class{VegXMethodDefinition}} with the measurement method for
#'   each of the element names stated in \code{waterBodyMeasurementMapping}.
#'   List names should be the same as water body subject measurement variables.
#'   Alternatively, methods can be specified using strings if predefined methods
#'   exist, see \code{\link{predefinedMeasurementMethod}}.
#' @param date.format A character string specifying the input format of dates
#'   (see \code{\link{as.Date}}).
#' @param missing.values A character vector of values that should be considered
#'   as missing observations/measurements.
#' @param fill.methods A flag to indicate that missing methods should be filled
#'   with dummy ones. This allows easily storing any environmental data, but
#'   without appropriate metadata.
#' @param verbose A boolean flag to indicate console output of the data
#'   integration process.
#'
#' @return The modified object of class \code{\linkS4class{VegX}}.
#'
#' @details 
#' Unlike in other functions, here the element names of mappings are only used
#' to find the corresponding method. The measured subject (e.g. pH, salinity or
#' mean annual temperature) is taken from the method definition. There is one
#' exception to this rule: users can use \code{fill.methods = TRUE} to skip
#' defining methods for all environmental variables. In this case, the function
#' will define dummy measurement methods, taking the element name of the mapping
#' list as subject. For example, if \code{soilMeasurementMapping = list(pHvar =
#' "pH")} and no method is provided for pHvar in \code{soilMeasurementMethods},
#' the function will create a dummy measurement method called 'pHvar'. Although
#' this possibility is given to ease import, users are encouraged to define site
#' measurement methods or to use predefined ones. When defining measurement
#' methods, users should preferably name subjects using the same strings as in
#' predefined methods, because this facilitates merging datasets where the same
#' entities have been measured. Please contact Veg-X developers to ask for
#' additional predefined measurement methods if you think they are relevant for
#' exchanging vegetation plot data.
#' 
#' Missing value policy:
#' \itemize{
#'   \item{Missing 'plotName' or 'obsStartDate' values are interpreted as if the
#'   previous non-missing value has to be used to define plot observation.}
#'   \item{Missing 'subPlotName' values are interpreted in that observation
#'   refers to the parent plotName.}
#'   \item{Missing measurements are simply not added to the Veg-X document.}
#' }
#' @references Wiser SK, Spencer N, De Caceres M, Kleikamp M, Boyle B & Peet RK
#'   (2011). Veg-X - an exchange standard for plot-based vegetation data
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
#'
#' # Create new Veg-X document with site observations
#' # Uses predefined measurement method "pH/0-14"
#' x = addSiteObservations(newVegX(), moki_site,
#'                         plotObservationMapping = mapping,
#'                         soilMeasurementMapping = list(a = "pH") ,
#'                         soilMeasurementMethods = list(a = "pH/0-14"))
#' # Examine results
#' summary(x)
#' head(showElementTable(x, "siteObservation", subject=TRUE))
#'
addSiteObservations<-function(target, x,
                              plotObservationMapping,
                              soilMeasurementMapping = list(),
                              climateMeasurementMapping = list(),
                              waterBodyMeasurementMapping = list(),
                              soilMeasurementMethods = list(),
                              climateMeasurementMethods = list(),
                              waterBodyMeasurementMethods = list(),
                              date.format = "%Y-%m-%d",
                              missing.values = c(NA,""),
                              fill.methods = FALSE,
                              verbose = TRUE) {

  if(class(target)!="VegX") 
    stop("Wrong class for 'target'. Should be an object of class 'VegX'")
  x = as.data.frame(x)
  nrecords = nrow(x)
  nmissing = 0

  
  
  #check mappings
  soilVariables = c()
  climateVariables = c()
  waterBodyVariables = c()
  

  plotObservationMappingsAvailable = c("plotName", "obsStartDate", "subPlotName")
  siteValues = list()
  for(i in 1:length(plotObservationMapping)) {
    if(!(names(plotObservationMapping)[i] %in% plotObservationMappingsAvailable)) stop(paste0("Mapping for '", names(plotObservationMapping)[i], "' cannot be defined."))
  }
  if(length(soilMeasurementMapping)>0) {
    for(i in 1:length(soilMeasurementMapping)) {
      soilVariables = c(soilVariables, names(soilMeasurementMapping)[i])
      siteValues[[names(soilMeasurementMapping)[i]]] = as.character(x[[soilMeasurementMapping[[i]]]])
      if(!(names(soilMeasurementMapping)[i] %in% names(soilMeasurementMethods))) {
        if(!fill.methods)  stop(paste0("Measurement method should be provided corresponding to mapping '", names(soilMeasurementMapping)[i], "' (alternatively, set 'fill.methods = TRUE')."))
        else {
          varname = names(soilMeasurementMapping)[i]
          values = x[[soilMeasurementMapping[[i]]]]
          warning(paste0("Dummy measurement method defined for '", varname, "'."))
          varclass = class(values)
          newMethod = defineQuantitativeScaleMethod(varname, description = "unknown", subject=varname, unit="unknown")
          soilMeasurementMethods[[varname]] = newMethod
        }
      }
    }
  }
  if(length(climateMeasurementMapping)>0) {
    for(i in 1:length(climateMeasurementMapping)) {
      climateVariables = c(climateVariables, names(climateMeasurementMapping)[i])
      siteValues[[names(climateMeasurementMapping)[i]]] = as.character(x[[climateMeasurementMapping[[i]]]])
      if(!(names(climateMeasurementMapping)[i] %in% names(climateMeasurementMethods))) {
        if(!fill.methods)  stop(paste0("Measurement method should be provided corresponding to mapping '", names(climateMeasurementMapping)[i], "' (alternatively, set 'fill.methods = TRUE')."))
        else {
          varname = names(climateMeasurementMapping)[i]
          values = x[[climateMeasurementMapping[[i]]]]
          warning(paste0("Dummy measurement method defined for '", varname, "'."))
          varclass = class(values)
          newMethod = defineQuantitativeScaleMethod(varname, description = "unknown", subject=varname, unit="unknown")
          climateMeasurementMethods[[varname]] = newMethod
        }
      }
    }
  }
  if(length(waterBodyMeasurementMapping)>0) {
    for(i in 1:length(waterBodyMeasurementMapping)) {
      waterBodyVariables = c(waterBodyVariables, names(waterBodyMeasurementMapping)[i])
      siteValues[[names(waterBodyMeasurementMapping)[i]]] = as.character(x[[waterBodyMeasurementMapping[[i]]]])
      if(!(names(waterBodyMeasurementMapping)[i] %in% names(waterBodyMeasurementMethods))) {
        if(!fill.methods)  stop(paste0("Measurement method should be provided corresponding to mapping '", names(waterBodyMeasurementMapping)[i], "' (alternatively, set 'fill.methods = TRUE')."))
        else {
          varname = names(waterBodyMeasurementMapping)[i]
          values = x[[waterBodyMeasurementMapping[[i]]]]
          warning(paste0("Dummy measurement method defined for '", varname, "'."))
          varclass = class(values)
          newMethod = defineQuantitativeScaleMethod(varname, description = "unknown", subject=varname, unit="unknown")
          waterBodyMeasurementMethods[[varname]] = newMethod
        }
      }
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
  if(length(waterBodyMeasurementMapping)>0) {
    if(length(waterBodyMeasurementMapping)>0) {
      for(i in 1:length(waterBodyMeasurementMapping)) {
        if(!(waterBodyMeasurementMapping[i] %in% names(x))) stop(paste0("Variable '", waterBodyMeasurementMapping[i],"' not found in column names. Revise mapping or data."))
      }
    }
  }
  plotNames = as.character(x[[plotObservationMapping[["plotName"]]]])
  obsStartDates = as.Date(as.character(x[[plotObservationMapping[["obsStartDate"]]]]), format = date.format)

  #Optional mappings
  subPlotFlag = ("subPlotName" %in% names(plotObservationMapping))
  if(subPlotFlag) {
    subPlotNames = as.character(x[[plotObservationMapping[["subPlotName"]]]])
  }

  #check methods for site variables
  if(length(soilMeasurementMethods)>0) {
    for(i in 1:length(soilMeasurementMethods)) {
      if(!(names(soilMeasurementMethods)[i] %in% names(soilMeasurementMapping))) stop(paste0("Mapping should be defined corresponding to measurement method '", names(soilMeasurementMethods)[i], "'."))
    }
  }
  if(length(climateMeasurementMethods)>0) {
    for(i in 1:length(climateMeasurementMethods)) {
      if(!(names(climateMeasurementMethods)[i] %in% names(climateMeasurementMapping))) stop(paste0("Mapping should be defined corresponding to measurement method '", names(climateMeasurementMethods)[i], "'."))
    }
  }
  if(length(waterBodyMeasurementMethods)>0) {
    for(i in 1:length(waterBodyMeasurementMethods)) {
      if(!(names(waterBodyMeasurementMethods)[i] %in% names(waterBodyMeasurementMapping))) stop(paste0("Mapping should be defined corresponding to measurement method '", names(waterBodyMeasurementMethods)[i], "'."))
    }
  }


  #add methods
  methodIDs = character(0)
  methodCodes = list()
  methodAttIDs = list()
  measurementMethods = c(soilMeasurementMethods, climateMeasurementMethods, waterBodyMeasurementMethods)
  for(m in names(measurementMethods)) {
    method = measurementMethods[[m]]
    if(class(method)=="character") {
      method = predefinedMeasurementMethod(method)
      measurementMethods[[m]] = method
    }
    else if (class(method) != "VegXMethodDefinition") stop(paste("Wrong class for method: ",m ,"."))
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
          if(method@DOI!="")  target@literatureCitations[[ncitid$id]]$DOI = method@DOI
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
        if(m %in% waterBodyVariables) {
          if(!("waterBodyMeasurements" %in% names(siteObs))) siteObs$waterBodyMeasurements = list()
          mesID = as.character(length(siteObs$waterBodyMeasurements)+1)
          siteObs$waterBodyMeasurements[[mesID]] = list()
          if(method@attributeType== "quantitative") {
            value = as.numeric(value)
            if(value > method@attributes[[1]]$upperLimit) {
              stop(paste0("Value '", value,"' for '", m, "' larger than upper limit of measurement definition. Please revise scale or data."))
            }
            else if(value < method@attributes[[1]]$lowerLimit) {
              stop(paste0("Value '", value,"' for '", m, "' smaller than lower limit of measurement definition. Please revise scale or data."))
            }
            siteObs$waterBodyMeasurements[[mesID]] = list("attributeID" = attIDs[[1]],
                                                        "value" = value)
          } else {
            ind = which(codes==as.character(value))
            if(length(ind)==1) {
              siteObs$waterBodyMeasurements[[mesID]] = list("attributeID" = attIDs[[ind]],
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
