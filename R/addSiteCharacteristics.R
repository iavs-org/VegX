#' Adds/replaces site information
#'
#' Adds/replaces static site characteristics (topography, geology, ...) to plot elements of a VegX object from a data table where rows are plots,
#' using a mapping to identify plot and subplot (optional). Additional mapping elements are used to map specific site variables.
#'
#' @param target The initial object of class \code{\linkS4class{VegX}} to be modified
#' @param x A data frame where each row corresponds to one plot observation. Columns can be varied.
#' @param projectTitle A string to identify the project title, which can be the same as one of the currently defined in \code{target}.
#' @param mapping A list with at least element name 'plotName', is used to specify the mapping of data columns (specified using strings for column names) onto these variables.
#' Site variables that can be mapped are: 'elevation','slope', 'aspect', 'landform', 'parentMaterial', 'geologyClass'.
#' Additional optional mappings are: 'subPlotName'.
#' @param measurementMethods A named list of objects of class \code{\linkS4class{VegXMethod}} with the measurement method
#' for each of the abiotic variables stated in \code{mapping}. List names should be the same as abiotic variables
#' (e.g. \code{list(aspect = aspectMeth)} to specify the use of method '\code{aspectMeth}' for aspect measurements).
#' @param missing.values A character vector of values that should be considered as missing observations/measurements.
#' @param verbose A boolean flag to indicate console output of the data integration process.
#'
#' @return The modified object of class \code{\linkS4class{VegX}}.
#' @export
#'
#' @references Wiser SK, Spencer N, De Caceres M, Kleikamp M, Boyle B & Peet RK (2011). Veg-X - an exchange standard for plot-based vegetation data
#'
#' @seealso \code{\link{addTaxonObservations}}, \code{\link{addTreeObservations}}.
#'
#' @examples
#' data(mokihinui)
#'
#' # Create new Veg-X document
#' target = newVegX()
#'
#' # Define mapping
#' sitemapping = list(plotName = "Plot", subPlotName = "Subplot",
#'                    elevation = "Altitude", slope = "PlotSlope", aspect = "PlotAspect")
#'
#' # Define site methods
#' elevm = predefinedMeasurementMethod("Elevation meters")
#' slopeDeg = predefinedMeasurementMethod("Slope degrees")
#' aspectDeg = predefinedMeasurementMethod("Aspect degrees")
#'
#' # Mapping process
#' w = addSiteCharacteristics(target, site, "Mokihinui",
#'                            mapping = sitemapping,
#'                            measurementMethods = list(elevation = elevm, slope = slopeDeg, aspect = aspectDeg))
#'
#' summary(w)
#'
addSiteCharacteristics<-function(target, x, projectTitle,
                                 mapping,
                                 measurementMethods = list(),
                                 missing.values = c(NA,""),
                                 verbose = TRUE) {
  x = as.data.frame(x)
  nrecords = nrow(x)
  nmissing = 0


  #check mappings
  siteVariables = c("elevation","slope", "aspect", "landform", "parentMaterial", "geologyClass")
  mappingsAvailable = c("plotName", "subPlotName", siteVariables)
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

  #Optional mappings
  obsEndFlag = ("obsEndDate" %in% names(mapping))
  if(obsEndFlag) {
    obsEndDates = as.Date(as.character(x[[mapping[["obsEndDate"]]]]))
  }
  subPlotFlag = ("subPlotName" %in% names(mapping))
  if(subPlotFlag) {
    subPlotNames = as.character(x[[mapping[["subPlotName"]]]])
  }

  #check methods for abiotic variables
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
                                        attributeClass = method@attributeClass,
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
  parsedPlots = character(0)
  parsedPlotIDs = character(0)
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
    #site characteristics
    for(m in names(measurementMethods)) {
      value = siteValues[[m]][i]
      method = measurementMethods[[m]]
      attIDs = methodAttIDs[[m]]
      codes = methodCodes[[m]]
      if(!(value %in% as.character(missing.values))) {
        if(method@attributeType== "quantitative") {
          # print(paste0(plotID," ", m, " ", value))
          value = as.numeric(value)
          if(value > method@attributes[[1]]$upperBound) {
            stop(paste0("Value '", value,"' for '", m, "' larger than upper bound of measurement definition. Please revise scale or data."))
          }
          else if(value < method@attributes[[1]]$lowerBound) {
            stop(paste0("Value '", value,"' for '", m, "' smaller than lower bound of measurement definition. Please revise scale or data."))
          }
          target@plots[[plotID]][[m]] = list("attributeID" = attIDs[[1]],
                              "value" = value)
          # print(target@plots[[plotID]][[m]])
        } else {
          ind = which(codes==as.character(value))
          if(length(ind)==1) {
            target@plots[[plotID]][[m]] = list("attributeID" = attIDs[[ind]],
                                "value" = value)
          }
          else stop(paste0("Value '", value,"' for '", m, "' not found in measurement definition. Please revise classes or data."))
        }
      } else {
        nmissing = nmissing + 1
      }

    }
  }
  finnplots = length(target@plots)
  if(verbose) {
    cat(paste0(" " , length(parsedPlots)," plot(s) parsed, ", finnplots-orinplots, " new plot(s) added.\n"))
    cat(paste0(" ", nrecords," record(s) parsed.\n"))
    if(nmissing>0) cat(paste0(" ", nmissing, " measurement(s) with missing value(s) not added.\n"))
  }

  return(target)
}
