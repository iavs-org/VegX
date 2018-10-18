#' Adds/replaces site information
#'
#' Adds/replaces static site characteristics (topography, geology, ...) to plot elements of a VegX object from a data frame where rows are plots.
#'
#' @param target The initial object of class \code{\linkS4class{VegX}} to be modified
#' @param x A data frame where each row corresponds to one plot observation. Columns can be varied.
#' @param mapping A named list whose elements are strings that correspond to column names in \code{x}. Names of the list should be:
#' \itemize{
#'    \item{\code{plotName} - A string identifying the vegetation plot within the data set (required).}
#'    \item{\code{subPlotName} - A string identifying a subplot of the plot given by \code{plotName} (optional).}
#'    \item{\code{slope} - Slope of the plot (optional).}
#'    \item{\code{aspect} - Aspect (i.e. orientation) of the plot (optional).}
#'    \item{\code{landform} - Site land form (e.g. slope, ridge, saddle point; optional).}
#'    \item{\code{parentMaterial} - Underlying geological material (generally bedrock or a superficial or drift deposit) in which soil horizons form. (optional).}
#' }
#' @param measurementMethods A named list of objects of class \code{\linkS4class{VegXMethodDefinition}} with the measurement method
#' for each of the site variables stated in \code{mapping}. List names should be the same as the names of site variables
#' (e.g. \code{list(aspect = aspectMeth)} to specify the use of method \code{aspectMeth} for aspect measurements). Alternatively,
#' methods can be specified using strings if a predefined method exists (e.g. \code{list(aspect = "Aspect/degrees")}), 
#' see \code{\link{predefinedMeasurementMethod}}.
#' @param missing.values A character vector of values that should be considered as missing observations/measurements.
#' @param verbose A boolean flag to indicate console output of the data integration process.
#'
#' @return The modified object of class \code{\linkS4class{VegX}}.
#' @export
#'
#' @details Named elements in \code{mapping} other than those used by this function will be ignored. Missing value policy:
#'  \itemize{
#'     \item{Missing \code{plotName} values are interpreted as if the previous non-missing value has to be used to define plot.}
#'     \item{Missing \code{subPlotName} values are interpreted in that data refers to the parent plotName.}
#'     \item{Missing measurements (e.g. \code{aspect}, \code{slope},...) are simply not added to the Veg-X document.}
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
#' sitemapping = list(plotName = "Plot", subPlotName = "Subplot",
#'                    slope = "PlotSlope", aspect = "PlotAspect")
#'
#'
#' # Create new Veg-X document with site characteristics
#' x = addSiteCharacteristics(newVegX(), moki_site, mapping = sitemapping,
#'                            measurementMethods = list(slope = "Slope/degrees", 
#'                                                      aspect = "Aspect/degrees"))
#'
#' # Inspect the result
#' showElementTable(x, "plot")
#'
addSiteCharacteristics<-function(target, x,
                                 mapping,
                                 measurementMethods = list(),
                                 missing.values = c(NA,""),
                                 verbose = TRUE) {
  
  if(class(target)!="VegX") stop("Wrong class for 'target'. Should be an object of class 'VegX'")
  x = as.data.frame(x)
  nrecords = nrow(x)
  nmissing = 0


  #check mappings
  siteVariables = c("slope", "aspect", "landform", "parentMaterial")
  mappingsAvailable = c("plotName", "subPlotName", siteVariables)
  
  #Warning for non-recognized mappings
  nonRecognizedMappings = names(mapping)[!(names(mapping) %in% mappingsAvailable)]
  if(length(nonRecognizedMappings)>0) warning(paste0("Mapping(s) for '",paste(nonRecognizedMappings, collapse = "', '"),"' is/are not recognized by the function and will be ignored."))
  
  #Check columns exist
  for(i in 1:length(mapping)) {
    if(!(mapping[i] %in% names(x))) {
      if(names(mapping)[i] %in% mappingsAvailable) stop(paste0("Variable '", mapping[i],"' not found in column names. Revise mapping or data."))
    }
  }
  
  siteValues = list()
  for(i in 1:length(mapping)) {
    if(names(mapping)[i] %in% siteVariables) {
      if(!(names(mapping)[i] %in% names(measurementMethods))) stop(paste0("Measurement method should be provided corresponding to mapping '", names(mapping)[i], "'."))
      siteValues[[names(mapping)[i]]] = as.character(x[[mapping[[i]]]])
    }
  }

  plotNames = as.character(x[[mapping[["plotName"]]]])

  #Optional mappings
  subPlotFlag = ("subPlotName" %in% names(mapping))
  if(subPlotFlag) {
    subPlotNames = as.character(x[[mapping[["subPlotName"]]]])
  }

  #check methods for abiotic variables
  for(i in 1:length(measurementMethods)) {
    if(!(names(measurementMethods)[i] %in% siteVariables)) stop(paste0("Method for '", names(measurementMethods)[i], "' cannot be applied."))
    if(!(names(measurementMethods)[i] %in% names(mapping))) stop(paste0("Mapping should be defined corresponding to measurement method '", names(measurementMethods)[i], "'."))
  }



  #add methods
  methodIDs = character(0)
  methodCodes = list()
  methodAttIDs = list()
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
      methodCodes[[m]] = .getAttributeCodesByMethodID(target, methodID)
      methodAttIDs[[m]] = .getAttributeIDsByMethodID(target, methodID)
      if(verbose) cat(paste0(" Measurement method '", method@name,"' for '",m,"' already included.\n"))
    }
  }


  orinplots = length(target@plots)
  parsedPlots = character(0)
  parsedPlotIDs = character(0)
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
    #site characteristics
    for(m in names(measurementMethods)) {
      value = siteValues[[m]][i]
      method = measurementMethods[[m]]
      attIDs = methodAttIDs[[m]]
      codes = methodCodes[[m]]
      if(!(value %in% as.character(missing.values))) {
        if(m %in% c("slope", "aspect", "landform")) {
          if(!("topography" %in% names(target@plots[[plotID]]))) target@plots[[plotID]]$topography = list()
          if(method@attributeType== "quantitative") {
            value = as.numeric(value)
            if(value > method@attributes[[1]]$upperLimit) {
              stop(paste0("Value '", value,"' for '", m, "' larger than upper bound of measurement definition. Please revise scale or data."))
            }
            else if(value < method@attributes[[1]]$lowerLimit) {
              stop(paste0("Value '", value,"' for '", m, "' smaller than lower bound of measurement definition. Please revise scale or data."))
            }
            target@plots[[plotID]]$topography[[m]] = list("attributeID" = attIDs[[1]],
                                               "value" = value)
          } else {
            ind = which(codes==as.character(value))
            if(length(ind)==1) {
              target@plots[[plotID]]$topography[[m]] = list("attributeID" = attIDs[[ind]],
                                                 "value" = value)
            }
            else stop(paste0("Value '", value,"' for '", m, "' not found in measurement definition. Please revise classes or data."))
          }
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
