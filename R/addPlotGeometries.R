#' Adds/replaces plot geometry information
#'
#' Adds/replaces static plot geometry information (plot shape, dimensions, ...) to plot elements of a VegX object from a data frame where rows are plots.
#'
#' @param target The initial object of class \code{\linkS4class{VegX}} to be modified.
#' @param x A data frame where each row corresponds to one plot. Columns can be varied.
#' @param mapping A named list whose elements are strings that correspond to column names in \code{x}. Names of the list should be:
#' #' \itemize{
#'    \item{\code{plotName} - A string identifying the vegetation plot within the data set (required).}
#'    \item{\code{subPlotName} - A string identifying a subplot of the plot given by \code{plotName} (optional).}
#'    \item{\code{area} - Area of the plot (optional).}
#'    \item{\code{shape} - Shape of the plot: circle/circular, rectangle/rectangular/squared, line/linear (optional).}
#'    \item{\code{radius} - Radius of the plot, for circular plots (optional).}
#'    \item{\code{width} -  Width of the plot, for rectangular plots (optional).}
#'    \item{\code{length} -  Length of the plot, for linear and rectangular plots (optional).}
#'    \item{\code{bandWidth} - Sampling bandwidth, for linear plots (optional).}
#' }
#' @param methods A list measurement methods for plot geometry measurements (each being an object of class \code{\linkS4class{VegXMethodDefinition}}).
#' @param missing.values A character vector of values that should be considered as missing data.
#' @param verbose A boolean flag to indicate console output of the data integration process.
#'
#' @return The modified object of class \code{\linkS4class{VegX}}.
#' @export
#'
#' @details Named elements in \code{mapping} beyond those used by this function will be ignored. Missing value policy:
#'  \itemize{
#'     \item{Missing \code{plotName} and \code{shape} values are interpreted as if the previous non-missing value has to be used to define plot.}
#'     \item{Missing \code{subPlotName} values are interpreted in that data refers to the parent plotName.}
#'     \item{Missing measurements (e.g. \code{area}, \code{radius},...) are simply not added to the Veg-X document.}
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
#' # Define location mapping
#' mapping = list(plotName = "Plot", subPlotName = "Subplot",
#'                area = "PlotArea", shape = "Shape",
#'                width = "PlotRectangleLength01", length = "PlotRectangleLength02")
#'
#' # Define methods
#' areaMethod = predefinedMeasurementMethod("Plot area/m2")
#' dimensionMethod = predefinedMeasurementMethod("Plot dimension/m")
#'
#' # Create new Veg-X document with plot locations
#' x = addPlotGeometries(newVegX(), moki_site, mapping,
#'                       list(area = areaMethod, width = dimensionMethod, length = dimensionMethod))
#'
#' # Inspect results
#' showElementTable(x, "plot")
#'
addPlotGeometries<-function(target, x,
                            mapping,
                            methods = list(),
                            missing.values = c(NA,""),
                            verbose = TRUE) {
  x = as.data.frame(x)
  nrecords = nrow(x)
  nmissing = 0

  circleVariables = c("radius")
  rectangleVariables = c("length", "width")
  lineVariables = c("length", "bandWidth")
  
  geometryVariables = unique(c("area", circleVariables, rectangleVariables, lineVariables))
  mappingsAvailable = c("plotName", "subPlotName", "shape", geometryVariables)
  geometryValues = list()
  
  #Check columns exist
  for(i in 1:length(mapping)) {
    if(!(mapping[i] %in% names(x))) { 
      if(names(mapping)[i] %in% mappingsAvailable) stop(paste0("Variable '", mapping[i],"' not found in column names. Revise mapping or data."))
    }
  }
  
  #Warning for non-recognized mappings
  nonRecognizedMappings = names(mapping)[!(names(mapping) %in% mappingsAvailable)]
  if(length(nonRecognizedMappings)>0) warning(paste0("Mapping(s) for '",paste(nonRecognizedMappings, collapse = "', '"),"' is/are not recognized by the function will be ignored."))
  
  #Read values
  plotNames = as.character(x[[mapping[["plotName"]]]])
  for(i in 1:length(mapping)) {
    if(names(mapping)[i] %in% geometryVariables) {
      if(!(names(mapping)[i] %in% names(methods)))  stop(paste0("Method needs to be provided to map values of '", names(mapping)[i], "'."))
      geometryValues[[names(mapping)[i]]] = as.character(x[[mapping[[i]]]])
    }
  }
  


  #Optional mappings
  subPlotFlag = ("subPlotName" %in% names(mapping))
  if(subPlotFlag) {
    subPlotNames = as.character(x[[mapping[["subPlotName"]]]])
  }
  shapeFlag = ("shape" %in% names(mapping))
  if(subPlotFlag) {
    shapes = as.character(x[[mapping[["shape"]]]])
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
  parsedPlots = character(0)
  parsedPlotIDs = character(0)
  #Record parsing loop
  for(i in 1:nrecords) {
    #plot
    if(!plotNames[i] %in% missing.values) {# If plotName is missing take the previous one
      plotName = plotNames[i]
    }
    if(!(plotName %in% parsedPlots)) {
      npid = .newPlotIDByName(target, plotName) # Get the new plot ID (internal code)
      plotID = npid$id
      if(npid$new) target@plots[[plotID]] = list("plotName" = plotNames[i])
      parsedPlots = c(parsedPlots, plotName)
      parsedPlotIDs = c(parsedPlotIDs, plotID)
    } else { #this access should be faster
      plotID = parsedPlotIDs[which(parsedPlots==plotName)]
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
    #Add 'geometry' element if necessary
    if(!("geometry" %in% names(target@plots[[plotID]]))) target@plots[[plotID]]$geometry = list()

    # area measurements
    if("area" %in% names(mapping)) {
      m = "area"
      method = methods[[m]]
      attIDs = methodAttIDs[[m]]
      codes = methodCodes[[m]]
      value = as.character(geometryValues[[m]][i])
      if(!(value %in% as.character(missing.values))) {
        if(method@attributeType== "quantitative") {
          value = as.numeric(value)
          if(value> method@attributes[[1]]$upperLimit) {
            stop(paste0("Area '", value,"' larger than upper limit of measurement definition. Please revise scale or data."))
          }
          else if(value < method@attributes[[1]]$lowerLimit) {
            stop(paste0("Area '", value,"' smaller than lower limit of measurement definition. Please revise scale or data."))
          }
          target@plots[[plotID]]$geometry[[m]] = list("attributeID" = attIDs[1], "value" = value)
        } else {
          ind = which(codes==value)
          if(length(ind)==1) {
            target@plots[[plotID]]$geometry[[m]] = list("attributeID" = attIDs[ind], "value" = value)
          }
          else stop(paste0("Value '", value,"' not found in area measurement definition. Please revise area classes or data."))
        }
      } else {
        nmissing = nmissing + 1
      }
    }

    if(shapeFlag) {
      #shape
      shape = NA
      if(!shapes[i] %in% missing.values) {# If shape is missing take the previous one
        shape = shapes[i]
      }
      if(!is.na(shape)) {
        if(tolower(shape) %in% c("circle", "circular")) {
          if(!("circle" %in% names(target@plots[[plotID]]$geometry))) circle = list()
          else circle = target@plots[[plotID]]$geometry$circle
          for(m in circleVariables) {
            if(m %in% names(mapping)) {
              method = methods[[m]]
              attIDs = methodAttIDs[[m]]
              codes = methodCodes[[m]]
              value = as.character(geometryValues[[m]][i])
              if(!(value %in% as.character(missing.values))) {
                if(method@attributeType== "quantitative") {
                  value = as.numeric(value)
                  if(value> method@attributes[[1]]$upperLimit) {
                    stop(paste0("Value '", value,"' larger than upper limit of measurement definition. Please revise scale or data."))
                  }
                  else if(value < method@attributes[[1]]$lowerLimit) {
                    stop(paste0("Value '", value,"' smaller than lower limit of measurement definition. Please revise scale or data."))
                  }
                  circle[[m]] = list("attributeID" = attIDs[1], "value" = value)
                } else {
                  ind = which(codes==value)
                  if(length(ind)==1) {
                    circle[[m]] = list("attributeID" = attIDs[ind], "value" = value)
                  }
                  else stop(paste0("Value '", value,"' not found in dimension measurement definition. Please revise measurement classes or data."))
                }
              } else {
                nmissing = nmissing + 1
              }
            }
          }
          target@plots[[plotID]]$geometry$circle = circle
        }
        else if(tolower(shape) %in% c("rectangle", "rectangular", "square", "squared")) {
          if(!("rectangle" %in% names(target@plots[[plotID]]$geometry))) rectangle = list()
          else rectangle = target@plots[[plotID]]$geometry$rectangle
          for(m in rectangleVariables) {
            if(m %in% names(mapping)) {
              method = methods[[m]]
              attIDs = methodAttIDs[[m]]
              codes = methodCodes[[m]]
              value = as.character(geometryValues[[m]][i])
              if(!(value %in% as.character(missing.values))) {
                if(method@attributeType== "quantitative") {
                  value = as.numeric(value)
                  if(value> method@attributes[[1]]$upperLimit) {
                    stop(paste0("Value '", value,"' larger than upper limit of measurement definition. Please revise scale or data."))
                  }
                  else if(value < method@attributes[[1]]$lowerLimit) {
                    stop(paste0("Value '", value,"' smaller than lower limit of measurement definition. Please revise scale or data."))
                  }
                  rectangle[[m]] = list("attributeID" = attIDs[1], "value" = value)
                } else {
                  ind = which(codes==value)
                  if(length(ind)==1) {
                    rectangle[[m]] = list("attributeID" = attIDs[ind], "value" = value)
                  }
                  else stop(paste0("Value '", value,"' not found in dimension measurement definition. Please revise measurement classes or data."))
                }
              } else {
                nmissing = nmissing + 1
              }
            }
          }
          target@plots[[plotID]]$geometry$rectangle = rectangle
        }
        else if(tolower(shape) %in% c("line", "linear")) {
          if(!("line" %in% names(target@plots[[plotID]]$line))) line = list()
          else line = target@plots[[plotID]]$geometry$line
          for(m in lineVariables) {
            if(m %in% names(mapping)) {
              method = methods[[m]]
              attIDs = methodAttIDs[[m]]
              codes = methodCodes[[m]]
              value = as.character(geometryValues[[m]][i])
              if(!(value %in% as.character(missing.values))) {
                if(method@attributeType== "quantitative") {
                  value = as.numeric(value)
                  if(value> method@attributes[[1]]$upperLimit) {
                    stop(paste0("Value '", value,"' larger than upper limit of measurement definition. Please revise scale or data."))
                  }
                  else if(value < method@attributes[[1]]$lowerLimit) {
                    stop(paste0("Value '", value,"' smaller than lower limit of measurement definition. Please revise scale or data."))
                  }
                  line[[m]] = list("attributeID" = attIDs[1], "value" = value)
                } else {
                  ind = which(codes==value)
                  if(length(ind)==1) {
                    line[[m]] = list("attributeID" = attIDs[ind], "value" = value)
                  }
                  else stop(paste0("Value '", value,"' not found in dimension measurement definition. Please revise measurement classes or data."))
                }
              } else {
                nmissing = nmissing + 1
              }
            }
          }

          target@plots[[plotID]]$geometry$line = line
        }
      }
    }
  }
  finnplots = length(target@plots)
  if(verbose) {
    cat(paste0(" " , length(parsedPlots)," plot(s) parsed, ", finnplots-orinplots, " new plot(s) added.\n"))
    cat(paste0(" ", nrecords," record(s) parsed.\n"))
    if(nmissing>0) cat(paste0(" ", nmissing, " record(s) with missing value(s) not added.\n"))
  }

  return(target)
}
