#' Adds/replaces plot geometry information
#'
#' Adds/replaces static plot geometry information (plot shape, dimensions, ...) to plot elements of a VegX object from a data table where rows are plots,
#' using a mapping to identify plot and subplot (optional). Additional mapping elements are used to map specific variables.
#'
#' @param target The initial object of class \code{\linkS4class{VegX}} to be modified.
#' @param x A data frame where each row corresponds to one plot. Columns can be varied.
#' @param mapping A list with at least element name 'plotName', is used to specify the mapping of data columns (specified using strings for column names) onto these variables.
#' Geometry variables that can be mapped are: 'area', 'shape', 'radius', 'length','width', 'bandWidth'.
#' Additional optional mappings are: 'subPlotName'.
#' @param methods A list measurement methods for plot geometry measurements (each being an object of class \code{\linkS4class{VegXMethod}}).
#' @param missing.values A character vector of values that should be considered as missing data.
#' @param verbose A boolean flag to indicate console output of the data integration process.
#'
#' @return The modified object of class \code{\linkS4class{VegX}}.
#' @export
#'
#' @details Missing 'plotName' and 'shape' values are interpreted as if the previous non-missing value has to be used to define plot.
#' Missing subPlotName values are interpreted in that data refers to the parent plotName.
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


  #check mappings and extract variables
  #Check columns exist
  for(i in 1:length(mapping)) {
    if(!(mapping[i] %in% names(x))) stop(paste0("Variable '", mapping[i],"' not found in column names. Revise mapping or data."))
  }

  circleVariables = c("radius")
  rectangleVariables = c("length", "width")
  lineVariables = c("length", "bandWidth")

  geometryVariables = unique(c("area", circleVariables, rectangleVariables, lineVariables))
  mappingsAvailable = c("plotName", "subPlotName", "shape", geometryVariables)
  geometryValues = list()
  for(i in 1:length(mapping)) {
    if(!(names(mapping)[i] %in% mappingsAvailable)) stop(paste0("Mapping for '", names(mapping)[i], "' cannot be defined."))
    if(names(mapping)[i] %in% geometryVariables) {
      if(!(names(mapping)[i] %in% names(methods)))  stop(paste0("Method needs to be provided to map values of '", names(mapping)[i], "'."))
      geometryValues[[names(mapping)[i]]] = as.character(x[[mapping[[i]]]])
    }
  }


  plotNames = as.character(x[[mapping[["plotName"]]]])

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
      if(!shapes[i] %in% missing.values) {# If shape is missing take the previous one
        shape = shapes[i]
      }
      if(tolower(shape) %in% c("circle", "circular")) {
        circle = list()
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
        rectangle = list()
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
        line = list()
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
  finnplots = length(target@plots)
  if(verbose) {
    cat(paste0(" " , length(parsedPlots)," plot(s) parsed, ", finnplots-orinplots, " new plot(s) added.\n"))
    cat(paste0(" ", nrecords," record(s) parsed.\n"))
    if(nmissing>0) cat(paste0(" ", nmissing, " record(s) with missing value(s) not added.\n"))
  }

  return(target)
}
