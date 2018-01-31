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
#' @references Wiser SK, Spencer N, De Caceres M, Kleikamp M, Boyle B & Peet RK (2011). Veg-X - an exchange standard for plot-based vegetation data
#'
#' @family add functions
#'
#' @examples
#' data(mokihinui)
#'
#' # Define location mapping
#' mapping = list(plotName = "Plot", subPlotName = "Subplot",
#'                area = "PlotArea", shape = "Shape",
#'                width = "PlotRectangleLength01", length = "PlotRectangleLength02")
#'
#' # Create new Veg-X document with plot locations
#' x = addPlotGeometries(newVegX(), moki_site, mapping)
#'
#' # Summary of the new Veg-X document
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


  #check mappings
  nonCoordVariables = c("authorLocation","locationNarrative", "placeName", "placeType")
  locVariables = c("x", "y", nonCoordVariables)
  mappingsAvailable = c("plotName", "subPlotName", locVariables)
  locValues = list()
  for(i in 1:length(mapping)) {
    if(!(names(mapping)[i] %in% mappingsAvailable)) stop(paste0("Mapping for '", names(mapping)[i], "' cannot be defined."))
    if(names(mapping)[i] %in% locVariables) {
      locValues[[names(mapping)[i]]] = as.character(x[[mapping[[i]]]])
    }
  }
  if(("y" %in% names(mapping)) && !("x" %in% names(mapping))) stop("Please supply mapping for 'x' to complete coordinates.")
  if(("x" %in% names(mapping)) && !("y" %in% names(mapping))) stop("Please supply mapping for 'y' to complete coordinates.")

  #Check columns exist
  for(i in 1:length(mapping)) {
    if(!(mapping[i] %in% names(x))) stop(paste0("Variable '", mapping[i],"' not found in column names. Revise mapping or data."))
  }
  plotNames = as.character(x[[mapping[["plotName"]]]])

  #Optional mappings
  subPlotFlag = ("subPlotName" %in% names(mapping))
  if(subPlotFlag) {
    subPlotNames = as.character(x[[mapping[["subPlotName"]]]])
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
    #Add 'location' element if necessary
    if(!("location" %in% names(target@plots[[plotID]]))) target@plots[[plotID]]$location = list()

    # Reset 'places' element if necessary
    if(reset.places && ("places" %in% names(target@plots[[plotID]]$location))) target@plots[[plotID]]$location$places = list()

    # Add new location if necessary
    if(("placeName" %in% names(mapping)) || ("placeType" %in% names(mapping))) {
      #Add 'places' element if necessary
      if(!("places" %in% names(target@plots[[plotID]]$location))) target@plots[[plotID]]$location$places = list()
      newloc = paste0(location, length(target@plots[[plotID]]$location$places)+1)
      target@plots[[plotID]]$location$places[[newloc]] = list()
    }

    #Add plot location data (non coordinate variables)
    for(m in names(mapping)[names(mapping) %in% nonCoordVariables]) {
      value = locValues[[m]][i]
      if(!(value %in% as.character(missing.values))) {
        if((m=="placeName") || (m=="placeType")){ #Add placeName/placeType to location
          target@plots[[plotID]]$location$places[[newloc]][[m]] = value
        } else {
          target@plots[[plotID]]$location[[m]] = value
        }
      } else {
        nmissing = nmissing + 1
      }
    }

    # Add coordinate variables (transform to latlong)
    if("x" %in% names(mapping)) {
      x = as.numeric(locValues[["x"]][i])
      y = as.numeric(locValues[["y"]][i])
      if((!is.na(x))&& (!is.na(y))) {
        sp = SpatialPoints(coords = matrix(c(x,y), nrow=1, ncol=2), proj4string = CRS(proj4string))
        splatlon = spTransform(sp, CRS("+proj=longlat +datum=WGS84"))
        target@plots[[plotID]]$location$DecimalLongitude = as.numeric(splatlon@coords[1,1])
        target@plots[[plotID]]$location$DecimalLatitude = as.numeric(splatlon@coords[1,2])
        target@plots[[plotID]]$location$GeodeticDatum = "WGS84"
      } else {
        nmissing = nmissing + 1
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
