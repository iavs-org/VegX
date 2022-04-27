#' Adds/replaces plot location information
#'
#' Adds/replaces static plot location information (spatial coordinates, elevation, place names, ...) to plot elements of a VegX object from a data table where rows are plots.
#'
#' @param target The initial object of class \code{\linkS4class{VegX}} to be modified
#' @param x A data frame where each row corresponds to one plot. Columns can be varied.
#' @param mapping A named list whose elements are strings that correspond to column names in \code{x}. Names of the list should be:
#' \itemize{
#'    \item{\code{plotName} - A string identifying the vegetation plot within the data set (required).}
#'    \item{\code{subPlotName} - A string identifying a subplot of the plot given by \code{plotName} (optional).}
#'    \item{\code{x}, \code{y} - Spatial coordinates of the plot (optional).}
#'    \item{\code{elevation} - Elevation of the plot (optional).}
#'    \item{\code{authorLocation} - A string describing the location of the plot as made by the author (optional).}
#'    \item{\code{locationNarrative} -  (optional).}
#'    \item{\code{placeName}, \code{placeType} - A string of a place name and place type (e.g. province, county, ...) (optional).}
#' }
#' Note that \code{placeName} and \code{placeType} will add new places to the list of places.
#' @param proj4string A string with projection attributes (see \code{\link{proj4string}} of package \code{sp}) to be used when 'x' and 'y' are supplied. 
#' This parameter is needed if \code{toWGS84 = TRUE}.
#' @param reset.places Whether the 'places' vector should be reset before adding new place names.
#' @param toWGS84 A boolean flag to indicate that coordinates should be transformed to "+proj=longlat +datum=WGS84".
#' @param methods A named list with measurement methods for plot horizontal/vertical location measurements (each being an object of class \code{\linkS4class{VegXMethodDefinition}}). 
#' Alternatively, methods can be specified using strings if predefined methods exist (see \code{\link{predefinedMeasurementMethod}}).
#' For example, \code{methods = c(xy = method1, elevation = method2)}. Measurement method for coordinates is not required, but that for 'elevation' is.
#' @param missing.values A character vector of values that should be considered as missing data (but see the following).
#' @param missing.coords A character vector of values that should be considered as missing coordinates (introduced to allow separate treatment).
#' @param missing.elevation A character vector of values that should be considered as missing elevation (introduced to allow separate treatment).
#' @param verbose A boolean flag to indicate console output of the data integration process.
#'
#' @return The modified object of class \code{\linkS4class{VegX}}.
#'
#' @details Named elements in \code{mapping} other than those used by this function will be ignored. Missing value policy:
#'  \itemize{
#'     \item{Missing \code{plotName} values are interpreted as if the previous non-missing value has to be used to define plot.}
#'     \item{Missing \code{subPlotName} values are interpreted in that data refers to the parent plotName.}
#'     \item{Missing measurements (e.g. \code{elevation}, \code{x}, \code{y}, ...) are simply not added to the Veg-X document.}
#'  }
#'  
#' @references Wiser SK, Spencer N, De Caceres M, Kleikamp M, Boyle B & Peet RK (2011). Veg-X - an exchange standard for plot-based vegetation data
#'
#' @family add functions
#' 
#' @importFrom sp SpatialPoints spTransform CRS

#'
#' @examples
#' data(mokihinui)
#'
#' # Define location mapping
#' mapping = list(plotName = "Plot", x = "Longitude", y = "Latitude")
#'
#' # Create new Veg-X document with plot locations
#' x = addPlotLocations(newVegX(), moki_loc, mapping,
#'                      proj4string = "+proj=longlat +datum=WGS84")
#'
#' # Summary of the new Veg-X document
#' showElementTable(x, "plot")
#' 
#' # Add 'elevation' from another table (moki_site). This implies considering subplots.
#' mapping = list(plotName = "Plot", subPlotName = "Subplot", elevation = "Altitude")
#' x = addPlotLocations(x, moki_site, mapping, 
#'                      methods = list(elevation = "Elevation/m"))
#'                      
#' # Summary of the updated Veg-X document
#' showElementTable(x, "plot")
#' 
addPlotLocations<-function(target, x,
                           mapping,
                           proj4string = NULL,
                           reset.places = FALSE,
                           toWGS84 = FALSE,
                           methods = list(),
                           missing.values = c(NA,""),
                           missing.coords = c(NA, 0, ""),
                           missing.elevation = c(NA, 0, ""),
                           verbose = TRUE) {
  if(class(target)!="VegX") stop("Wrong class for 'target'. Should be an object of class 'VegX'")
  x = as.data.frame(x)
  nrecords = nrow(x)
  nmissing = 0


  #check mappings
  nonCoordVariables = c("authorLocation","locationNarrative", "placeName", "placeType")
  coordVariables = c("x", "y", "elevation")
  allVariables = c(coordVariables, nonCoordVariables)
  mappingsAvailable = c("plotName", "subPlotName", "placementParty", allVariables)
  if(("y" %in% names(mapping)) && !("x" %in% names(mapping))) stop("Please supply mapping for 'x' to complete coordinates.")
  if(("x" %in% names(mapping)) && !("y" %in% names(mapping))) stop("Please supply mapping for 'y' to complete coordinates.")

  #Warning for non-recognized mappings
  nonRecognizedMappings = names(mapping)[!(names(mapping) %in% mappingsAvailable)]
  if(length(nonRecognizedMappings)>0) warning(paste0("Mapping(s) for '",paste(nonRecognizedMappings, collapse = "', '"),"' is/are not recognized by the function and will be ignored."))
  
  #Check columns exist
  for(i in 1:length(mapping)) {
    if(!(mapping[i] %in% names(x))) {
      if(names(mapping)[i] %in% mappingsAvailable) stop(paste0("Variable '", mapping[i],"' for '", names(mapping)[i], "' not found in column names. Revise mapping or data.")) 
    }
  }
  
  locValues = list()
  for(i in 1:length(mapping)) {
    if(names(mapping)[i] %in% allVariables) {
      locValues[[names(mapping)[i]]] = as.character(x[[mapping[[i]]]])
    }
  }
  
  
  plotNames = as.character(x[[mapping[["plotName"]]]])

  #Optional mappings
  subPlotFlag = ("subPlotName" %in% names(mapping))
  if(subPlotFlag) {
    subPlotNames = as.character(x[[mapping[["subPlotName"]]]])
  }
  placementPartyFlag = ("placementParty" %in% names(mapping))
  if(placementPartyFlag) {
    placementParties = as.character(x[[mapping[["placementParty"]]]])
  }

  if(("y" %in% names(mapping)) && ("x" %in% names(mapping))) {
    if(toWGS84 && is.null(proj4string)) {
      stop("Cannot translate input coordinates to WGS84 if 'proj4string' is not specified.")
    }
  }
  
  #add methods
  methodIDs = character(0)
  methodCodes = list()
  methodAttIDs = list()
  for(m in names(methods)) {
    method = methods[[m]]
    if(class(method)=="character") {
      method = predefinedMeasurementMethod(method)
      methods[[m]] = method
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
  orinparties = length(target@parties)
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
      if(npid$new) target@plots[[plotID]] = list("plotName" = plotName)
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
    #placementParty
    if(placementPartyFlag) {
      if(!(placementParties[i] %in% missing.values)) {
        npid = .newPartyIDByName(target, placementParties[i])
        partyID = npid$id
        if(npid$new) target@parties[[partyID]] = list(name = placementParties[i],
                                                      partyType = "individual")

        target@plots[[plotID]]$placementPartyID = partyID
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
      newloc = length(target@plots[[plotID]]$location$places)+1
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
    # Add coordinate variables (if required transform to latlong)
    if("elevation" %in% names(mapping)) {
      m = "elevation"
      method = methods[[m]]
      attIDs = methodAttIDs[[m]]
      codes = methodCodes[[m]]
      value = as.character(locValues[[m]][i])
      if(!(value %in% as.character(missing.elevation))) {
        if(method@attributeType== "quantitative") {
          value = as.numeric(value)
          if(value> method@attributes[[1]]$upperLimit) {
            stop(paste0("Elevation '", value,"' larger than upper limit of measurement definition. Please revise scale or data."))
          }
          else if(value < method@attributes[[1]]$lowerLimit) {
            stop(paste0("Elevation '", value,"' smaller than lower limit of measurement definition. Please revise scale or data."))
          }
          target@plots[[plotID]]$location$verticalCoordinates$elevation = list("attributeID" = attIDs[1], "value" = value)
        } else {
          ind = which(codes==value)
          if(length(ind)==1) {
            target@plots[[plotID]]$location$verticalCoordinates$elevation = list("attributeID" = attIDs[ind], "value" = value)
          }
          else stop(paste0("Value '", value,"' not found in area measurement definition. Please revise elevation classes or data."))
        }
      } else {
        nmissing = nmissing + 1
      }
      
    }
    # Add coordinate variables (if required transform to latlong)
    if(("x" %in% names(mapping)) && ("y" %in% names(mapping))) {
      attIDs = NA
      if("xy" %in% names(methods)) {
        m = "xy"
        method = methods[[m]]
        attIDs = methodAttIDs[[m]]
        codes = methodCodes[[m]]
      }
           
      if(locValues[["x"]][i] %in% missing.coords) {
        locValues[["x"]][i] <- NA
        }
      if(locValues[["y"]][i] %in% missing.coords) {
        locValues[["y"]][i] <- NA
        }       
      
      x = as.numeric(locValues[["x"]][i])
      y = as.numeric(locValues[["y"]][i])
      if((!is.na(x)) && (!is.na(y))) {
        if(toWGS84) {
          sp = SpatialPoints(coords = matrix(c(x,y), nrow=1, ncol=2), proj4string = CRS(proj4string))
          sp = spTransform(sp, CRS("+proj=longlat +datum=WGS84"))
          x = sp@coords[1,1]
          y = sp@coords[1,2]
          proj4string = "+proj=longlat +datum=WGS84"
        }
        target@plots[[plotID]]$location$horizontalCoordinates$coordinates$valueX = as.numeric(x)
        target@plots[[plotID]]$location$horizontalCoordinates$coordinates$valueY = as.numeric(y)
        if(!is.null(proj4string)) target@plots[[plotID]]$location$horizontalCoordinates$coordinates$spatialReference = proj4string
        if(!is.na(attIDs)) target@plots[[plotID]]$location$horizontalCoordinates$coordinates$attributeID = attIDs[1]
      } else {
        nmissing = nmissing + 1
      }
    }
  }
  finnplots = length(target@plots)
  finnparties = length(target@parties)

  if(verbose) {
    cat(paste0(" " , length(parsedPlots)," plot(s) parsed, ", finnplots-orinplots, " new plot(s) added.\n"))
    if(finnparties > orinparties) cat(paste0(" " , finnparties-orinparties, " new partie(s) were added to the document as individuals. Consider providing party information.\n"))
    cat(paste0(" ", nrecords," record(s) parsed.\n"))
    if(nmissing>0) cat(paste0(" ", nmissing, " record(s) with missing value(s) not added.\n"))
  }

  return(target)
}
