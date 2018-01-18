#' Adds plot location information
#'
#' Adds/replaces static plot location information (spatial coordinates, place names, ...) to plot elements of a VegX object from a data table where rows are plots,
#' using a mapping to identify plot and subplot (optional). Additional mapping elements are used to map specific variables.
#'
#' @param target The initial object of class \code{\linkS4class{VegX}} to be modified
#' @param points An object of class \code{\link{SpatialPoints}} with coordinates in a reference system.
#' @param x A data frame where each row corresponds to one plot observation. Columns can be varied.
#' @param projectTitle A string to identify the project title, which can be the same as one of the currently defined in \code{target}.
#' @param mapping A list with at least element name 'plotName', is used to specify the mapping of data columns (specified using strings for column names) onto these variables.
#' Site variables that can be mapped are: 'authorLocation','locationNarrative', 'placeName', 'placeType'.
#' Additional optional mappings are: 'subPlotName'. Note that 'placeName' and 'placeType' will add new places to the list of locations
#' @param reset.locations Whether the 'locations' vector (a vector of place names) should be reset before adding new place names.
#' @param missing.values A character vector of values that should be considered as missing data.
#' @param verbose A boolean flag to indicate console output of the data integration process.
#'
#' @return The modified object of class \code{\linkS4class{VegX}}.
#' @export
#'
#' @references Wiser SK, Spencer N, De Caceres M, Kleikamp M, Boyle B & Peet RK (2011). Veg-X - an exchange standard for plot-based vegetation data
#'
#' @seealso \code{\link{addSiteCharacteristics}}.
#'
#' @examples
addPlotLocations<-function(target, points, x, projectTitle,
                           mapping,
                           reset.locations = FALSE,
                           missing.values = c(NA,""),
                           verbose = TRUE) {
  x = as.data.frame(x)
  nrecords = nrow(x)
  nmissing = 0


  #check mappings
  siteVariables = c("authorLocation","slope", "aspect", "landform", "parentMaterial", "geologyClass")
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
    #Add 'plotLocation' element if necessary
    if(!("plotLocation" %in% names(target@plots[[plotID]]))) target@plots[[plotID]]["plotLocation"] = list()

    # Reset 'locations' element if necessary
    if(reset.locations && ("locations" %in% names(target@plots[[plotID]]$plotLocation))) target@plots[[plotID]]$plotLocation$locations = list()

    # Add new location if necessary
    if(("placeName" %in% names(mapping)) || ("placeType" %in% names(mapping))) {
      #Add 'locations' element if necessary
      if(!("locations" %in% names(target@plots[[plotID]]$plotLocation))) target@plots[[plotID]]$plotLocation$locations = list()
      newloc = paste0(location, length(target@plots[[plotID]]$plotLocation$locations)+1)
      target@plots[[plotID]]$plotLocation$locations[[newloc]] = list()
    }

    #Add plot location data
    for(m in names(mapping)) {
      value = siteValues[[m]][i]
      if(!(value %in% as.character(missing.values))) {
        if((m=="placeName") || (m=="placeType")){ #Add placeName/placeType to location
          target@plots[[plotID]]$plotLocation$locations[[newloc]][[m]] = value
        } else {
          target@plots[[plotID]]$plotLocation[[m]] = value
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
    if(nmissing>0) cat(paste0(" ", nmissing, " record(s) with missing value(s) not added.\n"))
  }

  return(target)
}
