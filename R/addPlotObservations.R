#' Add plot observation records
#'
#' Adds plot observation records to a VegX object from a data table where rows are plot observations,
#' using a mapping to identify plot observation: plot, subplot (optional), observation start date and end date (optional).
#'
#' @param target The initial object of class \code{\linkS4class{VegX}} to be modified
#' @param x A data frame where each row corresponds to one plot observation. Columns can be varied.
#' @param mapping A list with element names 'plotName', 'obsStartDate', used to specify the mapping of data columns (specified using strings for column names) onto these variables.
#' Additional optional mappings are: 'projectTitle'  'obsEndDate', 'subPlotName'.
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
#' # Define mapping
#' mapping = list(projectTitle = "Project", plotName = "Plot", subPlotName = "Subplot",
#'                obsStartDate = "PlotObsStartDate", obsEndDate = "PlotObsStopDate")
#'
#' # Create a new Veg-X document with projects, plots and plot observations (no data)
#' x = addPlotObservations(newVegX(), site, mapping = mapping)
#'
#' # Examine the result
#' showElementTable(x, "plot")
#' showElementTable(x, "plotObservation")
#'
addPlotObservations<-function(target, x,
                              mapping,
                              missing.values = c(NA,""),
                              verbose = TRUE) {
  x = as.data.frame(x)
  nrecords = nrow(x)
  nmissing = 0



  #check mappings
  plotObservationMappingsAvailable = c("projectTitle", "plotName", "obsStartDate", "obsEndDate", "subPlotName")
  for(i in 1:length(mapping)) {
    if(!(names(mapping)[i] %in% plotObservationMappingsAvailable)) stop(paste0("Mapping for '", names(plotObservationMapping)[i], "' cannot be defined."))
  }
  #Check columns exist
  for(i in 1:length(mapping)) {
    if(!(mapping[i] %in% names(x))) stop(paste0("Variable '", mapping[i],"' not found in column names. Revise mapping or data."))
  }
  plotNames = as.character(x[[mapping[["plotName"]]]])
  obsStartDates = as.Date(as.character(x[[mapping[["obsStartDate"]]]]))

  #Optional mappings
  projectFlag = ("projectTitle" %in% names(mapping))
  if(projectFlag) {
    projectTitles = as.character(x[[mapping[["projectTitle"]]]])
  }
  obsEndFlag = ("obsEndDate" %in% names(mapping))
  if(obsEndFlag) {
    obsEndDates = as.Date(as.character(x[[mapping[["obsEndDate"]]]]))
  }
  subPlotFlag = ("subPlotName" %in% names(mapping))
  if(subPlotFlag) {
    subPlotNames = as.character(x[[mapping[["subPlotName"]]]])
  }


  orinprojects = length(target@projects)
  orinplots = length(target@plots)
  orinplotobs = length(target@plotObservations)
  parsedProjects = character(0)
  parsedProjectIDs = character(0)
  parsedPlots = character(0)
  parsedPlotIDs = character(0)
  parsedPlotObs = character(0)
  parsedPlotObsIDs = character(0)
  #Record parsing loop
  for(i in 1:nrecords) {

    #project
    if(projectFlag) {
      if(!(projectTitles[i] %in% missing.values)) { # If projectTitle is missing take the previous one
        if(!(projectTitles[i] %in% parsedProjects)) {
          nprid = .newProjectIDByTitle(target, projectTitles[i]) # Get the new project ID (internal code)
          projectID = nprid$id
          if(nprid$new) target@projects[[projectID]] = list("title" = projectTitles[i])
          parsedProjects = c(parsedProjects, projectTitles[i])
          parsedProjectIDs = c(parsedProjectIDs, projectID)
        } else { #this access should be faster
          projectID = parsedProjectIDs[which(parsedProjects==projectTitles[i])]
        }
      }
    }

    #plot
    if(!(plotNames[i] %in% missing.values)) {# If plotName is missing take the previous one
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
      if(!(subPlotNames[i] %in% missing.values)) {# If subPlotName is missing take the previous one
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
    if(!(obsStartDates[i] %in% missing.values)) {# If observation date is missing take the previous one
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
    }
  }
  finnprojects = length(target@projects)
  finnplots = length(target@plots)
  finnplotobs = length(target@plotObservations)
  finnabioobs = length(target@siteObservations)
  if(verbose) {
    cat(paste0(" " , length(parsedProjects)," project(s) parsed, ", finnprojects-orinprojects, " new project(s) added.\n"))
    cat(paste0(" " , length(parsedPlots)," plot(s) parsed, ", finnplots-orinplots, " new plot(s) added.\n"))
    cat(paste0(" " , length(parsedPlotObs)," plot observation(s) parsed, ", finnplotobs-orinplotobs, " new plot observation(s) added.\n"))
  }

  return(target)
}
