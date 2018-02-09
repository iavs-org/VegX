#' Add plot observation records
#'
#' Adds plot observation records to a VegX object from a data table where rows are plot observations.
#'
#' @param target The initial object of class \code{\linkS4class{VegX}} to be modified
#' @param x A data frame where each row corresponds to one plot observation. Columns can be varied.
#' @param mapping A named list whose elements are strings that correspond to column names in \code{x}. Names of the list should be:
#'  \itemize{
#'    \item{\code{plotName} - A string identifying the vegetation plot within the data set (required).}
#'    \item{\code{subPlotName} - A string identifying a subplot of the plot given by \code{plotName} (optional).}
#'    \item{\code{obsStartDate} - Plot observation start date (required; see \code{date.format}).}
#'    \item{\code{projectTitle} - Title of the project related to the plot observation (optional).}
#'    \item{\code{obsEndDate} - Plot observation end date (optional; see  \code{date.format}).}
#'    \item{\code{observationParty} - Name of the party that undertook plot observation (optional).}
#'    \item{\code{plotUniqueIdentifier} - A string used to identify the plot uniquely, preferably globally unique (optional).}
#' }
#' @param date.format A character string specifying the input format of dates (see \code{\link{as.Date}}).
#' @param missing.values A character vector of values that should be considered as missing data (see details).
#' @param verbose A boolean flag to indicate console output of the data integration process.
#'
#' @return The modified object of class \code{\linkS4class{VegX}}.
#' @export
#'
#' @details Named elements in \code{mapping} beyond those used by this function will be ignored. Missing value policy:
#'  \itemize{
#'     \item{Missing \code{projectTitle}, \code{plotName}, \code{obsStartDate} or \code{obsEndDate} values are interpreted as if the previous non-missing value has to be used to define plot observation.}
#'     \item{Missing \code{subPlotName} values are interpreted in that data refers to the parent plotName.}
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
#' mapping = list(projectTitle = "Project", plotName = "Plot", subPlotName = "Subplot",
#'                obsStartDate = "PlotObsStartDate", obsEndDate = "PlotObsStopDate")
#'
#' # Create a new Veg-X document with projects, plots and plot observations (no data)
#' x = addPlotObservations(newVegX(), moki_site, mapping = mapping)
#'
#' # Examine the result
#' showElementTable(x, "plot")
#' showElementTable(x, "plotObservation")
#'
#' # The same but capturing unique identifiers from data source IDs (not used by the Veg-X package)
#'
#' mapping = list(projectTitle = "Project", plotName = "Plot", subPlotName = "Subplot",
#'                obsStartDate = "PlotObsStartDate", obsEndDate = "PlotObsStopDate",
#'                plotUniqueIdentifier = "PlotID", plotObservationUniqueIdentifier = "PlotObsID")
#' x = addPlotObservations(newVegX(), moki_site, mapping = mapping)
#'
#' showElementTable(x, "plot")
#' showElementTable(x, "plotObservation")

addPlotObservations<-function(target, x,
                              mapping,
                              missing.values = c(NA,""),
                              date.format = "%Y-%m-%d",
                              verbose = TRUE) {
  x = as.data.frame(x)
  nrecords = nrow(x)
  nmissing = 0



  #check mappings
  mappingsAvailable = c("projectTitle", "plotName", "obsStartDate", "obsEndDate", "subPlotName", "plotUniqueIdentifier", "plotObservationUniqueIdentifier",
                                       "observationParty")

  #Warning for non-recognized mappings
  nonRecognizedMappings = names(mapping)[!(names(mapping) %in% mappingsAvailable)]
  if(length(nonRecognizedMappings)>0) warning(paste0("Some names in 'mapping' were not recognized and therefore ignored: ", paste(nonRecognizedMappings, collapse = ", ")))
  
  #Check columns exist
  for(i in 1:length(mapping)) {
    if(!(mapping[i] %in% names(x))) {
      if(names(mapping)[i] %in% mappingsAvailable) stop(paste0("Variable '", mapping[i],"' not found in column names. Revise mapping or data."))
    }
  }
  
  
  plotNames = as.character(x[[mapping[["plotName"]]]])
  obsStartDates = as.Date(as.character(x[[mapping[["obsStartDate"]]]]), format =date.format)

  #Optional mappings
  projectFlag = ("projectTitle" %in% names(mapping))
  if(projectFlag) {
    projectTitles = as.character(x[[mapping[["projectTitle"]]]])
  }
  obsEndFlag = ("obsEndDate" %in% names(mapping))
  if(obsEndFlag) {
    obsEndDates = as.Date(as.character(x[[mapping[["obsEndDate"]]]]), format =date.format)
  }
  subPlotFlag = ("subPlotName" %in% names(mapping))
  if(subPlotFlag) {
    subPlotNames = as.character(x[[mapping[["subPlotName"]]]])
  }
  plotUniqueIDFlag = ("plotUniqueIdentifier" %in% names(mapping))
  if(plotUniqueIDFlag) {
    plotUniqueIdentifiers = as.character(x[[mapping[["plotUniqueIdentifier"]]]])
  }
  plotObsUniqueIDFlag = ("plotObservationUniqueIdentifier" %in% names(mapping))
  if(plotObsUniqueIDFlag) {
    plotObsUniqueIdentifiers = as.character(x[[mapping[["plotObservationUniqueIdentifier"]]]])
  }
  observationPartyFlag = ("observationParty" %in% names(mapping))
  if(observationPartyFlag) {
    observationParties = as.character(x[[mapping[["observationParty"]]]])
  }


  orinparties = length(target@parties)
  orinprojects = length(target@projects)
  orinplots = length(target@plots)
  orinplotobs = length(target@plotObservations)
  parsedParties = character(0)
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
        projectTitle = projectTitles[i]
      }
      if(!(projectTitle %in% parsedProjects)) {
        nprid = .newProjectIDByTitle(target, projectTitle) # Get the new project ID (internal code)
        projectID = nprid$id
        if(nprid$new) target@projects[[projectID]] = list("title" = projectTitle)
        parsedProjects = c(parsedProjects, projectTitle)
        parsedProjectIDs = c(parsedProjectIDs, projectID)
      } else { #this access should be faster
        projectID = parsedProjectIDs[which(parsedProjects==projectTitle)]
      }
    }

    #plot
    if(!(plotNames[i] %in% missing.values)) {# If plotName is missing take the previous one
      plotName = plotNames[i]
    }
    if(!(plotName %in% parsedPlots)) {
      npid = .newPlotIDByName(target, plotName) # Get the new plot ID (internal code)
      plotID = npid$id
      if(npid$new) target@plots[[plotID]] = list("plotName" = plotName)
      parsedPlots = c(parsedPlots, plotNames[i])
      parsedPlotIDs = c(parsedPlotIDs, plotID)
    } else { #this access should be faster
      plotID = parsedPlotIDs[which(parsedPlots==plotName)]
    }
    #subplot (if defined)
    if(subPlotFlag){
      if(!(subPlotNames[i] %in% missing.values)) {# If subPlotName is missing use parent plot ID
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
    # Set plot unique identifier
    if(plotUniqueIDFlag) {
      if(!(plotUniqueIdentifiers[i] %in% missing.values)) {
        target@plots[[plotID]]$plotUniqueIdentifier = plotUniqueIdentifiers[i]
      }
    }
    #plot observation
    if(!(obsStartDates[i] %in% missing.values)) {# If observation date is missing take the previous one
      obsStartDate = obsStartDates[i]
    }
    pObsString = paste(plotID, obsStartDate) # plotID+Date
    if(!(pObsString %in% parsedPlotObs)) {
      npoid = .newPlotObsIDByDate(target, plotID, obsStartDate) # Get the new plot observation ID (internal code)
      plotObsID = npoid$id
      if(npoid$new) {
        target@plotObservations[[plotObsID]] = list("plotID" = plotID,
                                                    "projectID" = projectID,
                                                    "obsStartDate" = obsStartDate)
      }
      parsedPlotObs = c(parsedPlotObs, pObsString)
      parsedPlotObsIDs = c(parsedPlotObsIDs, plotObsID)
    } else {
      plotObsID = parsedPlotIDs[which(parsedPlotObs==pObsString)]
    }
    #observationParty
    if(observationPartyFlag) {
      if(!(observationParties[i] %in% missing.values)) {
        npid = .newPartyIDByName(target, observationParties[i])
        partyID = npid$id
        if(npid$new) target@parties[[partyID]] = list(name = observationParties[i],
                                                      partyType = "individual")

        target@plotObservations[[plotObsID]]$observationPartyID = partyID
        parsedParties = c(parsedParties, observationParties[i])
      }
    }

    #add observation end if needed
    if(obsEndFlag) {
      if(!(obsEndDates[i] %in% missing.values)) {# If observation end date is missing take the previous one
        obsEndDate = obsEndDates[i]
      }
      target@plotObservations[[plotObsID]]$obsEndDate = obsEndDate
    }

    # Set plot observation unique identifier
    if(plotObsUniqueIDFlag) {
      if(!(plotObsUniqueIdentifiers[i] %in% missing.values)) {
        target@plotObservations[[plotObsID]]$plotObservationUniqueIdentifier = plotObsUniqueIdentifiers[i]
      }
    }

  }
  finnparties = length(target@parties)
  finnprojects = length(target@projects)
  finnplots = length(target@plots)
  finnplotobs = length(target@plotObservations)
  finnabioobs = length(target@siteObservations)
  if(verbose) {
    if(length(parsedParties)>0) cat(paste0(" " , length(parsedParties)," observation partie(s) parsed, ", finnparties-orinparties, " new partie(s) added. Consider providing party information.\n"))
    if(length(parsedProjects)>0) cat(paste0(" " , length(parsedProjects)," project(s) parsed, ", finnprojects-orinprojects, " new project(s) added. Consider providing project information.\n"))
    cat(paste0(" " , length(parsedPlots)," plot(s) parsed, ", finnplots-orinplots, " new plot(s) added.\n"))
    cat(paste0(" " , length(parsedPlotObs)," plot observation(s) parsed, ", finnplotobs-orinplotobs, " new plot observation(s) added.\n"))
  }

  return(target)
}
