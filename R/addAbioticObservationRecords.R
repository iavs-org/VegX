#' Add abiotic observation records
#'
#' Adds abiotic observation records to a VegX object from a data table where rows are plot observations,
#' using a mapping to identify plot observation: plot, subplot (optional) and observation date.
#' Additional mappings are used to map specific abiotic variables.
#'
#' @param target The initial object of class \code{\linkS4class{VegX}} to be modified
#' @param x A data frame where each row corresponds to one plot observation. Columns can be varied.
#' @param projectTitle A string to identify the project title, which can be the same as one of the currently defined in \code{target}.
#' @param mapping A list with element names 'plotName', 'obsStartDate', used to specify the mapping of data columns (specified using strings for column names) onto these variables.
#' Abiotic variables that can be mapped are: 'phosphorus', 'pottasium', 'magnesium', 'nitrogen' and 'ph'.
#' Additional optional mappings are: 'subPlotName' and 'obsEndDate'.
#' @param measurementMethods A list of objects of class \code{\linkS4class{VegXMethod}} with the measurement method
#' for each of the abiotic variables stated in \code{mapping}.
#' @param missing.values A character vector of values that should be considered as missing observations/measurements.
#' @param verbose A boolean flag to indicate console output of the data integration process.
#'
#' @return The modified object of class \code{\linkS4class{VegX}}.
#' @export
#'
#' @references Wiser SK, Spencer N, De Caceres M, Kleikamp M, Boyle B & Peet RK (2011). Veg-X - an exchange standard for plot-based vegetation data
#'
#' @seealso \code{\link{addTaxonObservationRecords}}, \code{\link{addTreeObservationRecords}}.
#'
#' @examples
addAbioticObservationRecords<-function(target, x, projectTitle,
                                       mapping,
                                       measurementMethods = list(),
                                       missing.values = c("NA",""),
                                       verbose = TRUE) {
  x = as.data.frame(x)
  nrecords = nrow(x)
  nmissing = 0

  #Check columns exist
  for(i in 1:length(mapping)) {
    if(!(mapping[i] %in% names(x))) stop(paste0("Variable '", mapping[i],"' not found in column names. Revise mapping or data."))
  }
  plotNames = as.character(x[[mapping[["plotName"]]]])
  obsStartDates = as.Date(as.character(x[[mapping[["obsStartDate"]]]]))

  #Optional mappings
  obsEndFlag = ("obsEndDate" %in% names(mapping))
  if(obsEndFlag) {
    obsEndDates = as.Date(as.character(x[[mapping[["obsEndDate"]]]]))
  }
  subPlotFlag = ("subPlotName" %in% names(mapping))
  if(subPlotFlag) {
    subPlotNames = as.character(x[[mapping[["subPlotName"]]]])
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
  orinplotobs = length(target@plotObservations)
  orinabioobs = length(target@abioticObservations)
  parsedPlots = character(0)
  parsedPlotIDs = character(0)
  parsedPlotObs = character(0)
  parsedPlotObsIDs = character(0)
  abioObsCounter = orinabioobs+1 #counter
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
    #plot observation
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
  finnplots = length(target@plots)
  finnplotobs = length(target@plotObservations)
  finnabioobs = length(target@abioticObservations)
  if(verbose) {
    cat(paste0(" " , length(parsedPlots)," plot(s) parsed, ", finnplots-orinplots, " new plot(s) added.\n"))
    cat(paste0(" " , length(parsedPlotObs)," plot observation(s) parsed, ", finnplotobs-orinplotobs, " new plot observation(s) added.\n"))
    cat(paste0(" ", nrecords," record(s) parsed, ", finnabioobs-orinabioobs, " new abiotic observation(s) added.\n"))
    if(nmissing>0) cat(paste0(" ", nmissing, " measurement(s) with missing value(s) not added.\n"))
  }

  return(target)
}
