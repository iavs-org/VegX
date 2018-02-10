#' Generates unique identifiers
#' 
#' Generates unique identifiers for plot elements or plot observation elements
#'
#' @param target The initial object of class \code{\linkS4class{VegX}} to be modified
#' @param element The Veg-X main element for which unique identifiers are needed: \code{"plot"} or \code{"plotObservation"}.
#' @param replace.existing A boolean flag to indicate whether newly generated identifiers should replace existing identifiers.
#'
#' @return The modified object of class \code{\linkS4class{VegX}}.
#' @export
#'
#' @references Wiser SK, Spencer N, De Caceres M, Kleikamp M, Boyle B & Peet RK (2011). Veg-X - an exchange standard for plot-based vegetation data
#'
#' @family fill functions
#'
#' @examples
#' # Load source data
#' data(mokihinui)
#'
#' # Create a new Veg-X document with projects, plots and plot observations (no data)
#' mapping = list(projectTitle = "Project", plotName = "Plot", subPlotName = "Subplot",
#'                obsStartDate = "PlotObsStartDate", obsEndDate = "PlotObsStopDate")
#' x = addPlotObservations(newVegX(), moki_site, mapping = mapping)
#'
#' # Examine the result
#' showElementTable(x, "plot")
#' 
#' # Add unique identifiers
#' y = fillUniqueIdentifiers(x, "plot")
#' 
#' # Examine the result
#' showElementTable(y, "plot")
#' 
fillUniqueIdentifiers<-function(target, element = "plot", replace.existing = FALSE) {
  if(class(target)!="VegX") stop("Wrong class for 'target'. Should be an object of class 'VegX'")
  element = match.arg(element, c("plot", "plotObservation"))
  
  if(element=="plot") {
    if(length(target@plots)>0) {
      for(i in 1:length(target@plots)) {
        uuid = UUIDgenerate()
        if(replace.existing) target@plots[[i]]$plotUniqueIdentifier = uuid
        else {
          if(!("plotUniqueIdentifier" %in% names(target@plots[[i]]))) {
            target@plots[[i]]$plotUniqueIdentifier = uuid
          } else {
            if(target@plots[[i]]$plotUniqueIdentifier=="") target@plots[[i]]$plotUniqueIdentifier = uuid
          }
        }
      }
    }
  }
  if(element=="plotObservation") {
    if(length(target@plotObservations)>0) {
      for(i in 1:length(target@plotObservations)) {
        uuid = UUIDgenerate()
        if(replace.existing) target@plotObservations[[i]]$plotObservationUniqueIdentifier = uuid
        else {
          if(!("plotObservationUniqueIdentifier" %in% names(target@plotObservations[[i]]))) {
            target@plotObservations[[i]]$plotObservationUniqueIdentifier = uuid
          } else {
            if(target@plotObservations[[i]]$plotObservationUniqueIdentifier=="") target@plotObservations[[i]]$plotObservationUniqueIdentifier = uuid
          }
        }
      }
    }
  }
  return(target)
}
