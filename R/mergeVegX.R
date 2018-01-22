#' Merges two Veg-X documents
#'
#' Merges two Veg-X documents, assuming that they may refer to the same projects, the same plots and the same plot observations.
#'
#' @param x,y The objects of class \code{\linkS4class{VegX}} to be merged
#' @param verbose A flag to indicate console output of the data integration process.
#'
#' @return An object of class \code{\linkS4class{VegX}} with the pooled data
#' @export
#'
#' @examples
mergeVegX<-function(x, y, verbose = TRUE) {

  # uses 'x' as the target and 'y' as the source of data

  #projects IMPORTANT: Should be modified if other elements than 'title' are considered
  projectIDmap = list()
  for(j in 1:length(y@projects)) {
    npid = .newProjectIDByTitle(x, y@projects[[j]]$title)
    if(npid$new) {
      x@projects[[npid$id]] = y@projects[[j]]
    }
    projectIDmap[names(y@projects)[j]] = npid$id
  }

  #plots
  plotIDmap = list()
  nmergedplots = 0
  for(j in 1:length(y@plots)) {
    npid = .newPlotIDByName(x, y@plots[[j]]$plotName)
    if(npid$new) {
      x@plots[[npid$id]] = y@plots[[j]]
    } else { #pool information
      x@plots[[npid$id]] = .mergePlots(x@plots[[npid$id]], y@plots[[j]])
      nmergedplots = nmergedplots + 1
    }
    plotIDmap[names(y@plots)[j]] = npid$id
  }
  if(verbose) {
    cat(paste0(" Final number of plots: ", length(x@plots),". Data pooled for ", nmergedplots, " plots.\n"))
  }
  #plotObservations
  plotObsIDmap = list()
  nmergedplotobs = 0
  for(j in 1:length(y@plotObservations)) {
    plotID = plotIDmap[y@plotObservations[j]$plotID]
    npoid = .newPlotObsIDByDate(x, plotID, y@plotObservations[[j]]$obsStartDate)
    if(npoid$new) {
      x@plotObservations[[npoid$id]] = y@plotObservations[[j]]
    } else { #pool information
      x@plotObservations[[npid$id]] = .mergePlotObservations(x@plotObservations[[npoid$id]], y@plotObservations[[j]])
      nmergedplotobs = nmergedplotobs + 1
    }
    plotObsIDmap[names(y@plotObservations)[j]] = npoid$id
  }
  if(verbose) {
    cat(paste0(" Final number of plot observations: ", length(x@plotObservations),". Data pooled for ", nmergedplotobs, " plot observation(s).\n"))
  }

  #                         taxonNameUsageConcepts = "list",
  #                         strata = "list",
  #                         stratumObservations = "list",
  #                         aggregatedObservations = "list",
  #                         individualOrganisms = "list",
  #                         individualObservations = "list",
  #                         vegetationObservations = "list",
  #                         surfaceCovers = "list",
  #                         surfaceCoverObservations = "list",
  #                         abioticObservations = "list",
  #                         ancillaryObservations = "list",
  #                         methods = "list",
  #                         attributes = "list"))
  return(x)
}
