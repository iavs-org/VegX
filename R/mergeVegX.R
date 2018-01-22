#' Merges two Veg-X documents
#'
#' Merges two Veg-X documents while considering that some entities may be shared.
#'
#' @param x,y The objects of class \code{\linkS4class{VegX}} to be merged
#' @param verbose A flag to indicate console output of the data integration process.
#'
#' @return An object of class \code{\linkS4class{VegX}} with the pooled data
#' @export
#'
#' @details Some entities are attempted to be merged or are kept as separate entities depeding on their definition:
#' \itemize{
#'   \item \code{projects} are merged when their element \code{title} has the same value
#'   \item \code{plots} are merged when their element \code{plotName} has the same value
#'   \item \code{plotObservations} are merged when both \code{plotID} and \code{obsStartDate} have the same value
#'   \item \code{taxonNameUsageConcepts} are merged when element \code{authorName} has the same value
#'   \item \code{methods} are merged when element \code{name} has the same value
#' }
#' Merging to these entities may cause interruption of the process if the two entities to be merged
#' have different value for the same element. Other entities (\code{stratumObservation}, \code{individualOrganismObservation}) are always considered as distinct
#' entities between the two data sets to be merged and hence are simply copied to the result.
#'
#' @examples
#'
#' data(mokihinui)
#'
#' # Create document 'x' with aggregated taxon observations
#' taxmapping = list(plotName = "Plot", obsStartDate = "obsDate", taxonAuthorName = "PreferredSpeciesName",
#'               stratumName = "Tier", value = "Category")
#' scale = defineCoverScale(name = "Standard Recce (Allen)", description = "Recce recording method by Allen",
#'                          citation = "Hurst, JM and Allen, RB. (2007) The Recce method for describing New Zealand vegetation – Field protocols. Landcare Research, Lincoln.",
#'                          breaks = c(0, 0.1, 1, 5, 25, 50, 75, 100),
#'                          midPoints = c(0.01, 0.05, 0.5, 15, 37.5, 62.5, 87.5),
#'                          values = c("P","1","2","3", "4", "5", "6"))
#' strataDef = defineStrataByHeight(name = "Recce strata",
#'                                 description = "Standard Recce stratum definition",
#'                                 citation = "Hurst, JM and Allen, RB. (2007) The Recce method for describing New Zealand vegetation – Field protocols. Landcare Research, Lincoln.",
#'                                 heightBreaks = c(0, 0.3,2.0,5, 12, 25,50, 100),
#'                                 stratumNames = paste0("Tier ",1:7))
#' x = addTaxonObservations(newVegX(), tcv, "Mokihinui",
#'                         mapping = taxmapping,
#'                         abundanceMethod = scale,
#'                         stratumDefinition = strataDef)
#'
#' # Create document 'y' with tree observations
#' treemapping = list(plotName = "Plot", subPlotName = "Subplot", obsStartDate = "obsDate",
#'                    taxonAuthorName = "PreferredSpeciesName", diameter = "Diameter")
#' diamMeth = predefinedMeasurementMethod("DBH")
#' y = addTreeObservations(newVegX(), dia, "Mokihinui",
#'                         mapping = treemapping,
#'                         diameterMethod = diamMeth)
#'
#' # Merge 'x' and 'y'
#' z = mergeVegX(x,y)
#' summary(z)
#'
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
  if(length(y@plots)>0) {
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
  }
  if(verbose) {
    cat(paste0(" Final number of plots: ", length(x@plots),". Data pooled for ", nmergedplots, " plot(s).\n"))
  }
  #plotObservations
  plotObsIDmap = list()
  nmergedplotobs = 0
  if(length(y@plotObservations)>0) {
    for(j in 1:length(y@plotObservations)) {
      plotID = plotIDmap[[y@plotObservations[[j]]$plotID]]
      y@plotObservations[[j]]$plotID = plotID # set plot ID to translated one in order to avoid matching problems (does not change id externally)
      npoid = .newPlotObsIDByDate(x, plotID, y@plotObservations[[j]]$obsStartDate)
      if(npoid$new) {
        x@plotObservations[[npoid$id]] = y@plotObservations[[j]]
      } else { #pool information
        x@plotObservations[[npoid$id]] = .mergePlotObservations(x@plotObservations[[npoid$id]], y@plotObservations[[j]])
        nmergedplotobs = nmergedplotobs + 1
      }
      plotObsIDmap[names(y@plotObservations)[j]] = npoid$id
    }
  }
  if(verbose) {
    cat(paste0(" Final number of plot observations: ", length(x@plotObservations),". Data pooled for ", nmergedplotobs, " plot observation(s).\n"))
  }

  #taxonNameUsageConcepts
  tnucIDmap = list()
  nmergedtnucs = 0
  if(length(y@taxonNameUsageConcepts)>0) {
    for(j in 1:length(y@taxonNameUsageConcepts)) {
      ntnucid = .newTaxonNameUsageConceptIDByName(x, y@taxonNameUsageConcepts[[j]]$authorName)
      if(ntnucid$new) {
        x@taxonNameUsageConcepts[[ntnucid$id]] = y@taxonNameUsageConcepts[[j]]
      } else { #pool information
        x@taxonNameUsageConcepts[[ntnucid$id]] = .mergeTaxonNameUsageConcepts(x@taxonNameUsageConcepts[[ntnucid$id]], y@taxonNameUsageConcepts[[j]])
        nmergedtnucs = nmergedtnucs + 1
      }
      tnucIDmap[names(y@taxonNameUsageConcepts)[j]] = ntnucid$id
    }
  }
  if(verbose) {
    cat(paste0(" Final number of taxon name usage concepts: ", length(x@taxonNameUsageConcepts),". Data pooled for ", nmergedtnucs, " taxon name usage concept(s).\n"))
  }

  # methods
  methodIDmap = list()
  nmergedmeths = 0
  if(length(y@methods)>0) {
    for(j in 1:length(y@methods)) {
      nmetid = .newMethodIDByName(x, y@methods[[j]]$name)
      if(nmetid$new) {
        x@methods[[nmetid$id]] = y@methods[[j]]
      } else { #pool information
        x@methods[[nmetid$id]] = .mergeMethods(x@methods[[nmetid$id]], y@methods[[j]])
        nmergedmeths = nmergedmeths + 1
      }
      methodIDmap[names(y@methods)[j]] = nmetid$id
    }
  }
  if(verbose) {
    cat(paste0(" Final number of methods: ", length(x@methods),". Data pooled for ", nmergedmeths, " method(s).\n"))
  }


  #strata
  strIDmap = list()
  nmergedstr = 0
  if(length(y@strata)>0) {
    for(j in 1:length(y@strata)) {
      methodID = methodIDmap[[y@strata[[j]]$methodID]]
      y@strata[[j]]$methodID = methodID # set method ID to translated one in order to avoid matching problems (does not change id externally)
      nstrid = .newStratumIDByName(x, y@strata[[j]]$stratumName)
      if(nstrid$new) {
        x@strata[[nstrid$id]] = y@strata[[j]]
      } else { #pool information
        x@strata[[nstrid$id]] = .mergeStrata(x@strata[[nstrid$id]], y@strata[[j]])
        nmergedstr = nmergedstr + 1
      }
      strIDmap[names(y@strata)[j]] = nstrid$id
    }
  }
  if(verbose) {
    cat(paste0(" Final number of strata: ", length(x@strata),". Data pooled for ", nmergedstr, " strata.\n"))
  }

  #                         stratumObservations = "list",
  #                         aggregatedObservations = "list",
  #                         individualOrganisms = "list",
  #                         individualObservations = "list",
  #                         vegetationObservations = "list",
  #                         surfaceCovers = "list",
  #                         surfaceCoverObservations = "list",
  #                         abioticObservations = "list",
  #                         ancillaryObservations = "list",
  #                         attributes = "list"))
  return(x)
}
