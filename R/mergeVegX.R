#' Merge Veg-X documents
#'
#' Merges two Veg-X documents while considering that some of their entities may be shared.
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
#'   \item \code{plotObservations} are merged when both their \code{plotID} and \code{obsStartDate} elements have the same value
#'   \item \code{taxonNameUsageConcepts} are merged when their element \code{authorName} has the same value
#'   \item \code{methods} are merged when their element \code{name} has the same value
#'   \item \code{strata} are merged when their element \code{name} has the same value
#'   \item \code{stratumObservations} are merged when both their \code{stratumID} and \code{plotObservationID} elements have the same value
#'   \item \code{aggregateOrganismObservations} are merged when their \code{plotObservationID} and \code{taxonUsageConceptID} (and \code{stratumObservationID}, if defined) have the same value
#'   \item \code{individualOrganisms} are merged when both their \code{plotID} and \code{identificationLabel} have the same value
#'   \item \code{individualOrganismObservations} are merged when both their \code{plotObservationID} and \code{individualOrganismID} have the same value.
#'   \item \code{siteObservations} are merged into the same element when their element \code{plotObservationID} has the same value, but particular measurements are always added
#'   as if they were distinct pieces of information.
#' }
#' Merging to these entities may cause interruption of the process if the two entities to be merged
#' have different value for the same element. Other entities (e.g., \code{attributes} of a method) are always considered as distinct
#' entities between the two data sets to be merged and hence are simply copied to the result.
#'
#' @seealso \code{\linkS4class{VegX}}
#'
#' @examples
#'
#' data(mokihinui)
#'
#' # Create document 'x' with aggregate taxon observations
#' taxmapping = list(plotName = "Plot", obsStartDate = "obsDate", taxonAuthorName = "PreferredSpeciesName",
#'               stratumName = "Tier", value = "Category")
#' scale = definePlantCoverScale(name = "Recce cover scale", description = "Recce recording method by Allen",
#'                          citation = "Hurst, JM and Allen, RB. (2007) The Recce method for describing New Zealand vegetation – Field protocols. Landcare Research, Lincoln.",
#'                          breaks = c(0, 0.1, 1, 5, 25, 50, 75, 100),
#'                          midPoints = c(0.01, 0.05, 0.5, 15, 37.5, 62.5, 87.5),
#'                          values = c("P","1","2","3", "4", "5", "6"))
#' strataDef = defineMixedStrata(name = "Recce strata",
#'                               description = "Standard Recce stratum definition",
#'                               citation = "Hurst, JM and Allen, RB. (2007) The Recce method for describing New Zealand vegetation – Field protocols. Landcare Research, Lincoln.",
#'                               heightStrataBreaks = c(0, 0.3,2.0,5, 12, 25, 50),
#'                               heightStrataNames = paste0("Tier ",1:6),
#'                               categoryStrataNames = "Tier 7")
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

  # methods/attributes
  methodIDmap = list()
  attIDmap = list()
  nmergedmeths = 0
  if(length(y@methods)>0) {
    for(j in 1:length(y@methods)) {
      nmetid = .newMethodIDByName(x, y@methods[[j]]$name)
      if(nmetid$new) {
        x@methods[[nmetid$id]] = y@methods[[j]]
        # add attributes
        attIDs = .getAttributeIDsByMethodID(y, names(y@methods)[j]) #Get attribute IDs in 'y'
        for(i in 1:length(attIDs)) {
          newAttID = as.character(length(x@attributes)+1)
          x@attributes[[newAttID]] = y@attributes[[attIDs[i]]]
          x@attributes[[newAttID]]$methodID = nmetid$id
          attIDmap[[attIDs[i]]] = newAttID
        }
      } else { #pool information
        x@methods[[nmetid$id]] = .mergeMethods(x@methods[[nmetid$id]], y@methods[[j]])
        #TO DO: We should check the attribute correspondence here
        nmergedmeths = nmergedmeths + 1
      }
      methodIDmap[names(y@methods)[j]] = nmetid$id
    }
  }
  if(verbose) {
    cat(paste0(" Final number of methods: ", length(x@methods),". Data pooled for ", nmergedmeths, " method(s).\n"))
    cat(paste0(" Final number of attributes: ", length(x@attributes),".\n"))
  }

  #projects IMPORTANT: Should be modified if other elements than 'title' are considered
  projectIDmap = list()
  for(j in 1:length(y@projects)) {
    npid = .newProjectIDByTitle(x, y@projects[[j]]$title)
    if(npid$new) {
      x@projects[[npid$id]] = y@projects[[j]]
    }
    projectIDmap[names(y@projects)[j]] = npid$id
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


  #plots
  plotIDmap = list()
  nmergedplots = 0
  if(length(y@plots)>0) {
    for(j in 1:length(y@plots)) {
      if("parentPlotID" %in% names(y@plots[[j]])) y@plots[[j]]$parentPlotID = plotIDmap[[y@plots[[j]]$parentPlotID]] #set parent plot ID to translated one in order to avoid matching problems
      npid = .newPlotIDByName(x, y@plots[[j]]$plotName)
      if(npid$new) {
        x@plots[[npid$id]] = .applyAttributeMappingToPlot(y@plots[[j]], attIDmap)
      } else { #pool information
        x@plots[[npid$id]] = .mergePlots(x@plots[[npid$id]], y@plots[[j]], attIDmap)
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
        x@plotObservations[[npoid$id]] = .mergePlotObservations(x@plotObservations[[npoid$id]], y@plotObservations[[j]], attIDmap)
        nmergedplotobs = nmergedplotobs + 1
      }
      plotObsIDmap[names(y@plotObservations)[j]] = npoid$id
    }
  }
  if(verbose) {
    cat(paste0(" Final number of plot observations: ", length(x@plotObservations),". Data pooled for ", nmergedplotobs, " plot observation(s).\n"))
  }


  # stratumObservations
  strObsIDmap = list()
  nmergedstrobs = 0
  if(length(y@stratumObservations)>0) {
    for(j in 1:length(y@stratumObservations)) {
      stratumID = strIDmap[[y@stratumObservations[[j]]$stratumID]]
      plotObsID = plotObsIDmap[[y@stratumObservations[[j]]$plotObservationID]]
      y@stratumObservations[[j]]$stratumID = stratumID # set stratum ID to translated one in order to avoid matching problems (does not change id externally)
      y@stratumObservations[[j]]$plotObservationID = plotObsID # set plot observation ID to translated one in order to avoid matching problems (does not change id externally)
      nstrobsid = .newStratumObsIDByIDs(x, plotObsID, stratumID)
      if(nstrobsid$new) {
        x@stratumObservations[[nstrobsid$id]] = y@stratumObservations[[j]]
      } else { #pool information
        x@stratumObservations[[nstrobsid$id]] = .mergeStratumObservations(x@stratumObservations[[nstrobsid$id]], y@stratumObservations[[j]], attIDmap)
        nmergedstrobs = nmergedstrobs + 1
      }
      strObsIDmap[names(y@stratumObservations)[j]] = nstrobsid$id
    }
  }
  if(verbose) {
    cat(paste0(" Final number of stratum observations: ", length(x@stratumObservations),". Data pooled for ", nmergedstrobs, " stratum observation(s).\n"))
  }

  # aggregateObservations
  aggObsIDmap = list()
  nmergedaggobs = 0
  if(length(y@aggregateObservations)>0) {
    for(j in 1:length(y@aggregateObservations)) {
      plotObsID = plotObsIDmap[[y@aggregateObservations[[j]]$plotObservationID]]
      tnucID = tnucIDmap[[y@aggregateObservations[[j]]$taxonNameUsageConceptID]]
      y@aggregateObservations[[j]]$plotObservationID = plotObsID # set plot observation ID to translated one in order to avoid matching problems (does not change id externally)
      y@aggregateObservations[[j]]$taxonNameUsageConceptID = tnucID # set tnuc ID to translated one in order to avoid matching problems (does not change id externally)
      strObsID = ""
      if("stratumObservationID" %in% names(y@aggregateObservations[[j]])) {
        if(y@aggregateObservations[[j]]$stratumObservationID!="") {
          strObsID = strObsIDmap[[y@aggregateObservations[[j]]$stratumObservationID]]
          y@aggregateObservations[[j]]$stratumObservationID = strObsID
        }
      }
      naggobsid = .newAggregateOrganismObservationIDByTaxonID(x, plotObsID, strObsID, tnucID)
      if(naggobsid$new) {
        x@aggregateObservations[[naggobsid$id]] = .applyAttributeMappingToAggregatePlotObservations( y@aggregateObservations[[j]], attIDmap)
      } else { #pool information
        x@aggregateObservations[[naggobsid$id]] = .mergeAggregateOrganismObservations(x@aggregateObservations[[naggobsid$id]], y@aggregateObservations[[j]], attIDmap)
        nmergedaggobs = nmergedaggobs + 1
      }
      aggObsIDmap[names(y@aggregateObservations)[j]] = naggobsid$id
    }
  }
  if(verbose) {
    cat(paste0(" Final number of aggregate organism observations: ", length(x@aggregateObservations),". Data pooled for ", nmergedaggobs, " aggregate organism observation(s).\n"))
  }

  # individualOrganisms
  indIDmap = list()
  nmergedind = 0
  if(length(y@individualOrganisms)>0) {
    for(j in 1:length(y@individualOrganisms)) {
      plotID = plotIDmap[[y@individualOrganisms[[j]]$plotID]]
      tnucID = tnucIDmap[[y@individualOrganisms[[j]]$taxonNameUsageConceptID]]
      y@individualOrganisms[[j]]$plotID = plotID # set plot ID to translated one in order to avoid matching problems (does not change id externally)
      y@individualOrganisms[[j]]$taxonNameUsageConceptID = tnucID # set tnuc ID to translated one in order to avoid matching problems (does not change id externally)
      nindid = .newIndividualOrganismIDByIdentificationLabel(x, plotID, y@individualOrganisms[[j]]$identificationLabel)
      if(nindid$new) {
        x@individualOrganisms[[nindid$id]] = y@individualOrganisms[[j]]
      } else { #pool information
        x@individualOrganisms[[nindid$id]] = .mergeIndividualOrganisms(x@individualOrganisms[[nindid$id]], y@individualOrganisms[[j]])
        nmergedind = nmergedind + 1
      }
      indIDmap[names(y@individualOrganisms)[j]] = nindid$id
    }
  }
  if(verbose) {
    cat(paste0(" Final number of individual organisms: ", length(x@individualOrganisms),". Data pooled for ", nmergedind, " individual organism(s).\n"))
  }


  # individualObservations
  indObsIDmap = list()
  nmergedindobs = 0
  if(length(y@individualObservations)>0) {
    for(j in 1:length(y@individualObservations)) {
      plotObsID = plotObsIDmap[[y@individualObservations[[j]]$plotObservationID]]
      indID = indIDmap[[y@individualObservations[[j]]$individualOrganismID]]
      y@individualObservations[[j]]$plotObservationID = plotObsID # set plot observation ID to translated one in order to avoid matching problems (does not change id externally)
      y@individualObservations[[j]]$individualOrganismID = indID # set individual organism ID to translated one in order to avoid matching problems (does not change id externally)
      if("stratumObservationID" %in% names(y@individualObservations[[j]])) {
        strObsID = strObsIDmap[[y@individualObservations[[j]]$stratumObservationID]]
        y@individualObservations[[j]]$stratumObservationID = strObsID
      }
      nindobsid = .newIndividualOrganismObservationIDByIndividualID(x, plotObsID, indID)
      if(nindobsid$new) {
        x@individualObservations[[nindobsid$id]] = .applyAttributeMappingToIndividualOrganismObservations(y@individualObservations[[j]], attIDmap)
      } else { #pool information
        x@individualObservations[[nindobsid$id]] = .mergeIndividualOrganismObservations(x@individualObservations[[nindobsid$id]], y@individualObservations[[j]], attIDmap)
        nmergedindobs = nmergedindobs + 1
      }
      indObsIDmap[names(y@individualObservations)[j]] = nindobsid$id
    }
  }
  if(verbose) {
    cat(paste0(" Final number of individual organism observations: ", length(x@individualObservations),". Data pooled for ", nmergedindobs, " individual organism observation(s).\n"))
  }


  # siteObservations
  siteObsIDmap = list()
  nmergedsiteobs = 0
  if(length(y@siteObservations)>0) {
    for(j in 1:length(y@siteObservations)) {
      plotObsID = plotObsIDmap[[y@siteObservations[[j]]$plotObservationID]]
      y@siteObservations[[j]]$plotObservationID = plotObsID # set plot observation ID to translated one in order to avoid matching problems (does not change id externally)
      nsiteobsid = .newSiteObservationIDByID(x, plotObsID)
      if(nsiteobsid$new) {
        x@siteObservations[[nsiteobsid$id]] = .applyAttributeMappingToSiteObservations(y@siteObservations[[j]], attIDmap)
      } else { #pool information
        x@siteObservations[[nsiteobsid$id]] = .mergeSiteObservations(x@siteObservations[[nsiteobsid$id]], y@siteObservations[[j]], attIDmap)
        nmergedsiteobs = nmergedsiteobs + 1
      }
      indObsIDmap[names(y@siteObservations)[j]] = nsiteobsid$id
    }
  }
  if(verbose) {
    cat(paste0(" Final number of site observations: ", length(x@siteObservations),". Data pooled for ", nmergedsiteobs, " site observation(s).\n"))
  }

  #                         vegetationObservations = "list",
  #                         surfaceCovers = "list",
  #                         surfaceCoverObservations = "list",
  return(x)
}
