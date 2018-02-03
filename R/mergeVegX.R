#' Merge Veg-X documents
#'
#' Merges two Veg-X documents while considering that some of their entities may be shared.
#'
#' @param x,y The objects of class \code{\linkS4class{VegX}} to be merged.
#' @param mergeIdentities A flag used to force that organism identities should be merged when sharing the same organismName.
#' @param verbose A flag to indicate console output of the data integration process.
#'
#' @return An object of class \code{\linkS4class{VegX}} with the pooled data
#' @export
#'
#' @details Some entities are attempted to be merged or are kept as separate entities depeding on their definition:
#' \itemize{
#'   \item \code{projects} are merged when their element \code{title} has the same value.
#'   \item \code{plots} are merged when their element \code{plotName} has the same value.
#'   \item \code{plotObservations} are merged when both their \code{plotID} and \code{obsStartDate} elements have the same value
#'   \item \code{organismIdentities} are merged if they share the same organismName and \code{mergeIdentities = TRUE} because
#'   one can have the same name used in different data sets but referring to different concepts.
#'   \item \code{methods} are merged when their element \code{name} has the same value.
#'   \item \code{strata} are merged when their element \code{stratumName} has the same value.
#'   \item \code{stratumObservations} are merged when both their \code{stratumID} and \code{plotObservationID} elements have the same value
#'   \item \code{aggregateOrganismObservations} are merged when their \code{plotObservationID} and \code{organismIdentityID} (and \code{stratumObservationID}, if defined) have the same value
#'   \item \code{individualOrganisms} are merged when both their \code{plotID} and \code{individualOrganismLabel} have the same value
#'   \item \code{individualOrganismObservations} are merged when both their \code{plotObservationID} and \code{individualOrganismID} have the same value.
#'   \item \code{surfaceTypes} are merged when their element \code{surfaceName} has the same value
#'   \item \code{surfaceCoverObservations} are merged when both their \code{surfaceTypeID} and \code{plotObservationID} elements have the same value
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
#' taxmapping = list(plotName = "Plot", obsStartDate = "PlotObsStartDate", organismName = "PreferredSpeciesName",
#'               stratumName = "Tier", cover = "Category")
#' coverscale = defineOrdinalScaleMethod(name = "Recce cover scale",
#'                    description = "Recce recording method by Hurst/Allen",
#'                    subject = "plant cover",
#'                    citation = "Hurst, JM and Allen, RB. (2007) The Recce method for describing New Zealand vegetation – Field protocols. Landcare Research, Lincoln.",
#'                    codes = c("P","1","2","3", "4", "5", "6"),
#'                    quantifiableCodes = c("1","2","3", "4", "5", "6"),
#'                    breaks = c(0, 1, 5, 25, 50, 75, 100),
#'                    midPoints = c(0.05, 0.5, 15, 37.5, 62.5, 87.5),
#'                    definitions = c("Presence", "<1%", "1-5%","6-25%", "26-50%", "51-75%", "76-100%"))
#' strataDef = defineMixedStrata(name = "Recce strata",
#'                               description = "Standard Recce stratum definition",
#'                               citation = "Hurst, JM and Allen, RB. (2007) The Recce method for describing New Zealand vegetation – Field protocols. Landcare Research, Lincoln.",
#'                               heightStrataBreaks = c(0, 0.3,2.0,5, 12, 25, 50),
#'                               heightStrataNames = paste0("Tier ",1:6),
#'                               categoryStrataNames = "Tier 7",
#'                               categoryStrataDefinition = "Epiphytes")
#' x = addAggregateOrganismObservations(newVegX(), moki_tcv, taxmapping,
#'                         methods = c(cover=coverscale),
#'                         stratumDefinition = strataDef)
#'
#' # Create document 'y' with tree observations
#' treemapping = list(plotName = "Plot", subPlotName = "Subplot", obsStartDate = "PlotObsStartDate",
#'                    organismName = "PreferredSpeciesName", diameterMeasurement = "Diameter")
#' diamMeth = predefinedMeasurementMethod("DBH/cm")
#' y = addIndividualOrganismObservations(newVegX(), moki_dia, treemapping,
#'                         methods = c(diameterMeasurement = diamMeth))
#'
#' # Merge 'x' and 'y' while keeping organism identities that have the same name separated
#' z1 = mergeVegX(x,y)
#' summary(z1)
#'
#' # Merge 'x' and 'y' while forcing organism identities that have the same name to
#' # be merged
#' z2 = mergeVegX(x,y, mergeIdentities = TRUE)
#' summary(z2)
#'
mergeVegX<-function(x, y, mergeIdentities = FALSE, verbose = TRUE) {

  # uses 'x' as the target and 'y' as the source of data
  
  # literatureCitations
  lcIDmap = list()
  nmergedlits = 0
  if(length(y@literatureCitations)>0) {
    for(j in 1:length(y@literatureCitations)) {
      nlcid = .newLiteratureCitationIDByCitationString(x, y@literatureCitations[[j]]$citationString)
      if(nlcid$new) {
        x@literatureCitations[[nlcid$id]] = y@literatureCitations[[j]]
      } else {
        x@literatureCitations[[nlcid$id]] = .mergeLiteratureCitations(x@literatureCitations[[nlcid$id]], y@literatureCitations[[j]])
      }
    }
  }
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
        if(length(attIDs)>0) {
          for(i in 1:length(attIDs)) {
            newAttID = .nextAttributeID(x)
            x@attributes[[newAttID]] = y@attributes[[attIDs[i]]]
            x@attributes[[newAttID]]$methodID = nmetid$id
            attIDmap[[attIDs[i]]] = newAttID
          }
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


  #organismIdentities
  oiIDmap = list()
  nmergedois = 0
  if(length(y@organismIdentities)>0) {
    for(j in 1:length(y@organismIdentities)) {
      if(mergeIdentities) {
        noiid = .newOrganismIdentityIDByName(x, y@organismIdentities[[j]]$organismName)
        if(noiid$new) {
          x@organismIdentities[[noiid$id]] = y@organismIdentities[[j]]
        } else { #pool information
          x@organismIdentities[[noiid$id]] = .mergeOrganismIdentities(x@organismIdentities[[noiid$id]], y@organismIdentities[[j]])
          nmergedois = nmergedois + 1
        }
        oiIDmap[names(y@organismIdentities)[j]] = noiid$id
      } else {
        newOIID = .nextOrganismIdentityID(x)
        x@organismIdentities[[newOIID]] = y@organismIdentities[[j]]
      }
    }
  }
  if(verbose) {
    cat(paste0(" Final number of organism identities: ", length(x@organismIdentities),". Data pooled for ", nmergedois, " organism identitie(s).\n"))
  }



  #strata
  strIDmap = list()
  nmergedstr = 0
  if(length(y@strata)>0) {
    for(j in 1:length(y@strata)) {
      methodID = methodIDmap[[y@strata[[j]]$methodID]]
      y@strata[[j]]$methodID = methodID # set method ID to translated one in order to avoid matching problems (does not change id externally)
      nstrid = .newStratumIDByName(x, methodID, y@strata[[j]]$stratumName)
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


  #projects IMPORTANT: Should be modified if other elements than 'title' are considered
  projectIDmap = list()
  if(length(y@projects)>0) {
    for(j in 1:length(y@projects)) {
      npid = .newProjectIDByTitle(x, y@projects[[j]]$title)
      if(npid$new) {
        x@projects[[npid$id]] = y@projects[[j]]
      }
      projectIDmap[names(y@projects)[j]] = npid$id
    }
  }
  if(verbose) {
    cat(paste0(" Final number of projects: ", length(x@projects),".\n"))
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
        x@stratumObservations[[nstrobsid$id]] = .applyAttributeMappingToStratumObservations(y@stratumObservations[[j]], attIDmap)
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
      oiID = oiIDmap[[y@aggregateObservations[[j]]$organismIdentityID]]
      y@aggregateObservations[[j]]$plotObservationID = plotObsID # set plot observation ID to translated one in order to avoid matching problems (does not change id externally)
      y@aggregateObservations[[j]]$organismIdentityID = oiID # set oi ID to translated one in order to avoid matching problems (does not change id externally)
      strObsID = ""
      if("stratumObservationID" %in% names(y@aggregateObservations[[j]])) {
        if(y@aggregateObservations[[j]]$stratumObservationID!="") {
          strObsID = strObsIDmap[[y@aggregateObservations[[j]]$stratumObservationID]]
          y@aggregateObservations[[j]]$stratumObservationID = strObsID
        }
      }
      naggobsid = .newAggregateOrganismObservationIDByOrganismIdentityID(x, plotObsID, strObsID, oiID)
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
      oiID = oiIDmap[[y@individualOrganisms[[j]]$organismIdentityID]]
      y@individualOrganisms[[j]]$plotID = plotID # set plot ID to translated one in order to avoid matching problems (does not change id externally)
      y@individualOrganisms[[j]]$organismIdentityID = oiID # set oi ID to translated one in order to avoid matching problems (does not change id externally)
      nindid = .newIndividualOrganismIDByIndividualOrganismLabel(x, plotID, y@individualOrganisms[[j]]$individualOrganismLabel)
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

  #surfaceTypes
  stIDmap = list()
  nmergedst = 0
  if(length(y@surfaceTypes)>0) {
    for(j in 1:length(y@surfaceTypes)) {
      methodID = methodIDmap[[y@surfaceTypes[[j]]$methodID]]
      y@surfaceTypes[[j]]$methodID = methodID # set method ID to translated one in order to avoid matching problems (does not change id externally)
      nstid = .newSurfaceTypeIDByName(x, methodID, y@surfaceTypes[[j]]$surfaceName)
      if(nstid$new) {
        x@surfaceTypes[[nstid$id]] = y@surfaceTypes[[j]]
      } else { #pool information
        x@surfaceTypes[[nstid$id]] = .mergeStrata(x@surfaceTypes[[nstrid$id]], y@surfaceTypes[[j]])
        nmergedst = nmergedst + 1
      }
      stIDmap[names(y@surfaceTypes)[j]] = nstid$id
    }
  }
  if(verbose) {
    cat(paste0(" Final number of surface types: ", length(x@surfaceTypes),". Data pooled for ", nmergedst, " surface types.\n"))
  }

  # surfaceCoverObservations
  scObsIDmap = list()
  nmergedscobs = 0
  if(length(y@surfaceCoverObservations)>0) {
    for(j in 1:length(y@surfaceCoverObservations)) {
      surfaceTypeID = stIDmap[[y@surfaceCoverObservations[[j]]$surfaceTypeID]]
      plotObsID = plotObsIDmap[[y@surfaceCoverObservations[[j]]$plotObservationID]]
      y@surfaceCoverObservations[[j]]$surfaceTypeID = surfaceTypeID # set surface ID to translated one in order to avoid matching problems (does not change id externally)
      y@surfaceCoverObservations[[j]]$plotObservationID = plotObsID # set plot observation ID to translated one in order to avoid matching problems (does not change id externally)
      nscobsid = .newSurfaceCoverObsIDByIDs(x, plotObsID, surfaceTypeID)
      if(nscobsid$new) {
        x@surfaceCoverObservations[[nscobsid$id]] = .applyAttributeMappingToSurfaceCoverObservations(y@surfaceCoverObservations[[j]], attIDmap)
      } else { #pool information
        x@surfaceCoverObservations[[nscobsid$id]] = .mergeSurfaceCoverObservations(x@surfaceCoverObservations[[nstrobsid$id]], y@surfaceCoverObservations[[j]], attIDmap)
        nmergedscobs = nmergedscobs + 1
      }
      scObsIDmap[names(y@surfaceCoverObservations)[j]] = nscobsid$id
    }
  }
  if(verbose) {
    cat(paste0(" Final number of surface cover observations: ", length(x@surfaceCoverObservations),". Data pooled for ", nmergedscobs, " surface cover observation(s).\n"))
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

  #                         surfaceCovers = "list",
  #                         surfaceCoverObservations = "list",

  #                         vegetationObservations = "list",
  return(x)
}
