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
#' taxmapping = list(plotName = "Plot", obsStartDate = "PlotObsStartDate",
#'               taxonName = "NVSSpeciesName",
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
#'                    description = "Standard Recce stratum definition",
#'                    citation = "Hurst, JM and Allen, RB. (2007) The Recce method for describing New Zealand vegetation – Field protocols. Landcare Research, Lincoln.",
#'                    heightStrataBreaks = c(0, 0.3,2.0,5, 12, 25, 50),
#'                    heightStrataNames = paste0("Tier ",1:6),
#'                    categoryStrataNames = "Tier 7",
#'                    categoryStrataDefinition = "Epiphytes")
#' x = addAggregateOrganismObservations(newVegX(), moki_tcv, taxmapping,
#'                    methods = c(cover=coverscale),
#'                    stratumDefinition = strataDef)
#'
#' # Create document 'y' with tree observations
#' treemapping = list(plotName = "Plot", subPlotName = "Subplot", obsStartDate = "PlotObsStartDate",
#'                    taxonName = "NVSSpeciesName", diameterMeasurement = "Diameter")
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

  # parties
  partyIDmap = list()
  nmergedparties = 0
  if(length(y@parties)>0) {
    for(j in 1:length(y@parties)) {
      nptid = .newPartyIDByName(x, y@parties[[j]]$name)
      if(nptid$new) {
        x@parties[[nptid$id]] = y@parties[[j]]
      } else {
        x@parties[[nptid$id]] = .mergeParties(x@parties[[nlcid$id]], y@parties[[j]])
        nmergedparties = nmergedparties + 1
      }
      partyIDmap[names(y@parties)[j]] = nptid$id
    }
  }
  if(verbose) {
    cat(paste0(" Final number of partie(s): ", length(x@parties),". Data pooled for ", nmergedparties, " partie(s).\n"))
  }

  # literatureCitations
  litIDmap = list()
  nmergedlits = 0
  if(length(y@literatureCitations)>0) {
    for(j in 1:length(y@literatureCitations)) {
      nlcid = .newLiteratureCitationIDByCitationString(x, y@literatureCitations[[j]]$citationString)
      if(nlcid$new) {
        x@literatureCitations[[nlcid$id]] = y@literatureCitations[[j]]
      } else {
        x@literatureCitations[[nlcid$id]] = .mergeLiteratureCitations(x@literatureCitations[[nlcid$id]], y@literatureCitations[[j]])
        nmergedlits = nmergedlits + 1
      }
      litIDmap[names(y@literatureCitations)[j]] = nlcid$id
    }
  }
  if(verbose) {
    cat(paste0(" Final number of literature citations: ", length(x@literatureCitations),". Data pooled for ", nmergedlits, " citation(s).\n"))
  }

  # methods/attributes
  methodIDmap = list()
  attIDmap = list()
  nmergedmeths = 0
  if(length(y@methods)>0) {
    for(j in 1:length(y@methods)) {
      nmetid = .newMethodIDByName(x, y@methods[[j]]$name)
      if(nmetid$new) {
        x@methods[[nmetid$id]] = .applyMappingsToMethod(y@methods[[j]], litIDmap)
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
        # add attributes
        xattIDs = .getAttributeIDsByMethodID(x, names(x@methods)[j]) #Get attribute IDs in 'x'
        yattIDs = .getAttributeIDsByMethodID(y, names(y@methods)[j]) #Get attribute IDs in 'y'
        if(length(xattIDs)>0) {
          for(i in 1:length(xattIDs)) {
            attIDmap[[yattIDs[i]]] = xattIDs[i]
          }
        }
        x@methods[[nmetid$id]] = .mergeMethods(x@methods[[nmetid$id]], y@methods[[j]], litIDmap)
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
        x@surfaceTypes[[nstid$id]] = .mergeStrata(x@surfaceTypes[[nstid$id]], y@surfaceTypes[[j]])
        nmergedst = nmergedst + 1
      }
      stIDmap[names(y@surfaceTypes)[j]] = nstid$id
    }
  }
  if(verbose) {
    cat(paste0(" Final number of surface types: ", length(x@surfaceTypes),". Data pooled for ", nmergedst, " surface types.\n"))
  }

  #organismNames
  onIDmap = list()
  nmergedons = 0
  if(length(y@organismNames)>0) {
    for(j in 1:length(y@organismNames)) {
      nonid = .newOrganismNameIDByName(x, y@organismNames[[j]]$name, y@organismNames[[j]]$taxon)
      if(nonid$new) {
        x@organismNames[[nonid$id]] = y@organismNames[[j]]
      } else { #pool information
        x@organismNames[[nonid$id]] = .mergeOrganismNames(x@organismNames[[nonid$id]], y@organismNames[[j]])
        nmergedons = nmergedons + 1
      }
      onIDmap[names(y@organismNames)[j]] = nonid$id
    }
  }
  if(verbose) {
    cat(paste0(" Final number of organism names: ", length(x@organismNames),". Data pooled for ", nmergedons, " organism name(s).\n"))
  }

  #taxonConcepts
  tcIDmap = list()
  nmergedtcs = 0
  if(length(y@taxonConcepts)>0) {
    for(j in 1:length(y@taxonConcepts)) {
      ntcid = .newTaxonConceptIDByNameCitation(x, y@organismNames[[y@taxonConcepts[[j]]$organismNameID]]$name,
                                               y@literatureCitations[[y@taxonConcepts[[j]]$citationID]]$citationString)
      if(ntcid$new) {
        x@taxonConcepts[[ntcid$id]] = .applyMappingsToTaxonConcept(y@taxonConcepts[[j]], onIDmap, litIDmap)
      } else { #pool information
        x@taxonConcepts[[ntcid$id]] = .mergeTaxonConcepts(x@taxonConcepts[[ntcid$id]], y@taxonConcepts[[j]], onIDmap, litIDmap)
        nmergedtcs = nmergedtcs + 1
      }
      tcIDmap[names(y@taxonConcepts)[j]] = ntcid$id
    }
  }
  if(verbose) {
    cat(paste0(" Final number of taxon concepts: ", length(x@taxonConcepts),". Data pooled for ", nmergedtcs, " organism name(s).\n"))
  }

  #organismIdentities
  oiIDmap = list()
  nmergedois = 0
  if(length(y@organismIdentities)>0) {
    for(j in 1:length(y@organismIdentities)) {
      if(mergeIdentities) {
        organismName = .getOrganismIdentityName(y, j)
        citationString = .getOrganismIdentityCitationString(y,j)
        orgId = .applyMappingsToOrganismIdentity(y@organismIdentities[[j]], onIDmap, tcIDmap)
        noiid = .newOrganismIdentityIDByTaxonConcept(x, organismName, citationString)
        if(noiid$new) {
          x@organismIdentities[[noiid$id]] = orgId
        } else { #pool information
          x@organismIdentities[[noiid$id]] = .mergeOrganismIdentities(x@organismIdentities[[noiid$id]], orgId)
          nmergedois = nmergedois + 1
        }
        oiIDmap[names(y@organismIdentities)[j]] = noiid$id
      } else {
        newOIID = .nextOrganismIdentityID(x)
        x@organismIdentities[[newOIID]] = y@organismIdentities[[j]]
        oiIDmap[names(y@organismIdentities)[j]] = newOIID
      }
    }
  }
  if(verbose) {
    cat(paste0(" Final number of organism identities: ", length(x@organismIdentities),". Data pooled for ", nmergedois, " organism identitie(s).\n"))
  }

  #projects
  nmergedpjs = 0
  projectIDmap = list()
  if(length(y@projects)>0) {
    for(j in 1:length(y@projects)) {
      npid = .newProjectIDByTitle(x, y@projects[[j]]$title)
      if(npid$new) {
        x@projects[[npid$id]] = .applyMappingsToProject(y@projects[[j]], partyIDmap, litIDmap)
      } else {
        x@projects[[npid$id]] = .mergeProjects(x@projects[[npid$id]], y@projects[[j]])
        nmergedpjs = nmergedpjs + 1

      }
      projectIDmap[names(y@projects)[j]] = npid$id
    }
  }
  if(verbose) {
    cat(paste0(" Final number of projects: ", length(x@projects),". Data pooled for ", nmergedpjs, " project(s).\n"))
  }

  #plots
  plotIDmap = list()
  nmergedplots = 0
  if(length(y@plots)>0) {
    for(j in 1:length(y@plots)) {
      if("parentPlotID" %in% names(y@plots[[j]])) y@plots[[j]]$parentPlotID = plotIDmap[[y@plots[[j]]$parentPlotID]] #set parent plot ID to translated one in order to avoid matching problems
      npid = .newPlotIDByName(x, y@plots[[j]]$plotName)
      if(npid$new) {
        x@plots[[npid$id]] = .applyMappingsToPlot(y@plots[[j]], partyIDmap, attIDmap)
      } else { #pool information
        x@plots[[npid$id]] = .mergePlots(x@plots[[npid$id]], y@plots[[j]], partyIDmap, attIDmap)
        nmergedplots = nmergedplots + 1
      }
      plotIDmap[names(y@plots)[j]] = npid$id
    }
  }
  if(verbose) {
    cat(paste0(" Final number of plots: ", length(x@plots),". Data pooled for ", nmergedplots, " plot(s).\n"))
  }

  # individualOrganisms
  indIDmap = list()
  nmergedind = 0
  if(length(y@individualOrganisms)>0) {
    for(j in 1:length(y@individualOrganisms)) {
      indOrg = .applyMappingsToIndividualOrganism(y@individualOrganisms[[j]], plotIDmap, oiIDmap)
      nindid = .newIndividualOrganismIDByIndividualOrganismLabel(x, indOrg$plotID, indOrg$individualOrganismLabel)
      if(nindid$new) {
        x@individualOrganisms[[nindid$id]] = indOrg
      } else { #pool information
        x@individualOrganisms[[nindid$id]] = .mergeIndividualOrganisms(x@individualOrganisms[[nindid$id]], indOrg)
        nmergedind = nmergedind + 1
      }
      indIDmap[names(y@individualOrganisms)[j]] = nindid$id
    }
  }
  if(verbose) {
    cat(paste0(" Final number of individual organisms: ", length(x@individualOrganisms),". Data pooled for ", nmergedind, " individual organism(s).\n"))
  }

  #plotObservations
  plotObsIDmap = list()
  nmergedplotobs = 0
  if(length(y@plotObservations)>0) {
    for(j in 1:length(y@plotObservations)) {
      plotObs = .applyMappingsToPlotObservation(y@plotObservations[[j]], plotIDmap, projectIDmap)
      npoid = .newPlotObsIDByDate(x, plotObs$plotID, plotObs$obsStartDate)
      if(npoid$new) {
        x@plotObservations[[npoid$id]] = plotObs
      } else { #pool information
        x@plotObservations[[npoid$id]] = .mergePlotObservations(x@plotObservations[[npoid$id]], plotObs)
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
      strObs = .applyMappingsToStratumObservation(y@stratumObservations[[j]], strIDmap, plotObsIDmap, attIDmap)
      nstrobsid = .newStratumObsIDByIDs(x, strObs$plotObservationID, strObs$stratumID)
      if(nstrobsid$new) {
        x@stratumObservations[[nstrobsid$id]] = strObs
      } else { #pool information
        x@stratumObservations[[nstrobsid$id]] = .mergeStratumObservations(x@stratumObservations[[nstrobsid$id]], strObs)
        nmergedstrobs = nmergedstrobs + 1
      }
      strObsIDmap[names(y@stratumObservations)[j]] = nstrobsid$id
    }
  }
  if(verbose) {
    cat(paste0(" Final number of stratum observations: ", length(x@stratumObservations),". Data pooled for ", nmergedstrobs, " stratum observation(s).\n"))
  }

  # individualObservations
  indObsIDmap = list()
  nmergedindobs = 0
  if(length(y@individualObservations)>0) {
    for(j in 1:length(y@individualObservations)) {
      indObs = .applyMappingsToIndividualOrganismObservation(y@individualObservations[[j]], plotObsIDmap, strObsIDmap, indIDmap, attIDmap)
      nindobsid = .newIndividualOrganismObservationIDByIndividualID(x, indObs$plotObservationID, indObs$individualOrganismID)
      if(nindobsid$new) {
        x@individualObservations[[nindobsid$id]] = indObs
      } else { #pool information
        x@individualObservations[[nindobsid$id]] = .mergeIndividualOrganismObservations(x@individualObservations[[nindobsid$id]], indObs)
        nmergedindobs = nmergedindobs + 1
      }
      indObsIDmap[names(y@individualObservations)[j]] = nindobsid$id
    }
  }
  if(verbose) {
    cat(paste0(" Final number of individual organism observations: ", length(x@individualObservations),". Data pooled for ", nmergedindobs, " individual organism observation(s).\n"))
  }

  # aggregateObservations
  aggObsIDmap = list()
  nmergedaggobs = 0
  if(length(y@aggregateObservations)>0) {
    for(j in 1:length(y@aggregateObservations)) {
      aggObs = .applyMappingsToAggregateOrganismObservation(y@aggregateObservations[[j]], plotObsIDmap, oiIDmap, strObsIDmap, attIDmap)
      naggobsid = .newAggregateOrganismObservationIDByOrganismIdentityID(x,
                                                                         aggObs$plotObservationID,
                                                                         aggObs$stratumObservationID,
                                                                         aggObs$organismIdentityID)
      if(naggobsid$new) {
        x@aggregateObservations[[naggobsid$id]] = aggObs
      } else { #pool information
        x@aggregateObservations[[naggobsid$id]] = .mergeAggregateOrganismObservations(x@aggregateObservations[[naggobsid$id]], aggObs)
        nmergedaggobs = nmergedaggobs + 1
      }
      aggObsIDmap[names(y@aggregateObservations)[j]] = naggobsid$id
    }
  }
  if(verbose) {
    cat(paste0(" Final number of aggregate organism observations: ", length(x@aggregateObservations),". Data pooled for ", nmergedaggobs, " aggregate organism observation(s).\n"))
  }



  # siteObservations
  siteObsIDmap = list()
  nmergedsiteobs = 0
  if(length(y@siteObservations)>0) {
    for(j in 1:length(y@siteObservations)) {
      siteObs = .applyMappingsToSiteObservation(y@siteObservations[[j]], plotObsIDmap, attIDmap)
      nsiteobsid = .newSiteObservationIDByID(x, siteObs$plotObservationID)
      if(nsiteobsid$new) {
        x@siteObservations[[nsiteobsid$id]] = siteObs
      } else { #pool information
        x@siteObservations[[nsiteobsid$id]] = .mergeSiteObservations(x@siteObservations[[nsiteobsid$id]], siteObs)
        nmergedsiteobs = nmergedsiteobs + 1
      }
      siteObsIDmap[names(y@siteObservations)[j]] = nsiteobsid$id
      # Update link in plotObservation
      x@plotObservations[[x@siteObservations[[nsiteobsid$id]]$plotObservationID]]$siteObservationID = nsiteobsid$id
    }
  }
  if(verbose) {
    cat(paste0(" Final number of site observations: ", length(x@siteObservations),". Data pooled for ", nmergedsiteobs, " site observation(s).\n"))
  }

  # communityObservations
  commObsIDmap = list()
  nmergedcommobs = 0
  if(length(y@communityObservations)>0) {
    for(j in 1:length(y@communityObservations)) {
      commObs = .applyMappingsToCommunityObservation(y@communityObservations[[j]], plotObsIDmap, attIDmap)
      ncommobsid = .newCommunityObservationIDByID(x, commObs$plotObservationID)
      if(ncommobsid$new) {
        x@communityObservations[[ncommobsid$id]] = commObs
      } else { #pool information
        x@communityObservations[[ncommobsid$id]] = .mergeCommunityObservations(x@communityObservations[[ncommobsid$id]], commObs)
        nmergedcommobs = nmergedcommobs + 1
      }
      commObsIDmap[names(y@communityObservations)[j]] = ncommobsid$id
      # Update link in plotObservation
      x@plotObservations[[x@communityObservations[[ncommobsid$id]]$plotObservationID]]$communityObservationID = ncommobsid$id
    }
  }
  if(verbose) {
    cat(paste0(" Final number of community observations: ", length(x@communityObservations),". Data pooled for ", nmergedsiteobs, " community observation(s).\n"))
  }


  # surfaceCoverObservations
  scObsIDmap = list()
  nmergedscobs = 0
  if(length(y@surfaceCoverObservations)>0) {
    for(j in 1:length(y@surfaceCoverObservations)) {
      scObs = .applyMappingsToSurfaceCoverObservation(y@surfaceCoverObservations[[j]], stIDmap, plotObsIDmap, attIDmap)
      nscobsid = .newSurfaceCoverObsIDByIDs(x, scObs$plotObservationID, scObs$surfaceTypeID)
      if(nscobsid$new) {
        x@surfaceCoverObservations[[nscobsid$id]] = scObs
      } else { #pool information
        x@surfaceCoverObservations[[nscobsid$id]] = .mergeSurfaceCoverObservations(x@surfaceCoverObservations[[nstrobsid$id]], scObs)
        nmergedscobs = nmergedscobs + 1
      }
      scObsIDmap[names(y@surfaceCoverObservations)[j]] = nscobsid$id
    }
  }
  if(verbose) {
    cat(paste0(" Final number of surface cover observations: ", length(x@surfaceCoverObservations),". Data pooled for ", nmergedscobs, " surface cover observation(s).\n"))
  }

  return(x)
}
