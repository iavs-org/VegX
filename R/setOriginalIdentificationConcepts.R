#' Sets original identification taxon concepts
#'
#' Sets (or resets) taxon concepts associated with the original identification of organisms
#'
#' @param target The initial object of class \code{\linkS4class{VegX}} to be modified
#' @param x A data frame where each row corresponds to one organism identity, given by a column that can be mapped to \code{originalOrganismName}.
#' @param mapping A named list whose elements are strings that correspond to column names in \code{x}. Names of the list should be:
#'  \itemize{
#'    \item{\code{originalOrganismName} - A string with the original name given by the author of the data set (required).}
#'    \item{\code{conceptName} - A string with the taxon name forming the taxon concept, if different from \code{originalOrganismName} (optional).}
#'    \item{\code{conceptCitation} - A string with the bibliographic citation forming the taxon concept (optional).}
#'    \item{\code{assertionDate} - Date of taxon concept assertion (see \code{date.format}) (optional).}
#'    \item{\code{assertionParty} - Name of the party that undertook taxon concept assertion (optional).}
#'  }
#' @param citationStringAll A string with the bibliographic citation to be applied to all organism identities of the VegX object (using the original organism names as taxon names),
#' or to all original organism names listed in \code{x}.
#' @param date.format A character string specifying the input format of dates (see \code{\link{as.Date}}).
#' @param missing.values A character vector of values that should be considered as missing data (see details).
#' @param verbose A boolean flag to indicate console output of the concept identification process.
#'
#' @return The modified object of class \code{\linkS4class{VegX}}.
#'
#' @references Wiser SK, Spencer N, De Caceres M, Kleikamp M, Boyle B & Peet RK (2011). Veg-X - an exchange standard for plot-based vegetation data
#'
#' @family organism identity functions
#'
#' @examples
#'
#' # Load source data
#' data(mokihinui)
#'
#' # Create new Veg-X document with aggregate organism observations
#' mapping = list(plotName = "Plot", obsStartDate = "PlotObsStartDate",
#'                taxonName = "NVSSpeciesName",
#'                stratumName = "Tier", cover = "Category")
#' coverscale = defineOrdinalScaleMethod(name = "Recce cover scale",
#'                    description = "Recce recording method by Hurst/Allen",
#'                    subject = "plant cover",
#'                    citation = "Hurst, JM and Allen, RB. (2007)
#'                         The Recce method for describing New Zealand vegetation – Field protocols.
#'                         Landcare Research, Lincoln.",
#'                    codes = c("P","1","2","3", "4", "5", "6"),
#'                    quantifiableCodes = c("1","2","3", "4", "5", "6"),
#'                    breaks = c(0, 1, 5, 25, 50, 75, 100),
#'                    midPoints = c(0.05, 0.5, 15, 37.5, 62.5, 87.5),
#'                    definitions = c("Presence", "<1%", "1-5%","6-25%", "26-50%",
#'                                   "51-75%", "76-100%"))
#' strataDef = defineMixedStrata(name = "Recce strata",
#'                    description = "Standard Recce stratum definition",
#'                    citation = "Hurst, JM and Allen, RB. (2007)
#'                       The Recce method for describing New Zealand vegetation – Field protocols.
#'                       Landcare Research, Lincoln.",
#'                    heightStrataBreaks = c(0, 0.3,2.0,5, 12, 25, 50),
#'                    heightStrataNames = paste0("Tier ",1:6),
#'                    categoryStrataNames = "Tier 7",
#'                    categoryStrataDefinition = "Epiphytes")
#' x = addAggregateOrganismObservations(newVegX(), moki_tcv,
#'                    mapping = mapping,
#'                    methods = c(cover=coverscale),
#'                    stratumDefinition = strataDef)
#'
#' # Inspect the original organism identities
#' head(showElementTable(x, "organismIdentity"))
#'
#' y = setOriginalIdentificationConcepts(x, citationStringAll="Allen 1998")
#'
#' head(showElementTable(y, "organismIdentity"))

setOriginalIdentificationConcepts<-function(target, citationStringAll = "",
                           x = NULL, mapping = list(),
                           date.format = "%Y-%m-%d",
                           missing.values = c(NA, ""),
                           verbose = TRUE) {
  if(class(target)!="VegX") stop("Wrong class for 'target'. Should be an object of class 'VegX'")

  if(is.null(x) && citationStringAll!="") {
    #Adds literature citation
    ncitid = .newLiteratureCitationIDByCitationString(target, citationStringAll)
    if(ncitid$new) {
      target@literatureCitations[[ncitid$id]] = list(citationString = citationStringAll)
    }

    #Reset taxon concepts
    orinlcs = length(target@literatureCitations)
    orintcs = length(target@taxonConcepts)
    target@taxonConcepts = list()

    #Adds taxon concepts
    if(length(target@organismIdentities)>0) {
      for(i in 1:length(target@organismIdentities)) {
        onID = target@organismIdentities[[i]]$originalOrganismNameID
        organismName = target@organismNames[[onID]]
        if(length(organismName)>1) organismName = organismName$name
        ntcid = .newTaxonConceptIDByNameCitation(target, organismName, citationStringAll) # Get the new taxon concept ID (internal code)
        tcID = ntcid$id
        if(ntcid$new) {
          target@taxonConcepts[[tcID]] = list("organismNameID" = onID,
                                              "accordingToCitationID" = ncitid$id)
        }
        target@organismIdentities[[i]]$originalIdentificationConcept$taxonConceptID = tcID
      }
    }
    finnlcs = length(target@literatureCitations)
    finntcs = length(target@taxonConcepts)
    if(verbose) {
      cat(paste0(" ", orintcs, " taxon concept(s) reset. ", finntcs-orintcs, " new taxon concept(s) added.\n"))
      cat(paste0(" ", finnlcs-orinlcs, " new literature citation(s) added.\n"))
    }
  }
  else {
    x = as.data.frame(x)
    availableMapping = c("originalOrganismName", "conceptName", "conceptCitation",
                         "assertionDate", "assertionParty")

    #Check columns exist
    for(i in 1:length(mapping)) {
      if(names(mapping)[i] %in% availableMapping) if(!(mapping[i] %in% names(x))) stop(paste0("Variable '", mapping[i],"' not found in column names. Revise mapping or data."))
    }

    originalOrganismNames = as.character(x[[mapping[["originalOrganismName"]]]])
    #Optional mappings
    conceptNameFlag = ("conceptName" %in% names(mapping))
    if(conceptNameFlag) {
      conceptNames = as.character(x[[mapping[["conceptName"]]]])
    }
    conceptCitationFlag = ("conceptCitation" %in% names(mapping))
    if(conceptCitationFlag) {
      conceptCitations = as.character(x[[mapping[["conceptCitation"]]]])
    } else {
      if(citationStringAll!="") {
        conceptCitations = rep(citationStringAll, length(originalOrganismNames))
      } else {
        stop("Supply mapping for 'conceptCitation' or suitable string for parameter 'citationStringAll'.")
      }
    }
    assertionDateFlag = ("assertionDate" %in% names(mapping))
    if(assertionDateFlag) {
      assertionDates = as.Date(as.character(x[[mapping[["assertionDate"]]]]), format =date.format)
    }
    assertionPartyFlag = ("assertionParty" %in% names(mapping))
    if(assertionPartyFlag) {
      assertionParties = as.character(x[[mapping[["assertionParty"]]]])
    }

    #Parse lookupTable
    resetCounter = 0
    orinpts = length(target@parties)
    orinlcs = length(target@literatureCitations)
    orintcs = length(target@taxonConcepts)
    for(i in 1:length(originalOrganismNames)) {
      originalOrganismName = originalOrganismNames[i]
      nonid = .newOrganismNameIDByName(target, originalOrganismName, TRUE)
      if(!nonid$new) { #If not new
        onID = nonid$id
        oiIDs = .getOrganismIdentityIDsByOriginalOrganismNameID(target, onID)
        if(length(oiIDs)>0) {

          if(conceptNameFlag) {
            conceptName = conceptNames[i]
            tnid = .newOrganismNameIDByName(target, conceptName, TRUE) # Get the new taxon name usage ID (internal code)
            tnID = tnid$id
            if(tnid$new) target@organismNames[[tnID]] = list("name" = conceptName, "taxon" = TRUE)
          } else {
            conceptName = originalOrganismName
            tnID = onID #If taxon names not supplied, use original organism name as taxon concept name
          }

          #citation string
          if(!(conceptCitations[i] %in% missing.values)) { # If citation string is missing use the previous one
            conceptCitation = conceptCitations[i]
            ncitid = .newLiteratureCitationIDByCitationString(target,conceptCitation)
            citID = ncitid$id
            if(ncitid$new) {
              target@literatureCitations[[citID]] = list(citationString = conceptCitation)
            }
          }

          #taxon concept
          ntcid = .newTaxonConceptIDByNameCitation(target, conceptName, conceptCitation) # Get the new taxon concept ID (internal code)
          tcID = ntcid$id
          if(ntcid$new) {
            target@taxonConcepts[[tcID]] = list("organismNameID" = tnID, "accordingToCitationID" = citID)
          }

          #assertionParty
          partyID = NULL
          if(assertionPartyFlag) {
            if(!(assertionParties[i] %in% missing.values)) { # assertion party is missing do not use
              npid = .newPartyIDByName(target, assertionParties[i])
              partyID = npid$id
              if(npid$new) target@parties[[partyID]] = list(name = assertionParties[i],
                                                            partyType = "individual")
            }
          }

          #assertionDate
          assertionDate = NULL
          if(assertionDateFlag) {
            if(!(assertionDates[i] %in% missing.values)) {# If assertion date is missing do not use
              assertionDate = assertionDates[i]
            }
          }

          for(j in 1:length(oiIDs)) {
            resetCounter = resetCounter + 1
            target@organismIdentities[[oiIDs[j]]]$originalIdentificationConcept$taxonConceptID = tcID
            if(!is.null(partyID)) target@organismIdentities[[oiIDs[j]]]$originalIdentificationConcept$assertionPartyID = partyID
            if(!is.null(assertionDate)) target@organismIdentities[[oiIDs[j]]]$originalIdentificationConcept$assertionDate = assertionDate
          }
        }
      }
    }
    finnpts = length(target@parties)
    finntcs = length(target@taxonConcepts)
    finnlcs = length(target@literatureCitations)
    if(verbose) {
      cat(paste0(" Taxon concept (re)set for ", resetCounter, " organism identity(ies) .\n"))
      if(finntcs > orintcs) cat(paste0(" ", finntcs-orintcs, " new taxon concept(s) added.\n"))
      if(finnlcs > orinlcs) cat(paste0(" ", finnlcs-orinlcs, " new literature citation(s) added.\n"))
      if(finnpts > orinpts) cat(paste0(" ", finnpts-orinpts, " new party(ies) added.\n"))
    }
  }

  return(target)
}
