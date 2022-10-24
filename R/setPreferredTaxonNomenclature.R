#' Sets preferred taxon nomenclature
#'
#' Sets (or resets) a preferred taxon nomenclature to the organism identities of a data set using a lookup table
#'
#' @param target The initial object of class \code{\linkS4class{VegX}} to be modified
#' @param x A data frame where each row corresponds to a different value of 'originalOrganismName'.
#' @param mapping A named list whose elements are strings that correspond to column names in \code{x}. Names of the list should be:
#'  \itemize{
#'    \item{\code{originalOrganismName} - A string with the original name given by the author of the data set (required).}
#'    \item{\code{preferredTaxonName} - A string with the preferred taxon name forming the taxon (required).}
#'    \item{\code{interpretationSource} - A string describing the source for the last nomenclature interpretation applied to this organism identity (i.e. the Plant List). (optional).}
#'    \item{\code{interpretationCitation} - A string of the publication where nomenclature interpretation is explained (optional).}
#'    \item{\code{interpretationDate} - Date of taxon nomenclature interpretation (see \code{date.format}) (optional).}
#'    \item{\code{interpretationParty} - Name of the party that undertook nomenclature interpretation (optional).}
#'  }
#' @param date.format A character string specifying the input format of dates (see \code{\link{as.Date}}).
#' @param missing.values A character vector of values that should be considered as missing data (see details).
#' @param verbose A boolean flag to indicate console output of the nomenclatural change process.
#'
#' @return The modified object of class \code{\linkS4class{VegX}}.
#'
#' @family organism identity functions
#'
#' @examples
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
#'                        The Recce method for describing New Zealand vegetation – Field protocols.
#'                        Landcare Research, Lincoln.",
#'                    codes = c("P","1","2","3", "4", "5", "6"),
#'                    quantifiableCodes = c("1","2","3", "4", "5", "6"),
#'                    breaks = c(0, 1, 5, 25, 50, 75, 100),
#'                    midPoints = c(0.05, 0.5, 15, 37.5, 62.5, 87.5),
#'                    definitions = c("Presence", "<1%", "1-5%","6-25%", "26-50%",
#'                                    "51-75%", "76-100%"))
#' strataDef = defineMixedStrata(name = "Recce strata",
#'                    description = "Standard Recce stratum definition",
#'                    citation = "Hurst, JM and Allen, RB. (2007)
#'                      The Recce method for describing New Zealand vegetation – Field protocols.
#'                      Landcare Research, Lincoln.",
#'                    heightStrataBreaks = c(0, 0.3,2.0,5, 12, 25, 50),
#'                    heightStrataNames = paste0("Tier ",1:6),
#'                    categoryStrataNames = "Tier 7",
#'                    categoryStrataDefinition = "Epiphytes")
#' x = addAggregateOrganismObservations(newVegX(), moki_tcv,
#'                         mapping = mapping,
#'                         methods = c(cover=coverscale),
#'                         stratumDefinition = strataDef)
#'
#' # Inspect the original organism identities
#' head(showElementTable(x, "organismIdentity"))
#'
#' y = setPreferredTaxonNomenclature(x, moki_lookup,
#'            c(originalOrganismName = "NVSSpeciesName", preferredTaxonName = "PreferredSpeciesName"))
#'
#' # Inspect the modified organism identities
#' head(showElementTable(y, "organismIdentity"))
#'
#' @export
setPreferredTaxonNomenclature<-function(target, x, mapping,
                                        date.format = "%Y-%m-%d",
                                        missing.values = c(NA, ""),
                                        verbose = TRUE) {

  if(!inherits(target, "VegX")) stop("Wrong class for 'target'. Should be an object of class 'VegX'")

  if(!(("originalOrganismName" %in% names(mapping)) && ("preferredTaxonName" %in% names(mapping)))) {
    stop(paste0("Mapping should include 'originalOrganismName' and 'preferredTaxonName'."))
  }
  #Check columns exist
  for(i in 1:length(mapping)) {
    if(!(mapping[i] %in% names(x))) stop(paste0("Variable '", mapping[i],"' not found in column names. Revise mapping or data."))
  }

  originalOrganismNames = as.character(x[[mapping[["originalOrganismName"]]]])
  preferredTaxonNames = as.character(x[[mapping[["preferredTaxonName"]]]])

  interpretationSourceFlag = ("interpretationSource" %in% names(mapping))
  if(interpretationSourceFlag) {
    interpretationSources = as.character(x[[mapping[["interpretationSource"]]]])
  }
  interpretationDateFlag = ("interpretationDate" %in% names(mapping))
  if(interpretationDateFlag) {
    interpretationDates = as.Date(as.character(x[[mapping[["interpretationDate"]]]]), format =date.format)
  }
  interpretationCitationFlag = ("interpretationCitation" %in% names(mapping))
  if(interpretationCitationFlag) {
    interpretationCitations = as.character(x[[mapping[["interpretationCitation"]]]])
  }
  interpretationPartyFlag = ("interpretationParty" %in% names(mapping))
  if(interpretationPartyFlag) {
    interpretationParties = as.character(x[[mapping[["interpretationParty"]]]])
  }

  if(length(target@organismIdentities)==0) stop(paste0("No organism identities in object 'target'."))

  nsetidentities = 0
  ntransfidentities = 0
  orinpts = length(target@parties)
  orinlcs = length(target@literatureCitations)
  orinons = length(target@organismNames)
  for(i in 1:length(target@organismIdentities)) {
     orgId = target@organismIdentities[[i]]
     oriOrgName = target@organismNames[[orgId$originalOrganismNameID]]
     if(oriOrgName$name %in% originalOrganismNames) {
       id = which(originalOrganismNames==oriOrgName$name)[1] # Get the first occurrence only
       # Check if the preferred name has to be added to the list of names
       nonid = .newOrganismNameIDByName(target, preferredTaxonNames[id], TRUE)
       if(nonid$new) {
         target@organismNames[[nonid$id]] = list("name" = preferredTaxonNames[id],
                                            "taxon" = TRUE)
       }
       # Set link to preferred name
       if(nonid$id != orgId$originalOrganismNameID) ntransfidentities = ntransfidentities + 1
       if(!("preferredTaxonNomenclature" %in% names(orgId))) orgId$preferredTaxonNomenclature = list()
       orgId$preferredTaxonNomenclature$preferredTaxonNameID = nonid$id

       if(interpretationSourceFlag) {
           if(!(interpretationSources[id] %in% missing.values)) {# If interpretation source is missing do not use
             orgId$preferredTaxonNomenclature$interpretationSource = interpretationSources[id]
           }
       }
       if(interpretationDateFlag) {
         if(!(interpretationDates[id] %in% missing.values)) {# If interpretation date is missing do not use
           orgId$preferredTaxonNomenclature$interpretationDate = interpretationDates[id]
         }
       }
       if(interpretationCitationFlag) {
         if(!(interpretationCitations[id] %in% missing.values)) {# If interpretation citation is missing do not use
           ncitid = .newLiteratureCitationIDByCitationString(target,interpretationCitations[id])
           citID = ncitid$id
           if(ncitid$new) {
             target@literatureCitations[[citID]] = list(citationString = interpretationCitations[id])
           }
           orgId$preferredTaxonNomenclature$interpretationCitationID = citID
         }
       }
       if(interpretationPartyFlag) {
         if(!(interpretationParties[id] %in% missing.values)) { # interpretation party is missing do not use
           npid = .newPartyIDByName(target, interpretationParties[id])
           partyID = npid$id
           if(npid$new) target@parties[[partyID]] = list(name = interpretationParties[id],
                                                         partyType = "individual")
           orgId$preferredTaxonNomenclature$interpretationPartyID = partyID
         }
       }


       nsetidentities = nsetidentities + 1
     }
     target@organismIdentities[[i]] = orgId
  }
  finnons = length(target@organismNames)
  finnpts = length(target@parties)
  finnlcs = length(target@literatureCitations)
  if(verbose) {
    if(nsetidentities > 0) cat(paste0(" Preferred taxon name was set on ", nsetidentities, " organism identities.\n"))
    if(ntransfidentities > 0) cat(paste0(" Preferred taxon name is now different than original organism name on ", ntransfidentities, " organism identities.\n"))
    if(finnons > orinons) cat(paste0(" " , finnons-orinons, " new organism name(s) added.\n"))
    if(finnlcs > orinlcs) cat(paste0(" ", finnlcs-orinlcs, " new literature citation(s) added.\n"))
    if(finnpts > orinpts) cat(paste0(" ", finnpts-orinpts, " new party(ies) added.\n"))
  }

  return(target)
}
