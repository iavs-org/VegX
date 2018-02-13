#' Add taxon concepts
#'
#' Sets or resets taxon concepts associated with organism identities
#'  
#' @param target The initial object of class \code{\linkS4class{VegX}} to be modified
#' @param x A data frame where each row corresponds to one organism identity, given by a column that can be mapped to \code{originalOrganismName}.
#' @param mapping A named list whose elements are strings that correspond to column names in \code{x}. Names of the list should be:
#'  \itemize{
#'    \item{\code{originalOrganismName} - A string with the original name given by the author of the data set (required).}
#'    \item{\code{taxonName} - A string with the taxon name forming the taxon concept, if different from \code{originalOrganismName} (optional).}
#'    \item{\code{citationString} - A string with the bibliographic citation forming the taxon concept (optional).}
#'    \item{\code{assertionDate} - Date of taxon concept assertion (see \code{date.format}) (optional).}
#'    \item{\code{assertionParty} - Name of the party that undertook taxon concept assertion (optional).}
#'  }
#' @param citationStringAll A string with the bibliographic citation to be applied to all organism identities of the VegX object (using the original organism names as taxon names),
#' or to all original organism names listed in \code{x}.
#' @param date.format A character string specifying the input format of dates (see \code{\link{as.Date}}).
#' @param verbose A boolean flag to indicate console output of the data integration process.
#'
#' @return The modified object of class \code{\linkS4class{VegX}}.
#' @export
#' 
#' @references Wiser SK, Spencer N, De Caceres M, Kleikamp M, Boyle B & Peet RK (2011). Veg-X - an exchange standard for plot-based vegetation data
#'
#' @family add functions
#'
#' @examples
addTaxonConcepts<-function(target, citationStringAll = "", 
                           x = NULL, mapping = list(), 
                           date.format = "%Y-%m-%d",
                           missing.values = c(NA, "0", ""),
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
        ntcid = .newTaxonConceptIDByNameCitation(target, organismName, citationStringAll) # Get the new taxon concept ID (internal code)
        tcID = ntcid$id
        if(ntcid$new) {
          target@taxonConcepts[[tcID]] = list("organismNameID" = onID,
                                              "citationID" = ncitid$id)
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
    availableMapping = c("originalOrganismName", "taxonName", "citationString", 
                         "assertionDate", "assertionParty")
    
    #Check columns exist
    for(i in 1:length(mapping)) {
      if(names(mapping)[i] %in% availableMapping) if(!(mapping[i] %in% names(x))) stop(paste0("Variable '", mapping[i],"' not found in column names. Revise mapping or data."))
    }
    
    originalOrganismNames = as.character(x[[mapping[["originalOrganismName"]]]])
    #Optional mappings
    taxonNameFlag = ("taxonName" %in% names(mapping))
    if(taxonNameFlag) {
      taxonNames = as.character(x[[mapping[["taxonName"]]]])
    }
    citationStringFlag = ("citationString" %in% names(mapping))
    if(citationStringFlag) {
      citationStrings = as.character(x[[mapping[["citationString"]]]])
    } else {
      if(citationStringAll!="") {
        citationStrings = rep(citationStringAll, length(originalOrganismNames))
      } else {
        stop("Supply mapping for 'citationString' or suitable string for parameter 'citationStringAll'.")
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
          
          if(taxonNameFlag) {
            taxonName = taxonNames[i]
            tnid = .newOrganismNameIDByName(target, taxonName, TRUE) # Get the new taxon name usage ID (internal code)
            tnID = tnid$id
            if(tnid$new) target@organismNames[[tnID]] = list("name" = taxonName, "taxon" = TRUE)
          } else {
            taxonName = originalOrganismName
            tnID = onID #If taxon names not supplied, use original organism name as taxon concept name
          }
          
          #citation string
          if(!(citationStrings[i] %in% missing.values)) {
            citationString = citationStrings[i]
            ncitid = .newLiteratureCitationIDByCitationString(target,citationString)
            citID = ncitid$id
            if(ncitid$new) {
              target@literatureCitations[[citID]] = list(citationString = citationString)
            }
          }
          
          #taxon concept
          ntcid = .newTaxonConceptIDByNameCitation(target, taxonName, citationString) # Get the new taxon concept ID (internal code)
          tcID = ntcid$id
          if(ntcid$new) {
            target@taxonConcepts[[tcID]] = list("organismNameID" = tnID, "citationID" = citID)
          }
          
          #assertionParty
          if(assertionPartyFlag) {
            if(!(assertionParties[i] %in% missing.values)) {
              npid = .newPartyIDByName(target, assertionParties[i])
              partyID = npid$id
              if(npid$new) target@parties[[partyID]] = list(name = assertionParties[i],
                                                            partyType = "individual")
            } else {
              partyID = ""
            }
          } else {
            partyID = ""
          }
          
          for(j in 1:length(oiIDs)) {
            resetCounter = resetCounter + 1
            target@organismIdentities[[oiIDs[j]]]$originalIdentificationConcept$taxonConceptID = tcID
            if(partyID!="") target@organismIdentities[[oiIDs[j]]]$originalIdentificationConcept$assertionPartyID = partyID
          }
        }
      }
    }
    finnpts = length(target@parties)
    finntcs = length(target@taxonConcepts)
    finnlcs = length(target@literatureCitations)
    if(verbose) {
      cat(paste0(" Taxon concept set for ", resetCounter, " organism identity(ies) .\n"))
      cat(paste0(" ", finntcs-orintcs, " new taxon concept(s) added.\n"))
      cat(paste0(" ", finnlcs-orinlcs, " new literature citation(s) added.\n"))
      cat(paste0(" ", finnpts-orinpts, " party(ies) added.\n"))
    }
  }
  
  return(target)
}