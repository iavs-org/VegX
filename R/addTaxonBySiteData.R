#' Add a taxon by site table
#'
#' Adds the aggregate organism observations of a taxon by site table to an existing VegX object.
#'
#' @param target The object of class \code{\linkS4class{VegX}} to be modified.
#' @param x A taxon by site (plot) table (with plots in rows and taxa in columns) and with plant abundance values.
#' @param abundanceMethod A measurement method for aggregate plant abundance (an object of class \code{\linkS4class{VegXMethodDefinition}}).
#' @param obsDates A \code{\link{Date}} or a vector of \code{\link{Date}} objects with plot observation dates. 
#' @param isTaxon A boolean flag (or vector) to indicate which organism names are taxa.
#' @param absence.values A vector of values to be interpreted as missing plant information.
#' @param verbose A flag to indicate console output of the data integration process.
#'
#' @return A modified object of class \code{\linkS4class{VegX}}
#'
#' @family add functions
#'
addTaxonBySiteData <-function(target,
                              x,
                              abundanceMethod,
                              obsDates = Sys.Date(), 
                              isTaxon = TRUE,
                              absence.values = c(NA, 0),
                              verbose = TRUE) {

  if(class(target)!="VegX") stop("Wrong class for 'target'. Should be an object of class 'VegX'")
  
  #plots
  orinplots = length(target@plots)
  nplot = nrow(x)
  plotIDs = character(0)
  plotNames = rownames(x)
  for(i in 1:nplot) {
    npid = .newPlotIDByName(target, plotNames[i]) # Get the new plot ID (internal code)
    plotIDs[i] = npid$id
    if(npid$new) target@plots[[plotIDs[i]]] = list("plotName" = plotNames[i])
  }
  finnplots = length(target@plots)
  if(verbose) {
    cat(paste0(" ", finnplots-orinplots, " new plot(s) added.\n"))
  }

  #plot observations
  if(length(obsDates)==1) obsDates = rep(obsDates, nplot)
  orinpobs = length(target@plotObservations)
  plotObsIDs = character(0)
  for(i in 1:nplot) {
    pObsString = paste(plotIDs[i], obsDates[i]) # plotID+Date
    npoid = .newPlotObsIDByDate(target, plotIDs[i], obsDates[i]) # Get the new plot observation ID (internal code)
    plotObsIDs[i] = npoid$id
    if(npoid$new) {
      target@plotObservations[[plotObsIDs[i]]] = list("plotID" = plotIDs[i],
                                                      "obsStartDate" = obsDates[i])
    }
  }
  finnpobs = length(target@plotObservations)
  if(verbose) {
    cat(paste0(" ", finnpobs-orinpobs, " new plot observation(s) added.\n"))
  }


  # organism identities/organism names
  orinon = length(target@organismNames)
  orinoi = length(target@organismIdentities)
  orgNames = colnames(x)
  noid = length(orgNames)
  if(length(isTaxon)==1) isTaxon = rep(isTaxon, noid)
  orgIDs = character(0)
  for(i in 1:noid) {
    nonid = .newOrganismNameIDByName(target, orgNames[i], isTaxon[i]) # Get the new taxon name usage ID (internal code)
    if(nonid$new) target@organismNames[[nonid$id]] = list("name" = orgNames[i],
                                                         "taxon" = isTaxon[i])
    noiid = .newOrganismIdentityIDByTaxonConcept(target, orgNames[i], "") # Get the new taxon name usage ID (internal code)
    orgIDs[i] = noiid$id
    if(noiid$new) target@organismIdentities[[noiid$id]] = list("originalOrganismNameID" = nonid$id)
  }
  finnon = length(target@organismNames)
  finnoi = length(target@organismIdentities)
  if(verbose) {
    cat(paste0(" ", finnon-orinon, " new organism name(s) added.\n"))
    cat(paste0(" ", finnoi-orinoi, " new organism identity(ies) added.\n"))
  }

  #methods/attributes (WARNING: method match should be made by attributes?)
  if(class(abundanceMethod)=="character") {
    abundanceMethod = predefinedMeasurementMethod(abundanceMethod)
  }
  nmtid = .newMethodIDByName(target,abundanceMethod@name)
  methodID = nmtid$id
  if(nmtid$new) {
    target@methods[[methodID]] = list(name = abundanceMethod@name,
                                      description = abundanceMethod@description,
                                      subject = abundanceMethod@subject,
                                      attributeType = abundanceMethod@attributeType)
    if(verbose) cat(paste0(" Abundance measurement method '", abundanceMethod@name,"' added.\n"))
    # add literature citation if necessary
    if(abundanceMethod@citationString!="") {
      ncitid = .newLiteratureCitationIDByCitationString(target, abundanceMethod@citationString)
      if(ncitid$new) {
        target@literatureCitations[[ncitid$id]] = list(citationString =abundanceMethod@citationString)
        if(abundanceMethod@DOI!="")  target@literatureCitations[[ncitid$id]]$DOI = abundanceMethod@DOI
      }
      target@methods[[methodID]]$citationID = ncitid$id
    }

    # add attributes if necessary
    for(i in 1:length(abundanceMethod@attributes)) {
      attid = .nextAttributeID(target)
      target@attributes[[attid]] = abundanceMethod@attributes[[i]]
      target@attributes[[attid]]$methodID = methodID
    }
    nattr = length(abundanceMethod@attributes)
  }

  if(abundanceMethod@attributeType!= "quantitative") {
    nattr = length(abundanceMethod@attributes)
    codes = character(nattr)
    ids = names(abundanceMethod@attributes)
    for(i in 1:nattr) codes[i] = as.character(abundanceMethod@attributes[[i]]$code)
  }

  # aggregate organism observations
  absence.values = as.character(absence.values)
  orinaoo = length(target@aggregateObservations)
  for(i in 1:nplot) {
    for(j in 1:noid) {
      if(!(as.character(x[i,j]) %in% absence.values)) {
        naoID = .newAggregateOrganismObservationIDByOrganismIdentityID(target, 
                                                                       plotObsIDs[i], 
                                                                       "", orgIDs[j])
        aggObsID = naoID$id
        if(naoID$new) {
          aggObs = list("plotObservationID" = plotObsIDs[i],
                        "organismIdentityID" = orgIDs[j],
                        "stratumObservationID" = "")
        }
        else {
          aggObs = target@aggregateObservations[[aggObsID]]
        }
        if(abundanceMethod@attributeType== "quantitative") {
          attID = "1"
          if(x[i,j]> abundanceMethod@attributes[[1]]$upperLimit) {
            stop(paste0("Value '", x[i,j],"' larger than upper limit of measurement definition. Please revise scale or data."))
          }
          else if(x[i,j] < abundanceMethod@attributes[[1]]$lowerLimit) {
            stop(paste0("Value '", x[i,j],"' smaller than lower limit of measurement definition. Please revise scale or data."))
          }
        } else {
          ind = which(codes==as.character(x[i,j]))
          if(length(ind)==1) attID = ids[ind]
          else stop(paste0("Value '", x[i,j],"' not found in measurement definition. Please revise scale or data."))
        }
        aggObs$aggregateOrganismMeasurements = list()
        aggObs$aggregateOrganismMeasurements[[1]] = list("attributeID" = attID,"value" = x[i,j])
        target@aggregateObservations[[aggObsID]] = aggObs
      }
    }
  }
  finnaoo = length(target@aggregateObservations)
  if(verbose) {
    cat(paste0(" ", finnaoo-orinaoo, " new aggregate organism observation(s) added.\n"))
  }

  return(target)
}
