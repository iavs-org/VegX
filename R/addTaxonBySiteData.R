#' Add a taxon-by-site table
#'
#' Adds the aggregate organism observations of a taxon-by-site table to an existing VegX object.
#' Data can be added to an existing project or a new project can be defined within the VegX object,
#' depending on the input project title.
#' Vegetation plots and taxon names can be the same as those already existing in the target VegX.
#' Subplots cannot be defined with this function, but observations can be referred to them if they exist.
#' New/old plot observations are distinguished by observation date.
#'
#' @param target The object of class \code{\linkS4class{VegX}} to be modified
#' @param x A site-by-species releve table
#' @param projectTitle A character string to identify the project title, which can be the same as one of the currently defined in \code{target}.
#' @param abundanceMethod A measurement method for aggregate plant abundance (an object of class \code{\linkS4class{VegXMethod}}).
#' @param obsDates A vector of \code{\link{Date}} objects with plot observation dates.
#' @param absence.values A vector of values to be interpreted as missing plant information.
#' @param verbose A flag to indicate console output of the data integration process.
#'
#' @return A modified object of class \code{\linkS4class{VegX}}
#' @export
#'
#' @family add functions
#'
#' @examples
addTaxonBySiteData <-function(target,
                              x,
                              projectTitle,
                              abundanceMethod,
                              obsDates = Sys.Date(), absence.values = c(NA, 0),
                              verbose = TRUE) {

  #get project ID and add new project if necessary
  nprid = .newProjectIDByTitle(target,projectTitle)
  projectID = nprid$id
  if(nprid$new) {
    target@projects[[projectID]] = list("title" = projectTitle)
    if(verbose) cat(paste0(" New project '", projectTitle,"' added.\n"))
  } else {
    if(verbose) cat(paste0(" Data will be added to existing project '", projectTitle,"'.\n"))
  }

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
    cat(paste0(" ", finnplots-orinplots, " new plots added.\n"))
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
                                                      "projectID" = projectID,
                                                      "obsStartDate" = obsDates[i])
    }
  }
  finnpobs = length(target@plotObservations)
  if(verbose) {
    cat(paste0(" ", finnpobs-orinpobs, " new plot observations added.\n"))
  }


  # #taxon name usage concepts
  orintuc = length(target@taxonNameUsageConcepts)
  tnucNames = colnames(x)
  ntnuc = length(tnucNames)
  tnucIDs = character(0)
  for(i in 1:ntnuc) {
    ntnucid = .newTaxonNameUsageConceptIDByName(target, tnucNames[i]) # Get the new taxon name usage ID (internal code)
    tnucIDs[i] = ntnucid$id
    if(ntnucid$new) target@taxonNameUsageConcepts[[tnucIDs[i]]] = list("authorTaxonName" = tnucNames[i])
  }
  finntuc = length(target@taxonNameUsageConcepts)
  if(verbose) {
    cat(paste0(" ", finntuc-orintuc, " new taxon name usage concepts added.\n"))
  }

  #methods/attributes (WARNING: method match should be made by attributes?)
  nmtid = .newMethodIDByName(target,abundanceMethod@name)
  methodID = nmtid$id
  if(nmtid$new) {
    target@methods[[methodID]] = list(name = abundanceMethod@name,
                                      description = abundanceMethod@description,
                                      subject = abundanceMethod@subject,
                                      attributeType = abundanceMethod@attributeType)
    if(verbose) cat(paste0(" Abundance measurement method '", abundanceMethod@name,"' added.\n"))
    # add literature citation if necessary
    if(method@citationString!="") {
      ncitid = .newLiteratureCitationIDByCitationString(target, method@citationString)
      if(ncitid$new) {
        target@literatureCitations[[ncitid$id]] = list(citationString =method@citationString)
        if(method@DOI!="")  target@literatureCitations[[ncitid$id]]$DOI = method@DOI
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
  aggObsCounter = orinaoo+1 #counter
  for(i in 1:nplot) {
    for(j in 1:ntnuc) {
      if(!(as.character(x[i,j]) %in% absence.values)) {
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
        target@aggregateObservations[[as.character(aggObsCounter)]] = list("plotObservationID" = plotObsIDs[i],
                                        "taxonNameUsageConceptID" = tnucIDs[j],
                                        "stratumObservationID" = "",
                                        "attributeID" = attID,
                                        "value" = x[i,j])
        aggObsCounter = aggObsCounter + 1
      }
    }
  }
  finnaoo = length(target@aggregateObservations)
  if(verbose) {
    cat(paste0(" ", finnaoo-orinaoo, " new aggregate organism observations added.\n"))
  }

  return(target)
}
