#' Create a Veg-X object from a releve table
#'
#' @param projectTitle a character string to identify the project title
#' @param x site-by-species releve table
#' @param method measurement method for aggregated plant abundance
#' @param obsDates a vector of \code{\link{Date}} objects with plot observation dates.
#' @param absence.values a vector of values to be interpreted as missing plant information
#'
#' @return an object of class \code{\linkS4class{VegX}}
#' @export
#'
#' @examples
relevetable2VegX <-function(projectTitle,
                            x,
                            method = defaultPercentCoverMethod(),
                            obsDates = Sys.Date(), absence.values = c(NA, 0)) {

  #project
  projVector = vector("list", 1)
  names(projVector) = "1"
  projVector[[1]] = list("title" = projectTitle)
  #plots
  nplot = nrow(x)
  plotIDs = as.character(1:nplot)
  plotNames = rownames(x)
  plotVector = vector("list", nplot)
  names(plotVector) = plotIDs
  for(i in 1:nplot) plotVector[[i]] = list("plotName" = plotNames[i])

  #plot observations
  if(length(obsDates)==1) obsDates = rep(obsDates, nplot)
  plotObsIDs = as.character(1:nplot)
  plotObsVector = vector("list", nplot)
  names(plotObsVector) = plotObsIDs
  for(i in 1:nplot) plotObsVector[[i]] = list("plotID" = plotIDs[i],
                                              "obsStartDate" = obsDates[i],
                                              "projectID" = "1")
  #taxon name usage concepts
  ntnuc = ncol(x)
  tnucIDs = as.character(1:ntnuc)
  tnucNames = colnames(x)
  tnucNamesVector = vector("list", ntnuc)
  names(tnucNamesVector) = tnucIDs
  for(i in 1:ntnuc) tnucNamesVector[[i]] = list("authorName" = tnucNames[i])

  #methods
  methodsVector = vector("list", 1)
  names(methodsVector) = "1"
  methodsVector[[1]] = list(name = method@name,
                            description = method@description,
                            attributeClass = method@attributeClass,
                            attributeType = method@attributeType)

  #attributes
  attributesVector = method@attributes
  nattr = length(attributesVector)
  for(i in 1:nattr) attributesVector[[i]]$methodID = "1"
  if(method@attributeType!= "quantitative") {
    codes = character(nattr)
    ids = names(attributesVector)
    for(i in 1:nattr) codes[i] = as.character(attributesVector[[i]]$code)
  }

  #aggregated organism observations
  absence.values = as.character(absence.values)
  aggObsCounter = 1 #counter
  aggObsVector = vector("list",0)
  for(i in 1:nplot) {
    for(j in 1:ntnuc) {
      if(!(as.character(x[i,j]) %in% absence.values)) {
        if(method@attributeType== "quantitative") {
          attID = "1"
          if(x[i,j]> method@attributes[[1]]$upperBound) {
            stop(paste0("Value '", x[i,j],"' larger than upper bound of measurement definition. Please revise scale or data."))
          }
          else if(x[i,j] < method@attributes[[1]]$lowerBound) {
            stop(paste0("Value '", x[i,j],"' smaller than lower bound of measurement definition. Please revise scale or data."))
          }
        } else {
          ind = which(codes==as.character(x[i,j]))
          if(length(ind)==1) attID = ids[ind]
          else stop(paste0("Value '", x[i,j],"' not found in measurement definition. Please revise scale or data."))
        }
        aggObsVector[[aggObsCounter]] = list("plotObservationID" = plotObsIDs[i],
                                        "taxonNameUsageConceptID" = tnucIDs[j],
                                        "attributeID" = attID,
                                        "value" = x[i,j])
        aggObsCounter = aggObsCounter + 1
      }
    }
  }


  #other lists
  strataVector = vector("list", 0)
  individualOrgVector = vector("list", 0)
  stratumObsVector = vector("list", 0)
  indOrgObsVector = vector("list", 0)
  return(new("VegX",
             projects = projVector,
             plots=plotVector,
             plotObservations = plotObsVector,
             taxonNameUsageConcepts = tnucNamesVector,
             individualObservations = indOrgObsVector,
             aggregatedObservations = aggObsVector,
             stratumObservations = stratumObsVector,
             strata = strataVector,
             individualOrganisms = individualOrgVector,
             methods = methodsVector,
             attributes = attributesVector))
}
