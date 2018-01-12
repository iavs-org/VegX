#' Create a Veg-X object from a releve table
#'
#' @param x site-by-species releve table
#' @param method measurement method for aggregated plant abundance
#' @param obsDates a vector of \code{\link{Date}} objects with plot observation dates.
#' @param absence.values a vector of values to be interpreted as missing plant information
#'
#' @return an object of class \code{\linkS4class{VegX}}
#' @export
#'
#' @examples
relevetable2VegX <-function(x, method = defaultPercentCoverMethod(),
                            obsDates = Sys.Date(), absence.values = c(NA, 0)) {
  #plots
  nplot = nrow(x)
  plotIDs = 1:nplot
  plotNames = rownames(x)
  plotVector = vector("list", nplot)
  names(plotVector) = plotIDs
  for(i in 1:nplot) plotVector[[i]] = list("plotName" = plotNames[i])

  #plot observations
  if(length(obsDates)==1) obsDates = rep(obsDates, nplot)
  plotObsIDs = 1:nplot
  plotObsVector = vector("list", nplot)
  names(plotObsVector) = plotObsIDs
  for(i in 1:nplot) plotObsVector[[i]] = list("plotUniqueIdentifier" = plotIDs[i],
                                              "obsStartDate" = obsDates[i])
  #taxa
  ntax = ncol(x)
  taxonIDs = 1:ntax
  taxNames = colnames(x)
  taxonNamesVector = vector("list", ntax)
  names(taxonNamesVector) = taxonIDs
  for(i in 1:ntax) taxonNamesVector[[i]] = list("taxonName" = taxNames[i])

  #aggregated organism observations
  absence.values = as.character(absence.values)
  aggObsID = 1 #counter
  aggObsVector = vector("list",0)
  for(i in 1:nplot) {
    for(j in 1:ntax) {
      if(!(as.character(x[i,j]) %in% absence.values)) {
        aggObsVector[[aggObsID]] = list("plotObservationID" = plotObsIDs[i],
                                        "taxonID" = taxonIDs[j],
                                        "attributeID" = 1,
                                        "value" = x[i,j])
        aggObsID = aggObsID + 1
      }
    }
  }
  #attributes
  attributesVector = vector("list", 1)
  attributesVector[[1]] = list("code" = attribute,
                               "ordinal" = !is.na(attributeScale),
                               "scale" = attributeScale)
  names(attributesVector) = 1


  #other lists
  strataVector = vector("list", 0)
  individualOrgVector = vector("list", 0)
  stratumObsVector = vector("list", 0)
  indOrgObsVector = vector("list", 0)
  return(new("VegX",plots=plotVector,
                    plotObservations = plotObsVector,
                    individualObservations = indOrgObsVector,
                    aggregatedObservations = aggObsVector,
                    stratumObservations = stratumObsVector,
                    strata = strataVector,
                    individualOrganisms = individualOrgVector,
                    taxonNames = taxonNamesVector,
                    attributes = attributesVector))
}
