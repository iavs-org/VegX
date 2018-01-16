#' Add a taxon observation records
#'
#' Adds aggregated taxon observation records to a VegX object from a data table
#' using a mapping to identify columns: plot, observation date, stratum, taxon name and value.
#'
#' @param target the original object of class \code{\linkS4class{VegX}} to be modified
#' @param x a data frame where each row corresponds to one aggregated taxon observation. Columns can be varied.
#' @param projectTitle a character string to identify the project title, which can be the same as one of the currently defined in \code{target}.
#' @param obsMapping a character vector with element names 'plotName', 'obsStartDate', 'stratumName', 'taxonAuthorName' and 'value'.
#' @param abundanceMethod measurement method for aggregated plant abundance (an object of class \code{\linkS4class{VegXMethod}}).
#' @param strMapping
#' @param stratumMethod
#'
#' @return The modified object of class \code{\linkS4class{VegX}}
#' @export
#'
#' @examples
addTaxonObservationRecords<-function(target, x, projectTitle,
                                     obsMapping, abundanceMethod = defaultPercentCoverMethod(),
                                     strMapping = NULL, stratumDefinition = defaultStratumDefinition()) {

  #get project ID and add new project if necessary
  nprid = .newProjectIDByTitle(target,projectTitle)
  projectID = nprid$id
  if(nprid$new) {
    target@projects[[projectID]] = list("title" = projectTitle)
    if(verbose) cat(paste0(" New project '", projectTitle,"' added.\n"))
  } else {
    if(verbose) cat(paste0(" Data will be added to existing project '", projectTitle,"'.\n"))
  }

  #methods/attributes (WARNING: method match should be made by attributes?)
  nmtid = .newMethodIDByName(target,abundanceMethod@name)
  methodID = nmtid$id
  if(nmtid$new) {
    target@methods[[methodID]] = list(name = abundanceMethod@name,
                                      description = abundanceMethod@description,
                                      attributeClass = abundanceMethod@attributeClass,
                                      attributeType = abundanceMethod@attributeType)
    if(verbose) cat(paste0(" Measurement method '", abundanceMethod@name,"' added.\n"))
    # add attributes if necessary
    cnt = length(target@attributes)+1
    for(i in 1:length(abundanceMethod@attributes)) {
      attid = as.character(cnt)
      target@attributes[[attid]] = abundanceMethod@attributes[[i]]
      target@attributes[[attid]]$methodID = methodID
      cnt = cnt + 1
    }
    nattr = length(abundanceMethod@attributes)
  }

  if(abundanceMethod@attributeType!= "quantitative") {
    nattr = length(abundanceMethod@attributes)
    codes = character(nattr)
    ids = names(abundanceMethod@attributes)
    for(i in 1:nattr) codes[i] = as.character(abundanceMethod@attributes[[i]]$code)
  }

  return(target)
}
