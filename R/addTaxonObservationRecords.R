#' Add a taxon observation records
#'
#' Adds aggregated taxon observation records to a VegX object from a data table
#' using a mapping to identify columns: plot, observation date, stratum, taxon name and value.
#'
#' @param target the original object of class \code{\linkS4class{VegX}} to be modified
#' @param x a data frame where each row corresponds to one aggregated taxon observation. Columns can be varied.
#' @param projectTitle A character string to identify the project title, which can be the same as one of the currently defined in \code{target}.
#' @param mapping A list with element names 'plotName', 'obsStartDate', 'taxonAuthorName' and 'value', used to specify the mapping of data columns (specified using strings for column names) onto these variables.
#'                Additional optional mappings are: 'obsEndDate' and 'stratumName'.
#' @param abundanceMethod Measurement method for aggregated plant abundance (an object of class \code{\linkS4class{VegXMethod}}).
#' @param stratumDefinition an object of class \code{\linkS4class{VegXStrata}} indicating the definition of strata.
#' @param verbose flag to indicate console output of the data integration process.
#'
#' @return The modified object of class \code{\linkS4class{VegX}}
#' @export
#'
#' @examples
addTaxonObservationRecords<-function(target, x, projectTitle,
                                     mapping,
                                     abundanceMethod = defaultPercentCoverMethod(),
                                     stratumDefinition = stratumDefinitionByHeight(),
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

  #methods/attributes (WARNING: method match should be made by attributes?)
  nmtid = .newMethodIDByName(target,abundanceMethod@name)
  methodID = nmtid$id
  if(nmtid$new) {
    target@methods[[methodID]] = list(name = abundanceMethod@name,
                                      description = abundanceMethod@description,
                                      attributeClass = abundanceMethod@attributeClass,
                                      attributeType = abundanceMethod@attributeType)
    if(verbose) cat(paste0(" Abundance measurement method '", abundanceMethod@name,"' added.\n"))
    # add attributes if necessary
    cnt = length(target@attributes)+1
    for(i in 1:length(abundanceMethod@attributes)) {
      attid = as.character(cnt)
      target@attributes[[attid]] = abundanceMethod@attributes[[i]]
      target@attributes[[attid]]$methodID = methodID
      cnt = cnt + 1
    }
  }

  if(abundanceMethod@attributeType!= "quantitative") {
    nattr = length(abundanceMethod@attributes)
    codes = character(nattr)
    ids = names(abundanceMethod@attributes)
    for(i in 1:nattr) codes[i] = as.character(abundanceMethod@attributes[[i]]$code)
  }

  # stratum definition
  if(!is.null(stratumDefinition)) {
    # stratum definition method (WARNING: method match should be made by attributes?)
    stratumDefMethod = stratumDefinition@method
    snmtid = .newMethodIDByName(target,stratumDefMethod@name)
    strmethodID = snmtid$id
    if(snmtid$new) {
      target@methods[[strmethodID]] = list(name = stratumDefMethod@name,
                                           description = stratumDefMethod@description,
                                           attributeClass = stratumDefMethod@attributeClass,
                                           attributeType = stratumDefMethod@attributeType)
      if(verbose) cat(paste0(" Stratum definition method '", stratumDefMethod@name,"' added.\n"))
      # add attributes if necessary
      cnt = length(target@attributes)+1
      for(i in 1:length(stratumDefMethod@attributes)) {
        attid = as.character(cnt)
        target@attributes[[attid]] = stratumDefMethod@attributes[[i]]
        target@attributes[[attid]]$methodID = strmethodID
        cnt = cnt + 1
      }
      # add strata (beware of new strata)
      orinstrata = length(target@strata)
      nstr = length(stratumDefinition@strata)
      stratumIDs = character(0)
      stratumNames = character(0)
      cnt = length(target@strata)+1
      for(i in 1:nstr) {
        strid = as.character(cnt)
        stratumIDs[i] = strid
        target@strata[[strid]] = stratumDefinition@strata[[i]]
        target@strata[[strid]]$methodID = strmethodID
        cnt = cnt + 1
      }
      finnstrata = length(target@strata)
      if(verbose) {
        cat(paste0(" ", finnstrata-orinstrata, " new stratum definitions added.\n"))
      }
    }
  }

  return(target)
}
