#' Add tree observation records
#'
#' Adds tree observation records to a VegX object from a data table,
#' using a mapping to identify columns: plot, observation date, taxon name and diameter.
#' Additional mappings can be used to specify a stratum where the tree is located.
#'
#' @param target The initial object of class \code{\linkS4class{VegX}} to be modified
#' @param x A data frame where each row corresponds to one tree observation. Columns can be varied.
#' @param projectTitle A string to identify the project title, which can be the same as one of the currently defined in \code{target}.
#' @param mapping A list with element names 'plotName', 'obsStartDate', 'taxonAuthorName' and 'diameter', used to specify the mapping of data columns (specified using strings for column names) onto these variables.
#'                Additional optional mappings are: 'subPlot', 'obsEndDate', 'individual', 'height' and 'stratumName'.
#' @param diameterMethod
#' @param heightMethod
#' @param stratumDefinition An object of class \code{\linkS4class{VegXStrata}} indicating the definition of strata.
#' @param verbose A boolean flag to indicate console output of the data integration process.
#'
#' @return The modified object of class \code{\linkS4class{VegX}}.
#' @export
#'
#' @examples
addTreeObservationRecords<-function(target, x, projectTitle,
                                    mapping,
                                    diameterMethod,
                                    heightMethod = NULL,
                                    stratumDefinition = NULL,
                                    verbose = TRUE) {
  x = as.data.frame(x)
  nrecords = nrow(x)

  #Check columns exist
  for(i in 1:length(mapping)) {
    if(!(mapping[i] %in% names(x))) stop(paste0("Variable '", mapping[i],"' not found in column names. Revise mapping or data."))
  }
  plotNames = as.character(x[[mapping[["plotName"]]]])
  obsStartDates = as.Date(x[[mapping[["obsStartDate"]]]])
  taxonAuthorNames = as.character(x[[mapping[["taxonAuthorName"]]]])
  diameters = as.character(x[[mapping[["diameter"]]]])

  #Optional mappings
  stratumFlag = ("stratumName" %in% names(mapping))
  if(stratumFlag) {
    stratumNames = as.character(x[[mapping[["stratumName"]]]])
  }
  obsEndFlag = ("obsEndDate" %in% names(mapping))
  if(obsEndFlag) {
    obsEndDates = as.Date(x[[mapping[["obsEndDate"]]]])
  }
  subPlotFlag = ("subPlot" %in% names(mapping))
  if(subPlotFlag) {
    subPlotNames = as.character(x[[mapping[["subPlot"]]]])
  }
  heightFlag = ("height" %in% names(mapping))
  if(stratumFlag) {
    heights = as.character(x[[mapping[["height"]]]])
  }
  individualFlag = ("individual" %in% names(mapping))
  if(stratumFlag) {
    individuals = as.character(x[[mapping[["individual"]]]])
  }
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
  nmtid = .newMethodIDByName(target,diameterMethod@name)
  methodID = nmtid$id
  diameterCodes = character(0)
  diamAttIDs = character(0)
  if(nmtid$new) {
    target@methods[[methodID]] = list(name = diameterMethod@name,
                                      description = diameterMethod@description,
                                      attributeClass = diameterMethod@attributeClass,
                                      attributeType = diameterMethod@attributeType)
    if(verbose) cat(paste0(" Diameter measurement method '", diameterMethod@name,"' added.\n"))
    # add attributes if necessary
    cnt = length(target@attributes)+1
    for(i in 1:length(diameterMethod@attributes)) {
      attid = as.character(cnt)
      target@attributes[[attid]] = diameterMethod@attributes[[i]]
      target@attributes[[attid]]$methodID = methodID
      diamAttIDs[i] = attid
      if(diameterMethod@attributes[[i]]$type != "quantitative") diameterCodes[i] = diameterMethod@attributes[[i]]$code
      cnt = cnt + 1
    }
  } else {
    diameterCodes = .getAttributeCodesByMethodID(methodID)
    diamAttIDs = .getAttributeIDsByMethodID(methodID)
    if(verbose) cat(paste0(" Diameter measurement method '", diameterMethod@name,"' already included.\n"))
  }
}
