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
    if(is.null(stratumDefinition)) stop("Stratum definition must be supplied to map stratum observations.\n  Revise mapping or provide a stratum definition.")
  } else {
    if(!is.null(stratumDefinition)) stop("You need to include a mapping for 'stratumName' in order to map stratum observations.")
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
    if(is.null(heightMethod)) stop("Height method must be supplied to map individual heights. Revise mapping or provide a method.")
  } else {
    if(!is.null(heightMethod)) stop("You need to include a mapping for 'height' in order to map individual heights.")
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

  if(heightFlag) {
    nmtid = .newMethodIDByName(target,heightMethod@name)
    methodID = nmtid$id
    heightCodes = character(0)
    heightAttIDs = character(0)
    if(nmtid$new) {
      target@methods[[methodID]] = list(name = heightMethod@name,
                                        description = heightMethod@description,
                                        attributeClass = heightMethod@attributeClass,
                                        attributeType = heightMethod@attributeType)
      if(verbose) cat(paste0(" Diameter measurement method '", heightMethod@name,"' added.\n"))
      # add attributes if necessary
      cnt = length(target@attributes)+1
      for(i in 1:length(heightMethod@attributes)) {
        attid = as.character(cnt)
        target@attributes[[attid]] = heightMethod@attributes[[i]]
        target@attributes[[attid]]$methodID = methodID
        heightAttIDs[i] = attid
        if(heightMethod@attributes[[i]]$type != "quantitative") heightCodes[i] = heightMethod@attributes[[i]]$code
        cnt = cnt + 1
      }
    } else {
      heightCodes = .getAttributeCodesByMethodID(methodID)
      heightAttIDs = .getAttributeIDsByMethodID(methodID)
      if(verbose) cat(paste0(" Diameter measurement method '", heightMethod@name,"' already included.\n"))
    }

  }

  # stratum definition
  if(stratumFlag) {
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
    } else { #Read stratum IDs and stratum names from selected method
      stratumIDs = .getStratumIDsByMethodID(strmethodID)
      if(verbose) cat(paste0(" Stratum definition '", stratumDefMethod@name,"' already included.\n"))
    }
  }
  return(target)
}
